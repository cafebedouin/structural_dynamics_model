% ============================================================================
% CONSTRAINT STORY: sotu_1945_truman_war_crimes_prosecution_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1945_truman_war_crimes_prosecution_mandate, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1945_truman_war_crimes_prosecution_mandate
 *   human_readable: Post-War War Crimes Prosecution and Accountability Mandate (Truman, 1945)
 *   domain: governance/international_law/post_conflict_justice
 *
 * SUMMARY:
 *   The war crimes prosecution mandate announced by President Truman in 1945
 *   established a binding international commitment to pursue Axis leadership
 *   for violations of international law and norms. This constraint created
 *   the Nuremberg and Tokyo tribunals, along with occupation-based
 *   prosecution frameworks that operated from 1945 through the early 1950s.
 *   The mandate exhibits characteristics of both genuine post-conflict
 *   coordination and asymmetric extraction: it provides a mechanism for
 *   channeling retribution through law rather than revenge violence
 *   (coordination benefit), but simultaneously functions as a vehicle for
 *   occupation consolidation, territorial control, and victor's justice
 *   (extraction mechanism). The constraint's theater ratio increases over
 *   time as the gap widens between universalist rhetoric ('international
 *   accountability for all war crimes') and selective application (victors
 *   prosecuted minimally, if at all). The mandate benefits victorious allied
 *   powers by establishing legal precedent for future geopolitical dominance,
 *   benefits future victims of aggression by creating an accountability
 *   framework, but extracts substantial costs from defeated states through
 *   occupation, forced judicial processes, and imposed justice mechanisms.
 *   The constraint demonstrates how justice frameworks can simultaneously
 *   coordinate legitimate accountability and extract political/territorial
 *   advantages.
 *
 * KEY AGENTS:
 *   - Truman Administration and Allied Powers: Primary beneficiary (institutional/arbitrage) — establishes accountability precedent that legitimizes post-war dominance and prevents future alliance breakdown
 *   - Axis Leadership (Captured): Primary victim (powerless/trapped) — faces tribunal prosecution they cannot evade, backed by military enforcement; subject to comprehensive accountability mechanisms
 *   - Occupied Territories and Civilian Populations: Secondary victim (powerless/trapped) — experience prolonged military occupation justified as necessary for accountability enforcement; extraction through occupation costs and sovereignty suppression
 *   - Occupation Authorities: Mixed role (institutional/constrained) — face genuine coordination problem of preventing revenge violence, but also extract through enforcement authority and territorial consolidation
 *   - International Legal Institutions: Observer-beneficiary (institutional/arbitrage) — benefits from precedent establishment; later becomes Piton as institution persists through inertia despite systematic failures to prevent subsequent atrocities
 *   - Future Victims of Aggression: Abstract beneficiary (powerful/arbitrage) — benefit from accountability precedent, though empirical impact on deterrence is contested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1945_truman_war_crimes_prosecution_mandate, 0.58).
domain_priors:suppression_score(sotu_1945_truman_war_crimes_prosecution_mandate, 0.72).
domain_priors:theater_ratio(sotu_1945_truman_war_crimes_prosecution_mandate, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1945_truman_war_crimes_prosecution_mandate, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1945_truman_war_crimes_prosecution_mandate, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_1945_truman_war_crimes_prosecution_mandate, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1945_truman_war_crimes_prosecution_mandate, tangled_rope).
narrative_ontology:human_readable(sotu_1945_truman_war_crimes_prosecution_mandate, "Post-War War Crimes Prosecution and Accountability Mandate (Truman, 1945)").
narrative_ontology:topic_domain(sotu_1945_truman_war_crimes_prosecution_mandate, "governance/international_law/post_conflict_justice").

domain_priors:requires_active_enforcement(sotu_1945_truman_war_crimes_prosecution_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1945_truman_war_crimes_prosecution_mandate, victorious_allied_powers).
narrative_ontology:constraint_beneficiary(sotu_1945_truman_war_crimes_prosecution_mandate, future_victims_of_aggression).
narrative_ontology:constraint_beneficiary(sotu_1945_truman_war_crimes_prosecution_mandate, international_legal_precedent).
narrative_ontology:constraint_victim(sotu_1945_truman_war_crimes_prosecution_mandate, defeated_axis_states).
narrative_ontology:constraint_victim(sotu_1945_truman_war_crimes_prosecution_mandate, axis_leadership_families).
narrative_ontology:constraint_victim(sotu_1945_truman_war_crimes_prosecution_mandate, occupied_territories).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPTURED AXIS LEADERSHIP (SNARE) — Physically trapped by military occupation and Allied custody. No exit option exists. The prosecution mandate operates as pure extraction: the leadership faces tribunal procedures they cannot evade or influence, backed by military enforcement. No coordination function benefits them. Maximum suppression through incarceration and denial of movement.
constraint_indexing:constraint_classification(sotu_1945_truman_war_crimes_prosecution_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OCCUPIED TERRITORIES AND CIVILIAN POPULATIONS (SNARE) — Face prolonged military occupation justified as necessary for accountability enforcement. Extraction occurs through occupation costs, military governance, and suppression of local sovereignty. The prosecution mechanism does not coordinate shared interests — it imposes external justice frameworks. High suppression through occupation and absent consent for tribunal authority.
constraint_indexing:constraint_classification(sotu_1945_truman_war_crimes_prosecution_mandate, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ALLIED OCCUPATION AUTHORITIES (TANGLED ROPE) — Face genuine coordination problem: how to establish legitimate post-war order without descending into revenge violence? The prosecution mandate provides coordination function — it channels retribution through law rather than mob justice, legitimizes occupation through judicial process, and establishes rules for conduct. But enforcement mechanisms also enable extraction: occupation authorities use prosecution authority to consolidate control, extract reparations, and reshape defeated territories. Significant enforcement required; genuine coordination benefit alongside asymmetric extraction.
constraint_indexing:constraint_classification(sotu_1945_truman_war_crimes_prosecution_mandate, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: VICTORIOUS ALLIED POWERS / STRATEGIC VIEW (ROPE) — Coordinate through prosecution mandate to prevent future alliance breakdown and establish rules for inter-state behavior. The prosecution mechanism benefits all victors equally — it creates precedent for future accountability and establishes international legal framework preventing ad-hoc revenge. Exit option is arbitrage: if dissatisfied with tribunal outcomes, any victor can withdraw (as happens with Soviet/Western divergence post-1947). Perceived as primarily coordination mechanism rather than extraction vehicle.
constraint_indexing:constraint_classification(sotu_1945_truman_war_crimes_prosecution_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL PRECEDENT / FUTURE ACCOUNTABILITY (TANGLED ROPE) — The prosecution mandate creates genuine coordination benefit: establishes precedent that aggression and war crimes carry legal consequences, protecting future victims of aggression. But the mechanism also extracts through selective application — victors are rarely prosecuted, defeated states face comprehensive accountability, and the framework's universality is performative (applies selectively based on geopolitical power). Both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(sotu_1945_truman_war_crimes_prosecution_mandate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL INSTITUTIONS / INSTITUTIONAL VIEW (PITON) — From institutional perspective, the prosecution mandate degrades into performative accountability theater: Nuremberg and Tokyo trials are celebrated as watershed moments in international law, but the underlying mechanisms fail to prevent subsequent atrocities (Korea, Vietnam, Cold War proxy conflicts, Rwanda, Yugoslavia). The institutional commitment to universal accountability persists through inertia despite systematic failures. Theater ratio rises as the gap between universalist rhetoric and selective application widens over decades.
constraint_indexing:constraint_classification(sotu_1945_truman_war_crimes_prosecution_mandate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the prosecution mandate appears to reflect an immutable principle: victors always establish accountability frameworks for defeated enemies; this is how power operates in the aftermath of total war. The constraint appears as a natural law of geopolitics — an inescapable feature of how dominant powers consolidate post-conflict order. However, the structural data reveals this as false summit: the 'naturalness' of victor's justice is constructed and benefits specific agents (victorious powers, international legal establishments).
constraint_indexing:constraint_classification(sotu_1945_truman_war_crimes_prosecution_mandate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1945_truman_war_crimes_prosecution_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1945_truman_war_crimes_prosecution_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1945_truman_war_crimes_prosecution_mandate, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1945_truman_war_crimes_prosecution_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1945_truman_war_crimes_prosecution_mandate, TR),
    TR >= 0.70.

:- end_tests(sotu_1945_truman_war_crimes_prosecution_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The prosecution mandate extracts significantly from defeated states through occupation, forced judicial processes, territorial reparations, and resource extraction justified as accountability enforcement. However, it is not as severe as pure extraction because: (1) genuine coordination function exists (channeling retribution through law prevents worse outcomes), (2) some legitimate accountability benefits exist (war crimes were committed), and (3) the mechanism has some transparency and rules (tribunals are semi-public, verdicts are documented). The extractiveness rises from 0.42 at interval start (immediate post-war period, when justice motivation is strong and occupation seems temporary) to 0.62 by interval end (as extraction mechanisms crystallize into long-term occupation and the temporary becomes permanent). Suppression (0.72): High. Military occupation backed by Allied forces suppresses all exit options for defeated populations and leadership. Prosecution authority blocks legal challenge, occupying forces block physical exit, tribunal procedures deny input to defendants. Suppression is justified as necessary for accountability, but operates without meaningful consent from affected populations. Theater ratio (0.48, rising to 0.56): Moderate and rising. Initial theater is low (trials are earnest accountability efforts, rhetoric matches function) but rises over time as the gap widens between universal justice rhetoric and selective prosecution (victors rarely prosecuted, defeated prosecuted comprehensively).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural mechanism (prosecution mandate) generates radically different classifications from different perspectives. The gap is driven by three factors: (1) power differential — powerless defendants see snare, institutional victors see rope; (2) time horizon — immediate view shows legitimate accountability, civilizational view shows degraded institution (piton); (3) exit options — trapped populations see extraction, arbitrage-enabled powers see coordination. The mandate's rhetoric is universalist ('accountability for all'), but its structure is asymmetric (victors exempt, defeated prosecuted). This gap between rhetoric and structure is the core diagnostic: the constraint derives legitimacy from appearing to coordinate justice, but structures extraction through selective application.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain flows from beneficiary/victim declarations and exit options. Axis leadership is declared victim + trapped → d=0.95 (maximum target). Occupation authorities are declared both beneficiary (extraction from territories) and victim (constrained by legitimacy requirements) → d=0.55. Allied powers are declared beneficiary + arbitrage → d=0.15 (low extraction experienced). Occupied populations are declared victim + trapped → d=0.88 (high extraction experienced). The sigmoid f(d) then maps these d values to effective extraction multipliers: f(0.95)≈1.42 (maximum), f(0.55)≈0.75 (moderate), f(0.15)≈-0.01 (near-zero to negative), f(0.88)≈1.28 (very high). Scope modifier σ(S) at global scale applies 1.2 amplification. These computations are automatic in the engine; the authoring task is declaring the structural data accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not resolve mandatrophy cleanly. Extractiveness (0.58) places it above the Rope threshold (χ≤0.35) and below pure Snare (χ≥0.66), landing in Tangled Rope zone where both coordination and extraction are present. The claimed type (tangled_rope) matches this placement. The mandate genuinely coordinates: (1) prevents uncontrolled revenge violence by channeling retribution through law, (2) establishes rules for post-conflict order that benefit all victors, (3) creates accountability precedent protecting future victims. But it simultaneously extracts: (1) through occupation imposing external governance, (2) through reparations and resource extraction, (3) through selective application benefiting victors. Mandatrophy resolution requires accepting that coordination and extraction can coexist in the same mechanism, that the proportion is perspectival (Rope from allied power view, Snare from defeated population view), and that this mixture is why the constraint persists despite failures to prevent subsequent atrocities. The piton classification at institutional long-term view indicates the mechanism's functions have degraded — tribunals are celebrated as precedent but fail to constrain subsequent atrocities — yet the commitment to accountability persists through institutional inertia. This is not a failure of mandatrophy resolution but an accurate description of a constraint that has become performative without ceasing to extract.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victor_vs_victim_accountability_asymmetry,
    'Is the prosecution mandate structurally asymmetric (victors exempt, defeated prosecuted), or does it establish genuinely universal accountability mechanisms?',
    'Historical analysis of post-WWII prosecution patterns; examination of whether Allied war crimes allegations receive equal tribunal scrutiny as Axis crimes; post-Cold War verification through ICC prosecution patterns across North/South and developed/developing power asymmetries',
    'If genuinely universal: constraint classifies as Rope from powerful perspectives (coordination benefit). If systematically asymmetric: constraint is Snare with false legitimation rhetoric (extraction disguised as justice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victor_vs_victim_accountability_asymmetry, empirical, 'Whether prosecution mandate applies symmetrically to victors and defeated').

omega_variable(
    occupation_duration_and_sovereignty_extraction,
    'How much of the prosecution mandate''s suppression mechanism derives from justified occupation needs versus extraction of territorial control and resources from defeated states?',
    'Comparative analysis of occupation duration justified by trial timelines versus actual judicial processes; examination of reparations, resource extraction, and territorial changes imposed during prosecution period; correlation between prosecution completion and occupation end dates',
    'If occupation aligns with trial necessity: suppression is justified enforcement cost. If occupation extends significantly beyond trial duration: prosecution mandate is vehicle for territorial/resource extraction disguised as accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupation_duration_and_sovereignty_extraction, empirical, 'Relationship between prosecution timeline and occupation extraction').

omega_variable(
    precedent_effectiveness_for_future_accountability,
    'Does establishing the Nuremberg/Tokyo precedent actually constrain future powerful actors from committing atrocities, or does it function primarily as legitimating theater for victor''s justice while subsequent atrocities proceed unchecked?',
    'Post-1945 historical record: comparison of atrocity rates/severity before and after precedent establishment; analysis of whether subsequent great-power actors modify behavior due to accountability fear; examination of prosecution patterns for subsequent conflicts (Korea, Vietnam, Cold War, Rwanda, Yugoslavia, Iraq)',
    'If effective: constraint is genuine coordination mechanism (Rope). If ineffective: constraint is Piton (degraded, performative institution maintained through inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_effectiveness_for_future_accountability, empirical, 'Whether accountability precedent constrains future atrocities').

omega_variable(
    local_victim_participation_versus_imposed_justice,
    'To what degree does the prosecution mandate coordinate justice preferences of local victims in defeated territories versus imposing external justice frameworks chosen by occupation authorities?',
    'Analysis of local input into tribunal design and witness protocols; comparison between locally-preferred justice outcomes and tribunal verdicts; examination of victim satisfaction and legitimacy perception across occupied territories during and after trials',
    'If locally coordinated: suppression is justified enforcement of legitimate shared framework. If imposed: suppression mechanisms are extraction disguised as accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_victim_participation_versus_imposed_justice, empirical, 'Local participation in versus external imposition of justice mechanisms').

omega_variable(
    false_summit_naturalization_risk,
    'Is the prosecution mandate a natural law of post-conflict governance, or a contingent institutional arrangement that benefits specific agents (victorious powers, legal establishments)?',
    'Comparative historical analysis of post-conflict justice mechanisms across different geopolitical contexts; examination of alternative models that priorited reconciliation, local accountability, or transitional justice over victor''s tribunals; analysis of whether defeat powers had agency in choosing justice mechanisms',
    'If natural law: mountain classification is correct; the mechanism is unchangeable. If contingent: mountain classification is false summit; beneficiary presence and selective application indicate tangled rope or snare with naturalization rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization_risk, conceptual, 'Whether prosecution mandate reflects natural law or constructed institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1945_truman_war_crimes_prosecution_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(warcrime_tr_t0, sotu_1945_truman_war_crimes_prosecution_mandate, theater_ratio, 0, 0.35).
narrative_ontology:measurement(warcrime_tr_t3, sotu_1945_truman_war_crimes_prosecution_mandate, theater_ratio, 3, 0.48).
narrative_ontology:measurement(warcrime_tr_t7, sotu_1945_truman_war_crimes_prosecution_mandate, theater_ratio, 7, 0.56).

% Extraction over time
narrative_ontology:measurement(warcrime_be_t0, sotu_1945_truman_war_crimes_prosecution_mandate, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(warcrime_be_t3, sotu_1945_truman_war_crimes_prosecution_mandate, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(warcrime_be_t7, sotu_1945_truman_war_crimes_prosecution_mandate, base_extractiveness, 7, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1945_truman_war_crimes_prosecution_mandate, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1945_truman_war_crimes_prosecution_mandate, nuremberg_tribunal_legitimacy).
narrative_ontology:affects_constraint(sotu_1945_truman_war_crimes_prosecution_mandate, tokyo_tribunal_victor_justice).
narrative_ontology:affects_constraint(sotu_1945_truman_war_crimes_prosecution_mandate, occupation_authority_consolidation).
narrative_ontology:affects_constraint(sotu_1945_truman_war_crimes_prosecution_mandate, post_war_reparations_extraction).

% DUAL FORMULATION NOTE:
% The war crimes prosecution mandate is upstream of specific tribunal implementations (Nuremberg, Tokyo) which have higher extractiveness values reflecting the selective application of justice. The mandate is also linked to occupation authority dynamics, which show how accountability authority becomes a vehicle for territorial consolidation. These downstream constraints have higher theater ratios as the gap widens between accountability rhetoric and actual justice application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1945_truman_war_crimes_prosecution_mandate, powerful, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
