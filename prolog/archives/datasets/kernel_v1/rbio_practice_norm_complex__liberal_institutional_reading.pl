% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Practice Norm Complex (Liberal Institutional Reading)
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   The RBIO (Responsibility to Protect, International Humanitarian Law, and
 *   Peacekeeping Operations) norm complex represents one of the most
 *   contested frameworks in contemporary international relations. This
 *   constraint story instantiates the LIBERAL INSTITUTIONAL READING — the
 *   interpretation that RBIO norms are genuinely universal (applicable to all
 *   states equally), consent-based (norms emerge from multilateral
 *   negotiation and state acceptance), and revisable through legitimate
 *   processes (weak states retain voice through UN General Assembly and
 *   treaty processes). Under this reading, enforcement selectivity is framed
 *   as a capacity problem, not a legitimacy problem: the international
 *   community lacks resources and political will to enforce RBIO
 *   consistently, but the norms themselves are impartial. This reading
 *   opposes two sibling readings: the HEGEMONIC EXTRACTION reading (RBIO
 *   masks asymmetric extraction benefiting powerful states and their
 *   contractors) and the SOVEREIGNTY MAXIMALIST reading (RBIO violates
 *   sovereign equality and should be replaced with strict non-intervention).
 *   The liberal reading produces a tangled rope constraint: genuine
 *   coordination functions exist (conflict prevention, predictable interstate
 *   behavior, humanitarian accountability) alongside asymmetric extraction
 *   (enforcement selectivity favors powerful states, conditionality imposes
 *   costs on weak states, intervention normalizes military solutions).
 *   Theater has increased over the measurement interval as the gap between
 *   authorized RBIO action and on-ground implementation has widened;
 *   institutions perform legitimacy while capacity stagnates.
 *
 * KEY AGENTS:
 *   - Intervening States (Permanent UNSC Members): Primary beneficiary (institutional/arbitrage) — veto power, alliance control, contractor access; experience framework as coordination with exit option
 *   - Targeted States / Civilian Populations: Primary victim (powerless/trapped) — subject to sanctions, intervention, or political pressure with no exit; bear full extraction cost
 *   - Regional Middle Powers / Non-Aligned States: Secondary victim (moderate/constrained) — constrained by threat of isolation or sanctions if they oppose UNSC action; benefit from RBIO conflict prevention but lose voice
 *   - Multilateral Institutions (UN, humanitarian orgs): Institutional enforcer (organized/constrained) — granted legitimacy by RBIO framework but lack enforcement capacity; caught between member state constraints and humanitarian mandate
 *   - International Legal Doctrine / Academic Community: Institutional interpreter (institutional/arbitrage) — maintains liberal institutional consensus through doctrinal work; benefits from authority to interpret RBIO; increasingly detached from on-ground enforcement reality
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing RBIO as inevitable feature of anarchic international system rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.52).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.48).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Practice Norm Complex (Liberal Institutional Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, 'c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4').
narrative_ontology:cs_kernel_codification('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', distributed).
narrative_ontology:cs_authority_grounding('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', extraction).
narrative_ontology:cs_interpretation_layer_present('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4').
narrative_ontology:cs_reading_relation('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', rbio_practice_norm_complex__sovereignty_maximalist_reading, influences).
narrative_ontology:cs_axiom('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', foundational, multilateral_process_legitimacy).
narrative_ontology:cs_axiom_status(multilateral_process_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', multilateral_process_legitimacy, deontological).
narrative_ontology:cs_axiom('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', foundational, consent_through_sovereign_participation).
narrative_ontology:cs_axiom_status(consent_through_sovereign_participation, holdable).
narrative_ontology:cs_axiom_grounding('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', consent_through_sovereign_participation, deontological).
narrative_ontology:cs_axiom('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', foundational, capacity_versus_will_distinction).
narrative_ontology:cs_axiom_status(capacity_versus_will_distinction, holdable).
narrative_ontology:cs_axiom_grounding('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', capacity_versus_will_distinction, empirically_contingent).
narrative_ontology:cs_created_at('c8dc8c5c-724f-4ce5-8f23-5ea2a08913c4', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, international_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institutions).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, norm_violators_without_capacity_to_resist).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED STATE / CIVILIAN POPULATION (SNARE) — No exit from UNSC-authorized sanctions or intervention; bears full extraction cost through economic collapse, infrastructure destruction, or political subjugation. Suppression operates through military force or economic coercion; civilians cannot organize effective resistance or exit. The liberal framing that UNSC authorization renders intervention 'legitimate' provides no material remedy for the trapped agent.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__liberal_institutional_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL MIDDLE POWER / NON-ALIGNED STATE (TANGLED ROPE) — Constrained by threat of sanctions or isolation if it opposes UNSC action or refuses to adopt RBIO norms. But also benefits from RBIO framework through conflict prevention mechanisms (diplomatic channels, multilateral dispute resolution) and access to international markets conditioned on RBIO compliance. Experiences both extraction (forced norm adoption) and coordination (predictable interstate behavior). Exit possible but costly — requires defection from multilateral system with high economic penalties.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERVENING STATE / PERMANENT UNSC MEMBER (ROPE) — Net beneficiary through veto power, alliance formation, and contractor access. Experiences RBIO as coordination mechanism: unilateral military action is constrained in exchange for legitimacy and coalition support. The veto ensures exit option (can block collective action). Experiences the framework as pure coordination because extraction flows toward this agent.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__liberal_institutional_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTILATERAL INSTITUTION / INTERNATIONAL BUREAUCRACY (TANGLED ROPE) — Both enforcer and victim. Benefits from legitimacy granted by RBIO framing (UNSC authorization, legal process) but constrained by dependence on member state compliance and funding. Theater ratio rises as institutions perform legitimacy while lacking enforcement capacity — the gap between authorized intervention and on-ground effectiveness is managed through bureaucratic theater. Experiences extraction from powerful states that dictate compliance terms; experiences coordination from peer multilateral bodies.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL DOCTRINE / ACADEMIC CONSENSUS (PITON) — The liberal institutional reading of RBIO has become institutionalized doctrine despite degraded functional authority. Legal scholars, policy analysts, and institutional actors rehearse RBIO legitimacy narratives (authorization, consent, multilateral process) while on-ground enforcement is selective, conditionality is asymmetric, and exit is constrained. The theatrical performance of 'legitimate process' persists through academic and institutional inertia even as its predictive power declines.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__liberal_institutional_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational universal perspective, some hierarchy in international relations is inherent to anarchy: absent a world government, power asymmetries will always structure outcomes. RBIO is framed as an inevitable accommodation to the anarchic condition — selective enforcement reflects capacity constraints, not legitimacy problems. This perspective naturalizes the framework as a law of international politics. However, the structural data (identifiable beneficiaries, asymmetric extraction, enforcement selectivity correlated with beneficiary interests) reveals this as a false summit.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__liberal_institutional_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rbio_practice_norm_complex__liberal_institutional_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rbio_practice_norm_complex__liberal_institutional_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, TR),
    TR >= 0.70.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The liberal reading posits genuine coordination benefits (humanitarian accountability, conflict prevention) alongside extraction from weak states (enforcement selectivity, conditionality, forced norm adoption). The 0.52 value reflects this hybrid: genuine coordination functions justify ≥ 0.30 baseline; asymmetric enforcement and conditionality add ≥ 0.22 additional extraction. Suppression (0.48): Moderate. Weak states face coercive pressure (threat of sanctions or military action) if they refuse UNSC authorization or resist conditionality, but not total immobilization — some voice options exist through General Assembly, NGO coalitions, and norm reinterpretation. Theater ratio (0.61): Moderate-high. The gap between authorized RBIO action and effective implementation has grown over the measurement interval. Institutions perform 'legitimate process' (UNSC authorization, due process, humanitarian framing) while on-ground enforcement remains selective and outcomes are asymmetric. Theater has increased from 0.48 (early RBIO era, when legitimacy claims seemed to track outcomes) to 0.61 (contemporary period, when the gap is visible and acknowledged even by liberal scholars). The rising trajectory reflects accumulating evidence that the framework's legitimacy theater is not proportional to functional effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival gap inherent to contested international norms. The intervening state sees coordination (rope perspective) — the framework enables coalition-building and legitimates military action through process. The targeted state sees pure extraction (snare perspective) — no voice, no exit, no benefit, only costs. The regional middle power sees mixed coordination and extraction (tangled rope) — benefits from conflict prevention but constrained by threat of exclusion. The multilateral institution sees its own degraded legitimacy (piton perspective) — performing authorization ritual while lacking enforcement capacity. The international legal doctrine sees naturalness (mountain perspective from analytical observer) — RBIO appears as inevitable accommodation to anarchic international system. But the structural data reveals the mountain as a false summit: the norms benefit identifiable agents (intervening states, contractors), are enforced selectively (according to great power interests), and are not equally revisable (weak states have constrained voice). The liberal institutional reading claims revision is possible through multilateral process, but the measurements show rising theater as the gap between authorized action and effective implementation widens — institutions increasingly perform legitimacy rather than deliver coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional values are derived from beneficiary/victim declarations and exit options. Intervening states with arbitrage exit (veto power) derive low d → low/negative χ; they are net beneficiaries. Targeted states with trapped exit derive high d → high f(d) → high χ; they experience maximal extraction. Regional powers with constrained exit derive moderate d → moderate f(d) → moderate χ; they experience mixed coordination and extraction. Multilateral institutions with constrained exit and both beneficiary and victim status derive d ≈ 0.50 → f(d) ≈ 0.65 → moderate-high χ. The analytical observer with analytical exit derives d ≈ 0.72 → high f(d) → risks naturalizing contingent arrangements. Scope modifier σ(S) = 1.2 for global scope: extractiveness is amplified by scope complexity (verifying RBIO compliance across 195 states, multiple implementation contexts). The formula χ = ε × f(d) × σ(S) produces: 0.52 × 0.65–1.15 × 1.2 ≈ 0.40–0.80 depending on observer position. The beneficiary (institutional, d ≈ 0.15, f(d) ≈ 0.02) experiences χ ≈ 0.01 (pure coordination); the victim (powerless, d ≈ 0.95, f(d) ≈ 1.42) experiences χ ≈ 0.88 (pure snare); the moderate state (d ≈ 0.50, f(d) ≈ 0.65) experiences χ ≈ 0.41 (tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy emerges at the analytical level when the mountain perspective (natural law) confronts the snare perspective (extraction). The analytical observer risks naturalizing RBIO as inevitable hierarchy in an anarchic system, but the powerless agent's snare classification and the identifiable beneficiaries reveal that RBIO is a contingent institutional arrangement. The tangled rope classification (this story's claimed_type) resolves the mandatrophy by acknowledging both genuine coordination (humanitarian accountability, conflict prevention, predictable interstate behavior) and genuine extraction (enforcement selectivity, conditionality, suppression of weak state voice). The constraint is neither pure coordination nor pure extraction — it is a hybrid that genuinely benefits some states while genuinely harming others. The rising theater ratio over the measurement interval signals increasing mandatrophy pressure: as the gap between authorized RBIO action and effective implementation widens, institutions increasingly perform legitimacy rather than deliver the coordination benefits the liberal reading promises. The false summit detector will flag the mountain perspective's naturalization and propose reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_mechanism,
    'Is enforcement selectivity driven by capacity constraints (liberal institutional explanation) or by strategic interests of powerful states (extractive explanation)?',
    'Correlation analysis: Does enforcement pattern track (a) state capacity to intervene, or (b) strategic interest of UNSC permanent members? Historical case comparison: interventions authorized vs. blocked; proxy variables for state interest, resource availability, alliance structure.',
    'If capacity-driven: the liberal institutional reading is structurally sound; selectivity is a technical problem solvable through capacity-building. If interest-driven: the constraint is a snare for weak states with liberal legitimacy theater; extractiveness increases, mandatrophy emerges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Whether enforcement selectivity is capacity-driven or interest-driven').

omega_variable(
    consent_authenticity_in_conditionality,
    'Can economic conditionality imposed through structural adjustment or sanctions be considered ''consent-based'' when the alternative is economic collapse or military intervention?',
    'Counterfactual analysis: What alternatives did the targeted state actually face? Comparison of negotiating positions with and without coercive backdrop. Document instances where states formally ''consented'' while contemporaneous records show coercion perception.',
    'If consent is authentic: conditionality is legitimate contract term; targeted state is moderate/constrained agent, not powerless/trapped. If coerced: conditionality is extraction mechanism; victims experience snare classification; extractiveness increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_authenticity_in_conditionality, conceptual, 'Whether consent under coercive threat constitutes authentic consent').

omega_variable(
    revisability_structural_constraint,
    'Can RBIO norms be genuinely revised through ''legitimate multilateral processes'' when revision requires consensus among parties with asymmetric power?',
    'Historical case studies: Instances where weak states proposed norm revision. What was the outcome? How many proposals were blocked by permanent members? Compare revision rates to those of peer systems (EU governance, regional bodies with more symmetric power). Test: does the UNSC ever revise RBIO interpretation against a permanent member''s preference?',
    'If genuinely revisable: the liberal claim holds; the framework offers exit through voice. If revision is structurally blocked: weak states have only exit or loyalty, not voice; constraint shifts toward snare; suppression increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revisability_structural_constraint, empirical, 'Whether RBIO norms are genuinely revisable through multilateral process').

omega_variable(
    reading_vs_hegemonic_extractive_thesis,
    'Does the liberal institutional reading differ structurally from the hegemonic extraction reading, or do they describe the same constraint with different legitimacy framings?',
    'Comparative analysis: (1) Beneficiary/victim declarations — do the readings agree on who benefits and who bears costs? (2) Extractiveness metric — would both readings measure ε identically? (3) Suppression mechanism — do they identify the same enforcement pathways? If the structural claims are identical and only the legitimacy framing differs, the readings coexist linguistically but may not be genuinely distinct constraints.',
    'If structurally identical: the readings are competing framings of one constraint (coexists_with relation confirmed). If structurally distinct: they describe different constraints and should be decomposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_hegemonic_extractive_thesis, conceptual, 'Whether liberal institutional and hegemonic extraction readings describe structurally distinct constraints').

omega_variable(
    false_summit_rbio_naturalness,
    'Is RBIO an immutable natural law of international politics, or a contingent institutional arrangement that appears natural because it benefits powerful states?',
    'Counterfactual: Could a different institutional arrangement (regional concert model, UN General Assembly majority rule, rotating intervention authority) satisfy coordination functions while reducing extraction? Historical analysis: When did RBIO norms emerge, and what conditions enabled them? Were alternative frameworks considered and rejected, or was RBIO the obvious default?',
    'If natural law: the mountain classification holds; constraint is inherent to anarchic international system. If contingent: false summit confirmed; the constraint is a snare with liberal legitimacy theater; extractiveness and mandatrophy emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_rbio_naturalness, conceptual, 'Whether RBIO is a natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_lib_tr_t0, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(rbio_lib_tr_t10, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(rbio_lib_tr_t20, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(rbio_lib_be_t0, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rbio_lib_be_t10, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(rbio_lib_be_t20, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(rbio_lib_su_t0, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(rbio_lib_su_t10, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(rbio_lib_su_t20, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, unsc_permanent_member_veto).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, structural_adjustment_conditionality).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_intervention_doctrine).

% DUAL FORMULATION NOTE:
% RBIO Practice Norm Complex decomposes into three structurally distinct constraints representing competing readings of the same kernel. The liberal institutional reading (this constraint) measures ε=0.52 and classifies as tangled_rope. The hegemonic extraction reading measures ε≥0.66 and classifies as snare. The sovereignty maximalist reading measures ε differently depending on the observable (state autonomy loss vs. humanitarian norm intrusion), suggesting further decomposition. All three are linked via kernel_id and must be read together for complete perspectival coverage. The liberal reading is the institutionally dominant interpretation in contemporary multilateral institutions; the hegemonic reading is analytically ascendant in critical IR scholarship; the sovereignty maximalist reading is state practice in Global South resistance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
