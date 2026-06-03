% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation to Admit Refugees but Not Economic Migrants (Kernel Reading)
 *   domain: political_philosophy/migration_law/international_law
 *
 * SUMMARY:
 *   The humanitarian obligation reading of border legitimacy posits that
 *   states have a duty to admit those fleeing persecution or disaster, but
 *   not general economic migrants. This reading grounds state obligation in
 *   the principle of humanitarian duty while preserving state sovereignty
 *   over border control by introducing a categorical distinction between
 *   forced displacement (refugees, persecuted persons) and voluntary
 *   migration (economic migrants). The reading instantiates a middle position
 *   in the border legitimacy kernel contest: it rejects the pure sovereignty
 *   reading (borders can exclude anyone) while rejecting the freedom of
 *   movement reading (borders are presumptively illegitimate). Instead, it
 *   conditionalizes the obligation: humanitarian duty applies only to
 *   specific categories of forced displacement. This constraint story models
 *   THIS reading alone, not the contest between readings. The sibling
 *   readings are structurally distinct constraints that appear in separate
 *   JSON files; this file represents only the humanitarian obligation
 *   instantiation.
 *
 * KEY AGENTS:
 *   - Refugees and Persecuted Persons: Primary victim-beneficiaries (powerless/trapped → snare and rope) — entitled to protection by the framework but dependent on state recognition of persecution category
 *   - Economic Migrants Excluded by Rule: Primary victim excluded from obligation (powerless/trapped → snare) — face survival pressures but categorical exclusion removes the humanitarian obligation entirely
 *   - Citizens of Receiving States: Secondary beneficiary (institutional/arbitrage) — benefit from the boundary because it limits admission obligations and potential fiscal/social costs
 *   - Receiving State (institutional actor): Primary beneficiary-coordinator (institutional/arbitrage and constrained) — gains soft power from humanitarian compliance while controlling costs through the categorical distinction
 *   - International Treaty Regime: Institutional actor (institutional/constrained → tangled rope) — coordinates refugee allocation responsibility while containing extraction through state sovereignty preservation
 *   - The Refugee/Economic Migrant Binary (institutional structure): Theater mechanism (institutional/mobile → piton) — categorical distinction persists through institutional inertia despite empirical contestation
 *   - Analytical Observer: Sees the reading as a genuine tangled rope — coordinates humanitarian duty with state interest but uses the boundary as an extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.38).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.52).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation to Admit Refugees but Not Economic Migrants (Kernel Reading)").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/migration_law/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, 'bd61063d-e322-4073-9a3c-e296dfc52eb2').
narrative_ontology:cs_kernel_codification('bd61063d-e322-4073-9a3c-e296dfc52eb2', fixed_text).
narrative_ontology:cs_authority_grounding('bd61063d-e322-4073-9a3c-e296dfc52eb2', lineage).
narrative_ontology:cs_interpretation_layer_present('bd61063d-e322-4073-9a3c-e296dfc52eb2').
narrative_ontology:cs_reading_relation('bd61063d-e322-4073-9a3c-e296dfc52eb2', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd61063d-e322-4073-9a3c-e296dfc52eb2', border_legitimacy__freedom_of_movement_reading, influences).
narrative_ontology:cs_axiom('bd61063d-e322-4073-9a3c-e296dfc52eb2', foundational, humanitarian_obligation_for_persecution).
narrative_ontology:cs_axiom_status(humanitarian_obligation_for_persecution, holdable).
narrative_ontology:cs_axiom_grounding('bd61063d-e322-4073-9a3c-e296dfc52eb2', humanitarian_obligation_for_persecution, deontological).
narrative_ontology:cs_axiom('bd61063d-e322-4073-9a3c-e296dfc52eb2', foundational, categorical_distinction_persecution_versus_economic).
narrative_ontology:cs_axiom_status(categorical_distinction_persecution_versus_economic, holdable).
narrative_ontology:cs_axiom_grounding('bd61063d-e322-4073-9a3c-e296dfc52eb2', categorical_distinction_persecution_versus_economic, empirically_contingent).
narrative_ontology:cs_reference_frame('bd61063d-e322-4073-9a3c-e296dfc52eb2', humanitarian_obligation_framework).
narrative_ontology:cs_drift_state('bd61063d-e322-4073-9a3c-e296dfc52eb2', contemporary_climate_migration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bd61063d-e322-4073-9a3c-e296dfc52eb2', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, receiving_states).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, citizens_of_receiving_states).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, refugees_and_persecuted).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants_categorically_excluded).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMIC MIGRANT EXCLUDED BY THE CATEGORICAL RULE (SNARE) — Trapped by the distinction itself. Faces poverty, lack of livelihood, climate-driven resource scarcity — survival pressures functionally indistinguishable from persecution. But categorical exclusion as 'economic migrant' removes the obligation. Maximum extraction: the state's humanitarian framework explicitly forecloses this agent's claim while maintaining it recognizes humanitarian duty. Suppression is near-total — no legal pathway, no exception mechanism, no appeal to the humanitarian principle itself.
constraint_indexing:constraint_classification(border_legitimacy__humanitarian_obligation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REFUGEE FLEEING PERSECUTION (ROPE) — Moderate power through legal status recognition and international convention (1951 Refugee Convention). Constrained exit: cannot return home safely but faces barriers in receiving states (language, employment discrimination, limited resources). However, also benefits from the humanitarian framework itself — the category 'refugee' creates legal standing, access to asylum procedures, and recognition of legitimacy. The coordination function is genuine: the state coordinates resource allocation for humanitarian protection while the refugee gains recognized status. Beneficiary and victim simultaneously — extraction is mixed with coordination.
constraint_indexing:constraint_classification(border_legitimacy__humanitarian_obligation_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RECEIVING STATE / SHORT-TERM (ROPE) — Benefits from the humanitarian obligation reading via international legitimacy, treaty compliance, and moral standing. The categorical distinction allows the state to appear humanitarian while containing fiscal and social costs by excluding economic migrants. Arbitrage exit: the state can maintain selective obligation, sign treaties, gain soft power, while controlling borders. Net beneficiary — the reading enables coordination that favors the state's interests.
constraint_indexing:constraint_classification(border_legitimacy__humanitarian_obligation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL COMMUNITY / TREATY REGIME (TANGLED ROPE) — Constrained by both humanitarian principles and state sovereignty. Must enforce refugee obligations while respecting state border authority. Genuine coordination function: the 1951 Refugee Convention solves the problem of allocating protection responsibility across states. But also contains asymmetric extraction: wealthy states can externalize refugee burden to poorer neighbors; upstream states can refuse to address root causes of flight. The regime's enforcement machinery (UNHCR, asylum adjudication) creates genuine overhead but also perpetuates the categorical distinction that enables exclusion.
constraint_indexing:constraint_classification(border_legitimacy__humanitarian_obligation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE REFUGEE/ECONOMIC MIGRANT BINARY AS INSTITUTIONAL THEATER (PITON) — The categorical distinction persists despite cumulative evidence that it cannot be sustainably drawn. Climate displacement is both environmental and economic; political instability triggers both persecution and resource scarcity; many migrants flee multiple combined pressures. The binary endures through institutional inertia (conventions, bureaucratic categories, policy frameworks) rather than through functional necessity. Theater ratio is high: asylum interviews and credibility assessments attempt to determine the 'true' motivation, but the underlying binary is increasingly performative — the category persists because institutions are built on it, not because it maps to reality. Mobile exit for states: countries are increasingly adopting humanitarian obligation frameworks (EU asylum directives, complementary pathways for climate migrants) that sidestep the binary entirely.
constraint_indexing:constraint_classification(border_legitimacy__humanitarian_obligation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — HUMANITARIAN OBLIGATION FRAMEWORK (TANGLED ROPE) — From a civilizational perspective, this reading coordinates genuine humanitarian duty (persecution requires protection) with legitimate state interest (control admission policy). But the coordination exhibits asymmetric extraction: the categorical distinction excludes agents facing survival pressures functionally similar to persecution. The framework genuinely coordinates humanitarian protection (coordination function present) while using the 'economic migrant' category to systematically exclude large victim populations (asymmetric extraction present). Extraction remains moderate because the framework IS functional — it does allocate protection and coordinate international responsibility — but the categorical boundary ensures that the boundary itself becomes an extraction mechanism.
constraint_indexing:constraint_classification(border_legitimacy__humanitarian_obligation_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_legitimacy__humanitarian_obligation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_legitimacy__humanitarian_obligation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, TR),
    TR >= 0.70.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading exhibits genuine coordination (states cooperate on refugee allocation, humanitarian principles) alongside asymmetric extraction (economic migrants are categorically excluded despite facing survival pressures). The extraction is not total (some agents — recognized refugees — benefit from the framework) and not minimal (the categorical distinction enables exclusion of large populations). The trajectory shows increasing extractiveness over the interval (0.28 → 0.38), reflecting accumulating pressure on the categorical boundary as climate displacement, state collapse, and economic instability make it harder to distinguish persecution from economic desperation. Suppression (0.52): Moderate-high. Excluded economic migrants face near-total suppression of their claims: no legal framework recognizes their obligation, no humanitarian pathway exists, categorical exclusion is backed by state enforcement. But suppression of recognized refugees is lower — the 1951 Convention creates legal standing and procedural protections. The average reflects a bifurcated suppression profile: high for excluded category, moderate for included. Theater ratio (0.58): Moderate-high. The boundary itself is increasingly theatrical. Asylum adjudication attempts to determine whether a migrant's flight was 'really' persecution or 'really' economic deprivation, but when climate change causes crop failure (economic) induced by state neglect (persecution-adjacent), the binary cannot coherently categorize the case. The theater has increased over time as case complexity has outpaced the categorical apparatus. States increasingly adopt humanitarian pathways (resettlement, complementary protection) that acknowledge the boundary is unsustainable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectives range from snare (excluded migrants) to rope (both refugees and receiving states, in different ways) to piton (the institutional binary itself) to tangled rope (international regime and analytical observer). The gap is primarily between the perspectives of recognized refugees and explicitly excluded economic migrants, both of whom face survival pressures but only one of whom is entitled to humanitarian protection. The receiving state sees coordination and legitimacy (rope); the excluded migrant sees pure extraction (snare); the refugee sees mixed protection and constraint (rope/tangled rope); the analytical observer sees the extraction embedded in the boundary itself (tangled rope). The most important gap is between the beneficiary-state perspective (humanitarian obligation is achievable and fair) and the excluded-migrant perspective (the boundary is arbitrary and lethal).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to THIS reading's categorical mechanism. Refugees recognized as persecuted have moderate d (beneficiaries of the framework despite constrained exit — their legal status is recognized but conditions in receiving states remain difficult). Economic migrants categorically excluded have high d (victims with trapped exit — the reading explicitly forecloses their humanitarian claim). Receiving states have low d (beneficiaries with arbitrage exit — they can choose admission levels, sign treaties, gain legitimacy, while controlling costs). The international treaty regime has moderate d (constrained — must enforce both humanitarian obligation and state sovereignty, resulting in mixed costs and benefits). The categorical binary itself has low d from the institutional perspective (beneficiary in the immediate term through institutional continuity, but facing mobility pressure from alternative frameworks like climate protection directives and complementary pathways).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This reading resolves mandatrophy by showing that the coordination function (allocating humanitarian responsibility) and extraction mechanism (categorical exclusion of economic migrants) are structurally inseparable. The frame that makes coordination possible — the distinction between refugees and economic migrants — is simultaneously the frame that enables exclusion. The reading is tangled rope precisely because it cannot separate coordination from extraction without collapsing the framework itself. A pure coordination reading would require either (a) admitting all forced displacement regardless of cause (moves toward freedom of movement reading) or (b) admitting that the coordination function is actually minimal and the mechanism is primarily extraction (moves toward snare classification). The mandatrophy is resolved by acknowledging that the humanitarian obligation reading genuinely coordinates refugee allocation AND genuinely extracts via categorical exclusion — both properties are intrinsic to the framework, not artifacts of perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_vs_deprivation_boundary,
    'Can persecution and severe economic deprivation be coherently distinguished as separate categories, or do they form a continuum where the boundary is inherently contestable?',
    'Comparative analysis of asylum case law across jurisdictions: identification of cases where identical survival pressures were classified differently (persecution vs economic) across systems or over time; longitudinal tracking of whether case law drift moves toward stricter persecution criteria (boundary hardens) or looser ones (boundary softens)',
    'If coherently distinguishable: this reading''s categorical distinction is structural and defensible. If continuum: the boundary is a choice point that appears natural but is actually policy-determined; reclassifies as more extractive (ε increases toward 0.5+). If drift toward stricter persecution criteria: institutional capture of the humanitarian obligation reading; reclassifies as snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(persecution_vs_deprivation_boundary, empirical, 'Whether persecution and deprivation can be coherently distinguished').

omega_variable(
    economic_migrant_causation_ambiguity,
    'When migration is driven by resource scarcity induced by conflict, failed governance, or climate change, is the migration ''economic'' or does it become persecution by proxy?',
    'Causal chain analysis: identification of cases where state policies or armed conflict created the resource scarcity driving migration; determination whether absence of direct persecution nullifies the humanitarian obligation if the survival pressure was state-induced',
    'If state-induced scarcity counts as persecution-equivalent: this reading forecloses itself — it cannot coherently exclude economic migrants whose economic desperation is state-caused. Reclassifies as internally incoherent (ε increases, mandatrophy worsens). If state-induced scarcity does NOT count: reading sustains its boundary but requires justification for why state causation does not trigger humanitarian obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_migrant_causation_ambiguity, conceptual, 'Status of state-induced resource scarcity as persecution-equivalent').

omega_variable(
    sibling_reading_foreclosure_coherence,
    'Does the humanitarian obligation reading actually foreclose the freedom_of_movement reading, or do they coexist as different normative commitments that can be held by different parties without logical contradiction?',
    'Identification of parties who hold both readings simultaneously (yes = coexists; no = forecloses or influences). Case law and policy analysis showing whether freedom of movement advocates accept the humanitarian obligation reading as a compromise position or reject it as inadequate.',
    'If coexists: revise reading_relations from forecloses to coexists_with. If forecloses and coherently: reading_relations correct. If forecloses but incoherently: indicates this reading contains internal contradictions that need surfacing in commentary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_coherence, conceptual, 'Logical relationship between humanitarian obligation and freedom of movement readings').

omega_variable(
    receiving_state_capacity_constraint,
    'Does the extractiveness (0.38) accurately reflect the constraint when capacity-constrained states face overwhelming numbers? Does extraction increase nonlinearly as absolute migrant volume increases?',
    'Comparative analysis of extractiveness in high-volume vs low-volume reception contexts; measurement of whether the categorical exclusion becomes more extractive (higher effective ε) when absolute numbers rise; identification of whether states with limited capacity experience the humanitarian obligation as a snare rather than a tangled rope.',
    'If capacity-dependent: ε should be decomposed into separate stories (capacity-unconstrained vs capacity-constrained receiving state). This story would be the unconstrained case (ε~0.38); a separate story models the constrained case (ε→0.6+, reclassifying toward snare). If uniform: current ε stands but commentary should address the capacity assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(receiving_state_capacity_constraint, empirical, 'Capacity-dependence of extractiveness').

omega_variable(
    axiom_holdability_humanitarian_duty,
    'Is the foundational axiom ''humanitarian_obligation_for_persecution'' currently holdable in international law, or has it been formally overridden or eroded by state practice?',
    'Audit of state ratification and implementation of 1951 Refugee Convention; measurement of rate of non-refoulement violations; tracking of whether humanitarian obligation is explicitly rejected (overridden) or merely ignored in practice (holdable but degraded)',
    'If overridden: reclassify axiom status from holdable to overridden; indicates this reading is losing its normative ground. If holdable but degraded: status correct but update commentary to note institutional erosion. If actively held: confirms axiom status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_holdability_humanitarian_duty, empirical, 'Current status of humanitarian obligation axiom in state practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_hum_tr_t0, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(border_hum_tr_t8, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 8, 0.51).
narrative_ontology:measurement(border_hum_tr_t16, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(border_hum_be_t0, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(border_hum_be_t8, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(border_hum_be_t16, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 16, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(border_hum_su_t0, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(border_hum_su_t8, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(border_hum_su_t16, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 16, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, resource_allocation).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, refugee_status_determination_procedural_theater).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, climate_displacement_categorization_crisis).

% DUAL FORMULATION NOTE:
% This story models the humanitarian obligation reading as ONE constraint in the border_legitimacy kernel contest. The 'border legitimacy' concept is not a single constraint — it is a contested kernel with at least three distinct readings, each instantiating a different constraint with different ε values and victim/beneficiary structures. Each reading appears in its own JSON file. This file links to the sibling readings via network.affects_constraints to indicate family membership. The constraint 'refugee_status_determination_procedural_theater' is downstream — it models the specific operational mechanism (asylum interviews, credibility assessment) through which the categorical distinction is enforced. The constraint 'climate_displacement_categorization_crisis' is a pressuring constraint — it models the contemporaneous collapse of the persecution/economic distinction under climate-driven migration, which increases pressure on this reading's categorical boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
