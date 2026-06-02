% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__r2p_reading, []).

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
 *   constraint_id: westphalian_sovereignty__r2p_reading
 *   human_readable: Westphalian Sovereignty under R2P Reading: Conditional Protection and Residual International Responsibility
 *   domain: international_law/political_philosophy/international_relations
 *
 * SUMMARY:
 *   The R2P (Responsibility to Protect) reading of Westphalian sovereignty
 *   represents a specific institutional interpretation of a contested kernel:
 *   the relationship between state sovereignty and international
 *   responsibility for population protection. This reading asserts that
 *   Westphalian sovereignty is no longer absolute but conditional — states
 *   retain autonomous decision-making authority over internal affairs except
 *   when they commit, incite, or allow mass atrocities against populations.
 *   When this threshold is crossed, sovereignty is suspended and residual
 *   responsibility transfers to the international community (operationalized
 *   through UNSC authorization). This constraint exhibits multiple distinct
 *   structural mechanisms depending on observer position: for vulnerable
 *   populations it is a snare (their protection depends on UNSC action that
 *   may never come); for non-aligned states it is a snare (loss of unilateral
 *   autonomy with no reciprocal benefit); for hegemonic powers it is a rope
 *   (legitimation mechanism for intervention + geopolitical coordination);
 *   for the UN institutional apparatus it is a piton (maintained through
 *   theater despite enforcement gaps); and for humanitarianists it is a
 *   tangled rope (empowering and constraining simultaneously). The
 *   measurement trajectory shows rising theater ratio (0.35 → 0.68) over 15
 *   years, indicating that R2P has become increasingly performative: more UN
 *   debates, more investigation mandates, more rhetorical commitment, but
 *   enforcement selectivity has also increased. Suppression and
 *   extractiveness both rise and stabilize around year 10, corresponding to
 *   the post-2011 period when Libya intervention's contradictions became
 *   apparent (humanitarian goal achieved via military means that destabilized
 *   the region). The constraint is not false summit despite analytical
 *   observer temptation to naturalize it — structural data clearly shows
 *   beneficiaries (P5 coalition), victims (powerless states and vulnerable
 *   populations), and active enforcement (UNSC authorization mechanisms).
 *
 * KEY AGENTS:
 *   - Vulnerable Populations: Primary victim (powerless/trapped) — subjected to mass atrocities; their protection is claimed as constraint rationale but depends on UNSC authorization driven by geopolitical interests
 *   - Non-Aligned States: Primary victim (powerless/trapped at generational horizon) — lose unilateral sovereignty to R2P scrutiny without reciprocal security guarantees; trapped in Westphalian system with subordinated decision authority
 *   - UNSC Permanent Members (P5): Primary beneficiary (institutional/arbitrage) — retain veto power over authorization; can selectively invoke R2P for geopolitical advantage; coordinate intervention through humanitarian framing
 *   - Hegemonic Coalition (US-led): Primary beneficiary (powerful/arbitrage) — uses R2P doctrine to legitimize military interventions; bypasses classical sovereignty objections through humanitarian rationale
 *   - Regional Power Balancers: Secondary agent (organized/constrained) — experience mixed extraction and coordination; R2P both constrains regional rivals and creates liability for their client states
 *   - Humanitarianist Advocacy Network: Secondary agent (powerful/mobile) — gains legal standing and international legitimacy through R2P; constrained by UNSC gatekeeping and P5 veto; benefits from doctrine despite subordination to geopolitics
 *   - UN Institutional Apparatus: Institutional actor (institutional/arbitrage) — maintains R2P through bureaucratic infrastructure and rhetorical commitment; enforcement depends entirely on P5 political will; the institution performs protection (debates, mandates) while geopolitics determines actual action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__r2p_reading, 0.58).
domain_priors:suppression_score(westphalian_sovereignty__r2p_reading, 0.62).
domain_priors:theater_ratio(westphalian_sovereignty__r2p_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__r2p_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__r2p_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__r2p_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__r2p_reading, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__r2p_reading, "Westphalian Sovereignty under R2P Reading: Conditional Protection and Residual International Responsibility").
narrative_ontology:topic_domain(westphalian_sovereignty__r2p_reading, "international_law/political_philosophy/international_relations").

domain_priors:requires_active_enforcement(westphalian_sovereignty__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__r2p_reading, 'e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b').
narrative_ontology:cs_kernel_codification('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', formalized).
narrative_ontology:cs_authority_grounding('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', extraction).
narrative_ontology:cs_interpretation_layer_present('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b').
narrative_ontology:cs_reading_relation('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', westphalian_sovereignty__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', westphalian_sovereignty__gradated_reading, coexists_with).
narrative_ontology:cs_axiom('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', foundational, mass_atrocity_threshold_conditions_sovereignty).
narrative_ontology:cs_axiom_status(mass_atrocity_threshold_conditions_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', mass_atrocity_threshold_conditions_sovereignty, deontological).
narrative_ontology:cs_axiom('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', foundational, residual_international_responsibility_operative).
narrative_ontology:cs_axiom_status(residual_international_responsibility_operative, holdable).
narrative_ontology:cs_axiom_grounding('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', residual_international_responsibility_operative, conventional).
narrative_ontology:cs_reference_frame('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', conditional_sovereignty_with_international_oversight).
narrative_ontology:cs_drift_state('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', contemporary_post_2013, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e7c2a3f1-9d4e-4b8a-b9e2-6f1c5a8d3e4b', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__r2p_reading, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__r2p_reading, intervening_international_coalition).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__r2p_reading, unsc_permanent_members).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__r2p_reading, global_hegemonic_power).
narrative_ontology:constraint_victim(westphalian_sovereignty__r2p_reading, non_aligned_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__r2p_reading, populations_under_threat).
narrative_ontology:constraint_victim(westphalian_sovereignty__r2p_reading, state_autonomy_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE POPULATION (SNARE) — Populations experiencing mass atrocities are trapped: they cannot exit, cannot organize resistance without state apparatus, and their survival depends on the very state mechanism accused of targeting them OR on international intervention that may never materialize. The R2P reading creates the legal claim that triggers intervention, but the population itself bears the cost of atrocity while awaiting international authorization. Maximum extraction experienced — the population's tragedy becomes the constraint's triggering event, instrumentalized by the international system.
constraint_indexing:constraint_classification(westphalian_sovereignty__r2p_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ALIGNED STATE / STRUCTURAL VULNERABILITY (SNARE) — A non-aligned state that lacks security guarantees or UNSC veto power is trapped in a structural vulnerability: R2P doctrine subordinates their sovereignty to international scrutiny and potential intervention if mass atrocities occur. They cannot exit Westphalian sovereignty (it is the only available framework), cannot veto UNSC authorization, and face extraction via loss of autonomous decision-making over internal security and population management. The constraint is maintained by UNSC gatekeeping and diplomatic pressure. Suppression is structural: weak states have no credible alternative to compliance.
constraint_indexing:constraint_classification(westphalian_sovereignty__r2p_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL POWER BALANCER (TANGLED ROPE) — States like France, Turkey, or India experience tangled extraction and coordination. R2P legitimizes humanitarian intervention within their sphere of influence (coordination benefit — they can invoke it to constrain rivals). But it also creates liability: if their client state commits mass atrocities, they face pressure to withdraw support or face international sanction. Exit is constrained — they can negotiate within the UNSC but cannot escape the doctrine's reach. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(westphalian_sovereignty__r2p_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: UNSC PERMANENT MEMBER / HEGEMONIC COALITION (ROPE) — The P5 (especially US, UK, France) benefits from R2P as a coordination mechanism and a legitimation tool: they can frame intervention as humanitarian (rope) while advancing geopolitical interests. The doctrine enables arbitrage — they can selectively invoke R2P in some regions (Syria hesitation) while ignoring it in others (Rwanda), coordinating with allies through humanitarian framing. Exit is available via veto power. This perspective sees R2P primarily as coordination (establishing international legal consensus on intervention thresholds) rather than as asymmetric extraction. Extraction runs toward this agent, not away.
constraint_indexing:constraint_classification(westphalian_sovereignty__r2p_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMANITARIANIST ADVOCACY NETWORK (TANGLED ROPE) — Human rights organizations and humanitarian advocates benefit from R2P (provides legal standing and international legitimacy for intervention advocacy) but also face constraints: they are dependent on UNSC authorization and P5 strategic interests; their moral claims must be filtered through geopolitical logic. R2P is both empowering (creates legal obligation to protect populations) and constraining (subordinates humanitarian need to Security Council veto). Moderate extraction with real coordination function: international norms on protection.
constraint_indexing:constraint_classification(westphalian_sovereignty__r2p_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: UN INSTITUTIONAL APPARATUS (PITON) — The UN and its bureaucratic mechanisms maintain R2P through institutional inertia and normative theater: the doctrine is formally enshrined in the UN Charter (interpreted) and General Assembly resolutions (2005 World Summit), but its actual enforcement is sporadic and geopolitically driven. The UN's existence is partly justified through R2P rhetoric, but the institution cannot independently enforce the doctrine — enforcement depends entirely on P5 political will. Theater ratio is high because the UN performs protection (holding debates, issuing statements, authorizing investigations) while protection is actually determined by hegemonic interest. The doctrine persists as institutional theater because replacement frameworks have not materialized.
constraint_indexing:constraint_classification(westphalian_sovereignty__r2p_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED DOCTRINE VIEW (MOUNTAIN) — From a civilizational/universal perspective, the R2P reading can appear as a natural law: populations have an inherent right to protection from mass atrocities; this right cannot be suspended by state sovereignty claims; therefore international responsibility is inherent to the global order. The constraint appears to emerge naturally from first principles of human dignity and collective security. However, the structural data contradicts the mountain classification: the constraint requires active enforcement by the UNSC, benefits identifiable powerful actors, and suppresses alternative frameworks (e.g., state autonomy doctrine). This is a false summit — the 'natural' framing naturalizes what is actually a contingent institutional arrangement grounded in post-1945 geopolitical power distribution.
constraint_indexing:constraint_classification(westphalian_sovereignty__r2p_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__r2p_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westphalian_sovereignty__r2p_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westphalian_sovereignty__r2p_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(westphalian_sovereignty__r2p_reading, TR),
    TR >= 0.70.

:- end_tests(westphalian_sovereignty__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The R2P reading extracts significant authority from non-aligned states over their internal sovereignty decisions. The extraction is not total (states retain autonomous governance except during mass atrocity crises) and benefits from legitimate humanitarian rationale (population protection is a real value). However, the UNSC's selective enforcement and geopolitical gatekeeping reveal that extraction benefits P5 coalition interests: they can invoke R2P in regions where they want intervention (geopolitical advantage) and ignore it elsewhere (preserving autonomy for allies). The extractiveness value reflects both the real coordination function (legitimate protection norms) and the asymmetric enforcement that exploits that function. Suppression (0.62): Moderate-high, rising over interval. Initial suppression (0.45) was lower because R2P was aspirational and not uniformly applied. As the doctrine matured and UNSC practice institutionalized selective enforcement, suppression increased: non-aligned states now live under continuous scrutiny and risk of intervention based on evolving atrocity thresholds. The suppression operates through normative pressure (sovereignty becomes conditional), legal liability (interventions authorized under R2P doctrine), and reputational risk (non-compliance with humanitarian norms). Theater ratio (0.65): High. The UNSC performs protection through debate, mandate creation, investigation authorization, but actual enforcement depends entirely on P5 political alignment. Libya 2011 was authorized because P5 interests converged on intervention against Gaddafi; Syria 2013 was vetoed despite comparable violence because P5 interests diverged. The doctrine's theater has increased because the rhetorical commitment to universal protection has not been matched by consistent enforcement — the gap between 'protection is a universal responsibility' and 'authorization follows P5 interests' widens as cases accumulate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence between powerless victims (snare), non-aligned states (snare with structural traps), regional powers (tangled rope with negotiation space), and P5 beneficiaries (rope with veto authority). The R2P reading creates the legal claim that populations under threat have a right to international protection and that this right conditions state sovereignty. Yet the UNSC's selective authorization reveals that the doctrine's actual enforcement is driven by geopolitical alignment rather than atrocity severity. This gap between the doctrine's normative claim ('universal responsibility to protect') and its practice ('selective intervention following P5 interests') is the source of high suppression: non-aligned states cannot credibly predict whether UNSC will authorize intervention in their case, forcing defensive preparedness and sovereignty restrictions. The humanitarianist perspective sees genuine coordination (establishing international protection norms) alongside extraction (subordination to UNSC gatekeeping). The UN institutional perspective sees piton dynamics: the institution maintains R2P through bureaucratic ritual (debates, mandates) while enforcement is purely external to UN mechanisms (depends on P5 military capability and political will).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives. Vulnerable populations (powerless/trapped) experience d ≈ 0.95 (full target of constraint): they have no exit options and bear full cost of atrocity while awaiting rescue that may not arrive. Non-aligned states (powerless/trapped at generational scale) experience d ≈ 0.90 (near-full target): they lose sovereign decision authority with no reciprocal benefit; the constraint is maintained through normative pressure and UNSC veto that subordinates their preferences. Regional powers (organized/constrained) experience d ≈ 0.55 (mixed): they benefit from R2P as a tool to constrain regional rivals but face extraction through liability for client-state atrocities; exit is constrained but negotiation is possible. P5 beneficiaries (institutional/arbitrage) experience d ≈ 0.15 (near-zero target, beneficiary position): they retain veto power and can selectively invoke R2P for geopolitical advantage; extraction runs toward them through legitimation and coordination benefits. The humanitarianist network (powerful/mobile) experiences d ≈ 0.50 (symmetric): real benefits from R2P legal standing and moral authority, but constrained by UNSC gatekeeping and P5 veto. The UN institutional apparatus (institutional/arbitrage) experiences d ≈ 0.10 (beneficiary): the institution's legitimacy is enhanced by R2P rhetoric and mandate creation, even though enforcement is entirely external. The analytical observer experiences d ≈ 0.72 (moderate target with residual duty): the observer is epistemically bound to follow UNSC authorization patterns while recognizing they diverge from universal protection claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the R2P reading's genuine coordination function (establishing international norms around population protection from mass atrocities) from its asymmetric enforcement mechanism (UNSC gatekeeping that benefits P5 coalition). The constraint is a true tangled rope: it contains both a real coordination component (legitimate protection norms that benefit all parties including powerless states, who gain nominal right to protection) and a real extraction component (the selective enforcement that extracts authority and reduces autonomy for non-aligned states). The mandatrophy dissolves when we recognize that both aspects are simultaneously true — R2P is neither pure coordination nor pure extraction, but a hybrid mechanism where coordination legitimates extraction. The doctrine provides protection norms (genuine good) while enabling geopolitical intervention (extraction benefit for P5). This is the signature tangled rope structure: cannot dissolve the constraint without losing the coordination function; cannot maintain the coordination function without sustaining the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mass_atrocity_definition_threshold,
    'What threshold of violence and intent constitutes ''mass atrocities'' triggering R2P residual responsibility? Is the threshold empirical (X deaths + intent to destroy group) or normative (humanitarian threshold set by political consensus)?',
    'Historical comparison of cases coded as triggering R2P (Libya 2011, Syria assessments, Rwanda retrospective) versus non-triggering cases (Turkey-PKK, Myanmar-Rohingya selective coding). Analysis of whether threshold variation follows empirical patterns or political interests.',
    'If empirical: constraint is narrower and more predictable (mountain-adjacent). If normative/political: constraint is broader and more extractive (snare adjacent). Affects whether the R2P reading is ''inherent to humanity'' (mountain false summit) or ''contingent on P5 judgment'' (tangled rope true).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mass_atrocity_definition_threshold, empirical, 'Definition threshold for mass atrocities under R2P').

omega_variable(
    unsc_authorization_selectivity,
    'Is the UNSC''s selective authorization of interventions (Libya authorized, Syria not, Rwanda post-facto) driven by genuine empirical disagreement about atrocity severity, or by geopolitical alignment of P5 interests?',
    'Analysis of case-pairs with comparable violence thresholds but different authorization outcomes (Syria 2013 vs Libya 2011; Myanmar Rohingya vs other cases). Mapping of UNSC authorization patterns to P5 strategic interest in each region.',
    'If empirical disagreement: R2P is genuinely constrained by disagreement, suggesting legitimate coordination (rope tier). If geopolitical: selectivity reveals UNSC gatekeeping extracts authority for P5 political interests, confirming snare/tangled_rope. Affects whether suppression is ''constraint from honest disagreement'' or ''enforcement of asymmetric power.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unsc_authorization_selectivity, empirical, 'UNSC selectivity in authorization patterns').

omega_variable(
    intervention_effectiveness_doctrine_coupling,
    'Does R2P doctrine produce net positive outcomes for populations (lives saved by intervention) or does the doctrine''s strategic deployment (authorized when P5 interests align) result in iatrogenic harm (destabilization, prolonged conflict, power vacuum)?',
    'Longitudinal outcome analysis of authorized interventions (Libya, Kosovo, Sierra Leone, East Timor) versus counterfactual non-intervention scenarios; measurement of population welfare trajectories post-intervention; distinction between immediate protection (atrocities stopped) and medium-term harm (state collapse, secondary violence).',
    'If net positive: extractiveness estimate is too high — suppression and beneficiary extraction are justified by humanitarian outcome. If net negative or neutral: extractiveness estimate confirmed — the doctrine serves P5 interests more than population protection. Affects classification along entire perspectival set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_effectiveness_doctrine_coupling, empirical, 'Net effectiveness of R2P authorized interventions').

omega_variable(
    reading_interpretation_divergence,
    'Which sibling reading (absolutist vs gradated) is the doctrine actually implementing through UNSC practice? Does actual UNSC behavior instantiate the R2P reading''s claim of conditional sovereignty with residual international responsibility, or does it revert to absolutist sovereignty (with R2P as normative aspiration rather than binding practice)?',
    'Comparison of UNSC voting patterns and authorization practices over time: are P5 states treating R2P as conditioning sovereignty (willingness to authorize intervention in peer states), or as a doctrine applicable only to weaker states (preserving veto-backed exemption from R2P scrutiny)? Historical trajectory of doctrine interpretation.',
    'If practice instantiates R2P reading: constraint is as described (tangled rope with snare experiences). If practice reverts to absolutism: the R2P reading is theater (piton) and the actual constraint is absolutist sovereignty. Classification of UNSC institutional perspective may shift from rope to piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_interpretation_divergence, empirical, 'Actual doctrine implementation versus sibling reading').

omega_variable(
    post_colonial_state_asymmetry,
    'Does R2P doctrine disproportionately constrain post-colonial states relative to established powers? Is the doctrine''s ''universal'' framing masking a structural enforcement asymmetry?',
    'Comparative analysis of R2P scrutiny applied to African states, Southeast Asian states, and Middle Eastern states versus scrutiny applied to NATO members or P5 allies. Measurement of UNSC debate frequency, investigation authorizations, and intervention willingness by regional power status.',
    'If asymmetry confirmed: extractiveness estimate too low — the constraint functions as neocolonial sovereignty subordination for non-aligned states, while reserving veto-backed autonomy for established powers. Snare classification for non-aligned states is correct; rope for P5 conceals extractive enforcement. If symmetry demonstrated: constraint is genuinely universal (though selectivity in enforcement remains).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_colonial_state_asymmetry, empirical, 'Asymmetric application of R2P to post-colonial states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__r2p_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(r2p_tr_t0, westphalian_sovereignty__r2p_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(r2p_tr_t5, westphalian_sovereignty__r2p_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(r2p_tr_t10, westphalian_sovereignty__r2p_reading, theater_ratio, 10, 0.65).
narrative_ontology:measurement(r2p_tr_t15, westphalian_sovereignty__r2p_reading, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(r2p_be_t0, westphalian_sovereignty__r2p_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(r2p_be_t5, westphalian_sovereignty__r2p_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(r2p_be_t10, westphalian_sovereignty__r2p_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(r2p_be_t15, westphalian_sovereignty__r2p_reading, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(r2p_su_t0, westphalian_sovereignty__r2p_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(r2p_su_t5, westphalian_sovereignty__r2p_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(r2p_su_t10, westphalian_sovereignty__r2p_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(r2p_su_t15, westphalian_sovereignty__r2p_reading, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__r2p_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__r2p_reading, westphalian_sovereignty__absolutist_reading).
narrative_ontology:affects_constraint(westphalian_sovereignty__r2p_reading, westphalian_sovereignty__gradated_reading).
narrative_ontology:affects_constraint(westphalian_sovereignty__r2p_reading, unsc_permanent_member_veto_authority).
narrative_ontology:affects_constraint(westphalian_sovereignty__r2p_reading, humanitarian_intervention_legitimacy).
narrative_ontology:affects_constraint(westphalian_sovereignty__r2p_reading, post_colonial_state_autonomy).

% DUAL FORMULATION NOTE:
% The Westphalian sovereignty kernel decomposes into three structurally distinct constraint stories: absolutist reading (sovereignty is inviolable, ε ≈ 0.15, mountain), R2P reading (sovereignty is conditional on protection, ε ≈ 0.58, tangled rope), and gradated reading (sovereignty degrades continuously with humanitarian performance, ε ≈ 0.42, tangled rope). Each reading has different beneficiaries, victims, and enforcement mechanisms. They are linked through network.affects_constraints because UNSC practice (actual implementation) instantiates one reading while discursively invoking another, creating institutional tension. The R2P reading is upstream of UNSC authority and post-colonial autonomy constraints — it enables and conditions them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__r2p_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
