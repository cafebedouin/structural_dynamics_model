% ============================================================================
% CONSTRAINT STORY: sotu_1954_eisenhower_mutual_security_pact_korea
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1954_eisenhower_mutual_security_pact_korea, []).

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
 *   constraint_id: sotu_1954_eisenhower_mutual_security_pact_korea
 *   human_readable: U.S.-ROK Mutual Security Pact (1954)
 *   domain: foreign_policy/military_alliance
 *
 * SUMMARY:
 *   The 1954 U.S.-ROK Mutual Security Pact formalizes American commitment to
 *   defend South Korea against renewed Communist aggression, establishing
 *   institutional predictability for allied governments while binding U.S.
 *   military resources to an indefinite forward deployment. The constraint
 *   exhibits a fundamental tension between its genuine coordination function
 *   (deterring Soviet/Chinese expansion, stabilizing the Cold War perimeter)
 *   and its asymmetric extraction mechanisms (South Korea loses strategic
 *   autonomy; the U.S. accepts permanent military expenditure and casualty
 *   risk; allied states must align foreign policy with U.S. Cold War
 *   objectives). The constraint demonstrates how the same structural
 *   arrangement can be classified as pure coordination (rope) from the
 *   beneficiary's strategic perspective, as mixed coordination-extraction
 *   (tangled rope) from moderate allies' perspective, as pure extraction
 *   (snare) from the trapped beneficiary's perspective, and as institutional
 *   theater (piton) when the original threat environment degrades. The
 *   theater ratio (0.38 initially, rising to 0.65) reflects that the
 *   constraint's original coordination purpose (deterring Soviet
 *   intervention) remains genuine in the early Cold War but increasingly
 *   becomes a theatrical affirmation of alliance identity as the threat
 *   environment changes.
 *
 * KEY AGENTS:
 *   - South Korean State: Primary trapped beneficiary (powerless/trapped) — gains security guarantee but structurally cannot exit; autonomy extraction is maximum
 *   - U.S. Military-Strategic Establishment: Primary beneficiary (institutional/arbitrage) — gains forward basing, integrated Pacific defense architecture, legitimated military expenditure
 *   - U.S. Domestic Political Economy: Secondary victim (powerful/mobile) — benefits from Cold War deterrence but bears sustained military expenditure and operational risk
 *   - Allied Pacific Governments (Japan, Philippines, Taiwan allies): Moderate actors (moderate/constrained) — experience coordinated deterrence benefit layered with strategic dependency
 *   - Soviet and Chinese Leadership: Implicit threat targets (analytical/analytical) — the constraint is designed to constrain their options; structural relationship is deterrent/escalatory
 *   - Cold War International Order: Institutional builder (organized/constrained) — scaffold function: pact establishes NATO-like bilateral alliance model; designed with sunset logic tied to threat reduction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as geopolitical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1954_eisenhower_mutual_security_pact_korea, 0.52).
domain_priors:suppression_score(sotu_1954_eisenhower_mutual_security_pact_korea, 0.48).
domain_priors:theater_ratio(sotu_1954_eisenhower_mutual_security_pact_korea, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1954_eisenhower_mutual_security_pact_korea, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1954_eisenhower_mutual_security_pact_korea, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1954_eisenhower_mutual_security_pact_korea, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1954_eisenhower_mutual_security_pact_korea, tangled_rope).
narrative_ontology:human_readable(sotu_1954_eisenhower_mutual_security_pact_korea, "U.S.-ROK Mutual Security Pact (1954)").
narrative_ontology:topic_domain(sotu_1954_eisenhower_mutual_security_pact_korea, "foreign_policy/military_alliance").

domain_priors:requires_active_enforcement(sotu_1954_eisenhower_mutual_security_pact_korea).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1954_eisenhower_mutual_security_pact_korea, south_korean_state).
narrative_ontology:constraint_beneficiary(sotu_1954_eisenhower_mutual_security_pact_korea, u_s_pacific_military_posture).
narrative_ontology:constraint_victim(sotu_1954_eisenhower_mutual_security_pact_korea, u_s_military_readiness_distribution).
narrative_ontology:constraint_victim(sotu_1954_eisenhower_mutual_security_pact_korea, allied_strategic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOUTH KOREAN STATE (SNARE) — Formally benefits from U.S. defense guarantee but is structurally trapped within it. Cannot meaningfully exit the alliance without existential risk (North Korean invasion probability becomes unmanageable). The binding mechanism is existential threat, not voluntary coordination. The beneficiary status is coerced — South Korea gains security but loses strategic autonomy. The constraint appears as protection from the insider view but extraction (of autonomy, sovereignty over defense posture, operational entanglement) is maximum because exit is literal death.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_mutual_security_pact_korea, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ALLIED PACIFIC GOVERNMENTS (TANGLED ROPE) — Experience the pact as coordinated deterrence (genuine coordination benefit: collective security against Communist expansion) layered with asymmetric extraction (must align foreign policy with U.S. Cold War objectives, constrained to accept U.S. military bases and strategic positioning, dependent on U.S. commitment that could shift). Exit costs are high (loss of security umbrella, geopolitical isolation) but surmountable. Genuine coordination function (shared deterrence) coexists with asymmetric extraction (strategic dependency).
constraint_indexing:constraint_classification(sotu_1954_eisenhower_mutual_security_pact_korea, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: U.S. MILITARY-STRATEGIC ESTABLISHMENT (ROPE) — Experiences the pact as pure coordination: establishes forward basing rights, institutional predictability for military planning, integration of Pacific defense architecture, and legitimation for sustained military expenditure. The constraint solves the coordination problem of 'how do we deter Soviet/Chinese expansion while maintaining domestic support for peacetime military buildup?' The beneficiary (U.S. strategic posture) has arbitrage options — could negotiate different terms, withdraw, or realign — and experiences low effective extraction because the benefits align with institutional incentives.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_mutual_security_pact_korea, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: U.S. DOMESTIC POLITICAL ECONOMY (TANGLED ROPE) — Experiences genuine coordination benefit (Cold War deterrence aligns with domestic security interests, military-industrial expansion aligns with economic growth narrative) alongside asymmetric extraction (sustained military expenditure diverts resources from domestic investment, permanent forward deployment creates recurring operational costs and casualty risk). The U.S. public has formal exit options (electoral opposition, Congressional debate) but sustained Cold War framing constrains these options. The constraint binds U.S. military resources for indefinite duration against emergent cost-benefit analysis.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_mutual_security_pact_korea, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-WAR INTERNATIONAL ORDER / NATO-ALIGNED SYSTEM (SCAFFOLD) — The pact is a foundational scaffolding element of the liberal democratic alliance system. It establishes the institutional template (bilateral mutual defense + integrated command structure) that becomes the NATO model and Cold War alliance architecture. From this perspective, the constraint has genuine sunset logic: it is designed to be temporary, lasting only as long as the Soviet/Chinese military threat justifies the coordination cost. The constraint serves its structural function (preventing great-power realignment, securing perimeter states) and is meant to degrade once threat environment changes. Theater ratio is low (0.38) because the institutional purpose is genuine.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_mutual_security_pact_korea, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR IDEOLOGICAL FRAMING (PITON) — By the end of the Cold War, the structural justification for the mutual security pact (deterring Soviet expansion in Korea) has substantially degraded, but the institutional arrangement persists through inertia. The pact becomes theatrical: it continues as a symbolic affirmation of U.S.-ROK alliance despite the original threat environment disappearing. The theater ratio increases over the interval (0.38 → 0.65 by 2000s), reflecting that the institution persists as an identity-coordination mechanism ('defending democracy') rather than as a pure deterrence mechanism. The pact is maintained because both parties benefit from the identity frame, not because the original extraction logic still applies.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_mutual_security_pact_korea, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION (MOUNTAIN) — From a civilizational view, the pact appears as an inevitable structural response to geopolitical reality: divided peninsula, great-power competition, asymmetric military capabilities. The constraint appears naturalized as 'how great powers stabilize peripheries' rather than as a contingent institutional arrangement. However, the beneficiary declarations (both South Korean state and U.S. military establishment benefit) reveal this as a false summit: if the constraint naturalizes as immutable law, who benefits and who bears costs should be irrelevant — but they are highly relevant. The mountain classification signals that analysts risk treating contingent power arrangements as structural inevitabilities.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_mutual_security_pact_korea, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1954_eisenhower_mutual_security_pact_korea_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1954_eisenhower_mutual_security_pact_korea, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1954_eisenhower_mutual_security_pact_korea, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1954_eisenhower_mutual_security_pact_korea, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1954_eisenhower_mutual_security_pact_korea, TR),
    TR >= 0.70.

:- end_tests(sotu_1954_eisenhower_mutual_security_pact_korea_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, rising from 0.38): Moderate and rising. Initial extractiveness reflects genuine coordination benefit (deterring Communist expansion) with moderate asymmetric costs (South Korea loses autonomy, U.S. accepts military commitment). The rising trajectory reflects two processes: (1) accumulating U.S. military expenditure over time as forward deployment becomes permanent; (2) degradation of the original threat justification while institutional persistence maintains the binding mechanism. By year 35 (post-Cold War), the extractiveness is driven increasingly by institutional inertia (theater) and sunk costs rather than by active deterrence. Suppression (0.48): Moderate. South Korea faces high material barriers to exit (existential threat from North Korea, military imbalance) but these barriers are partly externally imposed (Soviet/Chinese military buildup) and partly institutional (U.S. alliance structure itself constrains alternative security arrangements). Allies face constrained exit options (can renegotiate but not costlessly abandon). U.S. faces domestic political constraints on rebalancing commitments. Theater ratio (0.38, rising to 0.65): Initially low (0.20), reflecting genuine military coordination and deterrence function. Rises over the interval as the original Cold War threat environment degrades and the pact persists through institutional inertia. By year 35, theater ratio indicates performative maintenance of alliance identity rather than active deterrence against the original threat.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence across structural positions. The U.S. military establishment sees pure coordination (rope) — the pact solves the deterrence problem and provides institutional benefits. South Korea sees maximum extraction (snare) — security benefit coexists with autonomy loss and structural trap. Allies see mixed coordination-extraction (tangled rope) — genuine collective deterrence layered with strategic dependency. The U.S. domestic economy sees moderate extraction (tangled rope) — Cold War benefits coexist with sustained military expenditure. The post-Cold War analyst sees institutional theater (piton) — the original coordination function has degraded but the institution persists. The civilizational observer risks seeing natural law (mountain) — great powers naturally stabilize peripheries — but beneficiary declarations reveal this naturalization as false summit. The perspectival gap widens over time: the constraint's classification from each perspective remains stable, but the ratio of genuine coordination benefit to theatrical inertia shifts, making the divergence increasingly apparent.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) from South Korea's perspective: trapped beneficiary + existential threat = d ≈ 0.95, producing maximum experienced extraction (χ). The constraint provides security (beneficiary status) but removes the option to exit, creating a coerced benefit that is structurally indistinguishable from extraction of autonomy. From the U.S. military establishment's perspective: institutional beneficiary + arbitrage options = d ≈ 0.10, producing negative/minimal χ. The beneficiary experiences the constraint as pure coordination. From the U.S. domestic economy's perspective: victim status (bears military expenditure) + constrained but existent political exit options = d ≈ 0.68, producing moderate χ. The domestic bearers of cost experience extraction but retain formal agency (electoral politics). From allied states' perspective: moderate power + constrained exit + mixed beneficiary/victim status = d ≈ 0.58, producing moderate χ. The constraint is experienced as genuine but asymmetric coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The mutual security pact resolves the mandatrophy by demonstrating that multiple classification types are simultaneously valid depending on structural position and time horizon. The mandatrophy is not 'is this coordination or extraction?' but 'whose perspective and at what time?' The pact is rope from the military establishment's view (genuine coordination, low extraction). It is snare from South Korea's view (trapped beneficiary). It is tangled rope from allied states' view (mixed coordination and dependency). It is tangled rope from the U.S. domestic economy's view (benefits from deterrence, costs from expenditure). It is scaffold from the institutional order view (temporary foundation, sunset tied to threat reduction). It is piton from the post-Cold War civilizational view (performative persistence past original purpose). The false summit classification (mountain from analytical perspective) signals that analysts risk naturalizing the contingent power arrangement as structural inevitability. The resolution is not choosing which type is 'correct' but recognizing that the constraint's structural character evolves with time horizon and changes meaning as the threat environment changes. The theater ratio trajectory (0.20 → 0.65) is diagnostic: as the original coordinating function (deterrence of active threat) degrades and the institutional form (alliance structure, base commitments) persists, the balance shifts from genuine coordination to theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    south_korean_autonomy_extraction,
    'Is the constraint extraction from South Korea (loss of strategic autonomy) a necessary cost of security or a contingent institutional arrangement that could be renegotiated?',
    'Comparative analysis: How much strategic autonomy do other U.S.-allied states retain (Japan, Germany, Australia)? Can South Korea conduct independent foreign policy without alliance retaliation? Does ally input on strategic decisions correlate with treaty text?',
    'If necessary: South Korean beneficiary status is genuine coerced benefit (trapped in snare). If contingent: extraction represents negotiable power asymmetry; perspective could shift to tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(south_korean_autonomy_extraction, empirical, 'Whether South Korean autonomy loss is inherent to security or extractive negotiation outcome').

omega_variable(
    us_domestic_extraction_accumulation,
    'Does sustained U.S. military commitment to Korea extract from domestic U.S. economy in ways that accumulate over time, or do military expenditures create offsetting economic benefits?',
    'Long-term economic accounting: Opportunity costs of forward-deployed troops and equipment (what else could $X billion/year buy domestically?). Comparison of military-industrial growth vs. domestic investment growth over interval. Casualty/cost asymmetry between U.S. and ROK burden-sharing.',
    'If extraction accumulates: U.S. domestic perspective shifts from rope (coordination benefit) toward snare (sustained extraction). If offsets exist: tangled_rope classification remains stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(us_domestic_extraction_accumulation, empirical, 'Whether U.S. military costs accumulate as extraction from domestic economy').

omega_variable(
    communist_threat_severity_assessment,
    'What is the actual threat level from Soviet/Chinese intervention in Korea? Is the mutual security pact a proportionate response to genuine threat or an institutional overresponse to worst-case scenarios?',
    'Declassified intelligence assessment: Soviet/Chinese military capacity and intention regarding Korea. Comparison of stated threat in 1954 vs. actual threat realized. Alternative deterrence mechanisms (ROK-indigenous capacity, looser alliance structures) and their effectiveness.',
    'If threat severe and persistent: scaffold and rope perspectives confirmed — genuine coordination value. If threat was exaggerated: constraint is overbuilt; theater ratio rises; piton trajectory confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communist_threat_severity_assessment, empirical, 'Actual vs. perceived Communist threat severity in Korea').

omega_variable(
    alliance_exit_option_empirical,
    'Is exit from the U.S.-ROK alliance actually trapped (impossible without existential risk) or constrained (high cost but possible)?',
    'Counterfactual analysis: What would happen to South Korean security if the pact were abandoned? How much of the difference is irreducible (geography, military balance) vs. institutional (U.S. commitments, base structure)? Can alternative security arrangements substitute?',
    'If truly trapped: South Korean exit_options should remain ''trapped''; snare classification stable. If constrained: exit_options could shift to ''constrained''; tangled_rope perspective becomes more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_exit_option_empirical, empirical, 'Whether ROK exit from pact is structurally impossible or high-cost').

omega_variable(
    institutional_inertia_post_cold_war,
    'After the end of the Cold War (Soviet collapse, reduction in overt Chinese military threat), does the pact persist because of residual structural benefits or primarily through institutional inertia and theater?',
    'Analysis of justifications offered in post-Cold War period. Measurement of actual deterrent activity vs. ceremonial activity. Comparison of stated threat environment in 1990 vs. 1954. Interviews with policymakers about maintenance rationale.',
    'If residual benefits: constraint remains tangled_rope; institutional value persists. If primarily inertia/theater: piton classification strengthens; theater_ratio rises; constraint becomes candidate for renegotiation or sunset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_inertia_post_cold_war, empirical, 'Post-Cold War: structural benefits vs. institutional inertia in pact maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1954_eisenhower_mutual_security_pact_korea, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(korea_pact_tr_t0, sotu_1954_eisenhower_mutual_security_pact_korea, theater_ratio, 0, 0.2).
narrative_ontology:measurement(korea_pact_tr_t10, sotu_1954_eisenhower_mutual_security_pact_korea, theater_ratio, 10, 0.32).
narrative_ontology:measurement(korea_pact_tr_t20, sotu_1954_eisenhower_mutual_security_pact_korea, theater_ratio, 20, 0.38).
narrative_ontology:measurement(korea_pact_tr_t35, sotu_1954_eisenhower_mutual_security_pact_korea, theater_ratio, 35, 0.65).

% Extraction over time
narrative_ontology:measurement(korea_pact_be_t0, sotu_1954_eisenhower_mutual_security_pact_korea, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(korea_pact_be_t10, sotu_1954_eisenhower_mutual_security_pact_korea, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(korea_pact_be_t20, sotu_1954_eisenhower_mutual_security_pact_korea, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(korea_pact_be_t35, sotu_1954_eisenhower_mutual_security_pact_korea, base_extractiveness, 35, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1954_eisenhower_mutual_security_pact_korea, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1954_eisenhower_mutual_security_pact_korea, u_s_forward_military_posture_pacific).
narrative_ontology:affects_constraint(sotu_1954_eisenhower_mutual_security_pact_korea, soviet_korean_peninsula_influence).
narrative_ontology:affects_constraint(sotu_1954_eisenhower_mutual_security_pact_korea, nato_bilateral_alliance_template).

% DUAL FORMULATION NOTE:
% The mutual security pact is a primary constraint that establishes the institutional template for U.S. Cold War alliance architecture. Downstream constraints (NATO bilateral model, U.S. forward military posture) inherit the structural features (integrated command, mutual defense obligation, forward bases) from this foundational pact. Upstream constraints (Soviet presence in Korea, division of peninsula) establish the threat environment that the pact responds to. The pact's extractiveness reflects the balance between genuine deterrence coordination and asymmetric military/political costs that accumulate over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1954_eisenhower_mutual_security_pact_korea, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
