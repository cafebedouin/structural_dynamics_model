% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Commerce Clause Expansive Federal Reading: Interstate Commerce Encompasses All Economic Activity with Substantial Aggregate Effects
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   The Commerce Clause text ('Congress shall have Power ... To regulate
 *   Commerce ... among the several States') is a kernel contested by three
 *   major readings within American constitutional law. This constraint story
 *   instantiates ONE reading: the expansive federal reading, which interprets
 *   'Commerce ... among the several States' to encompass any economic
 *   activity with substantial aggregate effects on interstate markets. Under
 *   this reading, the federal government holds regulatory authority over
 *   vastly more economic activity than the Framers likely envisioned,
 *   subordinating state sovereignty to federal administrative authority. The
 *   expansive reading became ascendant in the New Deal transformation of
 *   constitutional doctrine and has been the dominant interpretation for
 *   nearly a century. The constraint exhibits tangled rope structure: genuine
 *   coordination function (establishing uniform rules for interstate
 *   commerce, solving multi-jurisdictional coordination problems) coupled
 *   with extractive subordination of state authority (forelosure of state
 *   regulatory autonomy, concentration of power in federal apparatus). The
 *   measurement trajectory shows the reading's accrual over time: from a
 *   contestable interpretation at the founding (extractiveness 0.25) to the
 *   dominant doctrine by the contemporary era (extractiveness 0.58), with
 *   theater ratio rising as the reading relies increasingly on expansive
 *   interpretation of its textual authorization rather than novel doctrinal
 *   development.
 *
 * KEY AGENTS:
 *   - Federal Regulatory Apparatus: Primary beneficiary (institutional/arbitrage) — EPA, OSHA, FTC, SEC, and federal agencies gain enumerated authority; experience the reading as pure coordination
 *   - National Policy Coherence Advocates: Coordinated beneficiary (organized/arbitrage) — interstate chambers, large corporations, national consumer movements benefit from uniform federal standards
 *   - State Sovereignty: Primary victim (powerless/trapped) — state governments lose regulatory authority over economic domains; cannot exit or alternatives to federal preemption within this reading
 *   - Local & Regional Authority: Secondary victim (moderate/constrained) — cities, counties, regional bodies face preemption and dormant Commerce Clause scrutiny; higher exit costs but some negotiation space with federal agencies
 *   - Originalist Constitutional Scholars: Alternative institutional position (analytical/constrained) — read the Commerce Clause narrowly; experience the expansive reading as capturing and degrading the constitutional text (piton perspective)
 *   - States as Organized Coalition: Mixed agent (organized/constrained) — when coordinating through compacts or federalist movements, states both coordinate on uniform rules AND bear extraction from federal override authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.58).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.62).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Commerce Clause Expansive Federal Reading: Interstate Commerce Encompasses All Economic Activity with Substantial Aggregate Effects").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, 'f833085c-3452-4b46-9528-559c2fe07d92').
narrative_ontology:cs_kernel_codification('f833085c-3452-4b46-9528-559c2fe07d92', fixed_text).
narrative_ontology:cs_authority_grounding('f833085c-3452-4b46-9528-559c2fe07d92', lineage).
narrative_ontology:cs_interpretation_layer_present('f833085c-3452-4b46-9528-559c2fe07d92').
narrative_ontology:cs_reading_relation('f833085c-3452-4b46-9528-559c2fe07d92', originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('f833085c-3452-4b46-9528-559c2fe07d92', substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('f833085c-3452-4b46-9528-559c2fe07d92', foundational, economic_activity_aggregate_effects_doctrine).
narrative_ontology:cs_axiom_status(economic_activity_aggregate_effects_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('f833085c-3452-4b46-9528-559c2fe07d92', economic_activity_aggregate_effects_doctrine, instrumental).
narrative_ontology:cs_axiom('f833085c-3452-4b46-9528-559c2fe07d92', secondary, federal_regulatory_uniformity_priority).
narrative_ontology:cs_axiom_status(federal_regulatory_uniformity_priority, holdable).
narrative_ontology:cs_axiom_grounding('f833085c-3452-4b46-9528-559c2fe07d92', federal_regulatory_uniformity_priority, conventional).
narrative_ontology:cs_reference_frame('f833085c-3452-4b46-9528-559c2fe07d92', enumerated_federal_commerce_authority).
narrative_ontology:cs_drift_state('f833085c-3452-4b46-9528-559c2fe07d92', contemporary_new_deal_plus_doctrine, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f833085c-3452-4b46-9528-559c2fe07d92', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, uniform_market_regulation_constituencies).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_sovereignty).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_economic_variation).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, federalism_structural_parity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE SOVEREIGNTY (SNARE) — State regulatory authority is trapped within the expansive reading's definitional scope. Any economic activity with even indirect, aggregate effects on interstate commerce falls under federal enumerated power. States cannot exit: their own internal regulations are subject to Commerce Clause preemption. No alternatives exist except formal constitutional amendment. Maximum extraction from state power perspective.
constraint_indexing:constraint_classification(commerce_clause_text__expansive_federal_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL & LOCAL REGULATORY AUTHORITY (SNARE) — Cities, counties, and regional bodies face high suppression from the expansive reading's enforcement. Local economic regulation (licensing, zoning with economic effects, environmental protection tied to commerce) are subject to federal preemption or dormant Commerce Clause scrutiny. Exit costs are severe (losing regulatory instruments or litigating every local decision), but some jurisdictions maintain exit pathways through negotiation with federal agencies or by structuring regulations as health/safety rather than commerce controls.
constraint_indexing:constraint_classification(commerce_clause_text__expansive_federal_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATES AS ORGANIZED COALITION (TANGLED ROPE) — When states organize (through interstate compacts, ALEC, coordinated litigation), they both coordinate economic policy (genuine coordination benefit via uniform rules across regions) AND experience extraction (loss of unilateral regulatory authority to federal override). States benefit from some coordination gains (predictable interstate commerce, reduced race-to-bottom dynamics in some domains) while bearing extraction costs (foreclosure of local variation that could benefit constituent populations).
constraint_indexing:constraint_classification(commerce_clause_text__expansive_federal_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL REGULATORY APPARATUS (ROPE) — The expansive reading is the primary beneficiary. Federal agencies (EPA, OSHA, FTC, SEC) gain enumerated authority over an essentially unlimited economic domain. National policy coherence advocates (interstate commerce chambers, large corporations seeking uniform rules, national consumer protection movements) benefit from predictable federal standards and absence of state-by-state variation. Experience the constraint as pure coordination: solving the multi-jurisdictional coordination problem. Net beneficiaries with high arbitrage options (can exit specific regulatory domains by appealing to Congress or using administrative procedures).
constraint_indexing:constraint_classification(commerce_clause_text__expansive_federal_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ORIGINALIST ALTERNATIVE VIEW (PITON) — From the originalist reading's perspective, the expansive reading is a degraded institutional form that has captured the constitutional apparatus. The original text ('to regulate Commerce ... among the several States') is being performed through the expansive reading's theatrical interpretation, not genuine original meaning. The Piton classification reflects high theater (the reading invokes constitutional text as authority while departing substantially from its historical scope) and persistence despite acknowledged structural degradation within originalist jurisprudence.
constraint_indexing:constraint_classification(commerce_clause_text__expansive_federal_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global analytical position, the expansive reading exhibits both genuine coordination function (solving multi-state market coordination) and extractive structure (subordinating state sovereignty to federal authority). The reading is not a natural law but a sustained institutional commitment to a specific constitutional interpretation that has accrued significant beneficiary constituencies (federal agencies, corporations seeking regulatory certainty, national movements). Classified as tangled rope because the coordination gains are real but the extraction structure is evident and actively enforced.
constraint_indexing:constraint_classification(commerce_clause_text__expansive_federal_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commerce_clause_text__expansive_federal_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commerce_clause_text__expansive_federal_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, TR),
    TR >= 0.70.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The expansive reading extracts state regulatory authority across a vast domain of economic activity. However, extraction is not maximal because genuine coordination gains exist (uniform interstate commerce rules solve legitimate multi-jurisdictional problems). The reading benefits national policy coherence, addresses race-to-the-bottom risks, and enables federal consumer and worker protections. States retain some regulatory authority in non-commerce domains and can negotiate with federal agencies. The trajectory from 0.25 (founding) to 0.58 (contemporary) reflects the reading's historical accrual — from a contestable interpretation to settled doctrine, with increased subordination of state alternatives. Suppression (0.62): High. The reading includes strong enforcement mechanisms (dormant Commerce Clause preemption, Supremacy Clause doctrines, federal agency authority) that suppress state alternatives. States cannot maintain parallel regulatory systems or opt out of federal standards. However, suppression is not total — states retain police powers in health/safety domains and can litigate preemption boundaries, creating exit pathways at high cost. Theater ratio (0.65): Moderate-high. The reading relies heavily on interpretive theater: the constitutional text ('regulate Commerce ... among the several States') is invoked as authorization, but the reading's actual scope vastly exceeds the text's original meaning. Courts perform constitutional deference to federal authority while minimizing originalist constraints. The theater has increased over time as the reading has become settled doctrine and relies on established precedent rather than novel textual exegesis. Contemporary doctrine treats the reading as texturally compelled, even though it is better understood as a sustained institutional commitment to a particular interpretive framework.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence across the constitutional observership. State governments see snare: they are trapped in a system that forecloses their regulatory autonomy. Federal agencies see rope: they are solving the coordination problem of interstate commerce. Originalist scholars see piton: the expansive reading has captured the constitutional apparatus and performs the text rather than honoring its original scope. The analytical observer sees tangled rope: coordination gains are real, but extraction is evident and structurally enforced. The perspectival gap emerges from different experiences of the same reading — beneficiaries and victims occupy fundamentally different structural positions within the expansive reading's scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's structural position relative to this specific reading. Federal agencies are beneficiaries with arbitrage options (can exit specific regulatory domains by appealing to Congress or shifting enforcement priorities) — low d → negative f(d) → negative experienced extraction. States are victims with trapped options (cannot exit the federal system without amendment) — high d → high f(d) → maximum experienced extraction. The analytical observer measures from a neutral position (d ≈ 0.73) capturing the reading's structural asymmetry. Organized states as a coalition have constrained exit (can negotiate with federal agencies, coordinate through compacts) — moderate d. The directional divergence explains why the same reading classifies as rope (federal perspective), snare (state perspective), and tangled rope (analytical perspective).
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive reading resolves the mandatrophy through perspectival plurality. The reading IS coordinating interstate commerce (genuine rope-level function) for beneficiaries and coordinating entities. Simultaneously, IT IS extracting state regulatory authority (genuine snare-level extraction) for state victims. The contradiction is not a classification error — it is an accurate depiction of tangled rope structure: a constraint that delivers both coordination and extraction, with asymmetric distribution. The constraint does NOT reduce to pure extraction (snare) because the coordination gains are structurally real and economically significant. It does NOT reduce to pure coordination (rope) because the extraction of state sovereignty is enforced and irreversible without constitutional amendment. The tangled rope classification captures both truths simultaneously from the analytical vantage point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantial_effects_threshold_ambiguity,
    'What degree of aggregate economic effect constitutes ''substantial'' for Commerce Clause coverage? Does any measurable effect suffice, or is a threshold required?',
    'Jurisprudential analysis of Wickard v. Filburn, Gonzales v. Raich, and subsequent cases establishing the operational threshold; empirical measurement of effects courts deem substantial vs. insubstantial',
    'If any measurable effect triggers federal jurisdiction: the reading has maximal scope and suppression, approaching pure extraction (snare classification from state perspective). If a high numerical threshold is required: states retain meaningful regulatory autonomy in minor-effect domains, moderating extraction to tangled rope levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantial_effects_threshold_ambiguity, empirical, 'Definition of ''substantial aggregate effects'' threshold for Commerce Clause jurisdiction').

omega_variable(
    reading_vs_natural_law_boundary,
    'Is the expansive reading a necessary interpretation of the constitutional text, or is it a contestable reading that could be replaced without denying the text itself?',
    'Constitutional interpretation scholarship comparing originalist exegesis (Barnett, McGinnis) with living constitution (Ackerman, Balkin); analysis of whether the text''s ordinary meaning constrains the expansive reading or permits it as one valid reading among others',
    'If necessary interpretation: the expansive reading approaches mountain classification (immutable constitutional law). If contestable: it remains tangled rope, potentially foreclosable by alternative readings. This is the core kernel ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_natural_law_boundary, conceptual, 'Whether expansive reading is compelled by text or one contestable interpretation among alternatives').

omega_variable(
    federalism_equilibrium_stability,
    'Does the expansive reading produce a stable equilibrium between federal and state power, or does it create pressure toward further federal consolidation and eventual state power erosion to ceremonial status?',
    'Historical trajectory analysis: comparison of state regulatory authority across centuries; identification of reversal points or renewed federalism doctrines (like the Commerce Clause limits in United States v. Lopez); assessment of whether political coalitions can sustain federalism even under an expansive reading',
    'If stable equilibrium: states retain meaningful authority zones despite the reading''s scope. If erosive: the reading''s structure predicts continued state power loss, confirming the snare classification from state perspective. If reversal is possible: alternative readings may become ascendant at different historical moments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_equilibrium_stability, empirical, 'Long-term stability of federalism equilibrium under expansive Commerce Clause reading').

omega_variable(
    kernel_reading_status_mandatrophy,
    'This is ONE reading of the commerce_clause_text kernel. Are all three readings (expansive, originalist, substantial_effects_limited) equally valid under living constitutionalism, or does one reading foreclose the others within a single coherent constitutional framework?',
    'Meta-constitutional analysis: examination of whether the Supreme Court''s doctrine admits all three readings as internally consistent positions on the same text, or whether doctrinal commitments made in pursuit of one reading logically entail rejection of the others',
    'If equally valid (coexists_with): the kernel admits multiple readings simultaneously; classification varies by observer context. If one forecloses others: the kernel has a terminal attractor reading; alternative readings are historical artifacts or strategic positions rather than genuinely live options. This determines whether reading_relations should be coexists_with or forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_status_mandatrophy, conceptual, 'Kernel reading co-possibility: can all three readings coexist or does one foreclose others').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comexp_theater_founding_era, commerce_clause_text__expansive_federal_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comexp_theater_post_civil_war, commerce_clause_text__expansive_federal_reading, theater_ratio, 1, 0.48).
narrative_ontology:measurement(comexp_theater_new_deal, commerce_clause_text__expansive_federal_reading, theater_ratio, 2, 0.62).
narrative_ontology:measurement(comexp_theater_contemporary, commerce_clause_text__expansive_federal_reading, theater_ratio, 3, 0.65).

% Extraction over time
narrative_ontology:measurement(comexp_extract_founding_era, commerce_clause_text__expansive_federal_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(comexp_extract_post_civil_war, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1, 0.38).
narrative_ontology:measurement(comexp_extract_new_deal, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(comexp_extract_contemporary, commerce_clause_text__expansive_federal_reading, base_extractiveness, 3, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comexp_suppression_founding_era, commerce_clause_text__expansive_federal_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(comexp_suppression_new_deal, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, dormant_commerce_clause).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, supremacy_clause_preemption).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, interstate_compacts_coordination).

% DUAL FORMULATION NOTE:
% The expansive Commerce Clause reading generates three downstream constraints: (1) dormant_commerce_clause — the reading's implication that state economic regulation affecting interstate commerce is presumptively invalid; (2) supremacy_clause_preemption — the reading's reliance on Supremacy Clause doctrines to enforce federal regulatory dominance; (3) interstate_compacts_coordination — the reading's effect on state negotiation and coordination mechanisms. Each has its own extractiveness value reflecting how the downstream mechanism operates, but all three are structurally dependent on the expansive reading's foundational scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__expansive_federal_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
