% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocity Principle: Pharaoh's Mutual Obligations
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   The Ma'at reciprocity principle instantiates one reading of the contested
 *   kernel governing pharaonic legitimacy and cosmic order. Under this
 *   reading, the Pharaoh is subject to mutual obligations: the commoner
 *   provides labor and taxation; the Pharaoh provides justice, agricultural
 *   stability through Nile management, and resource redistribution. The
 *   constraint is neither pure coordination (Rope) nor pure extraction
 *   (Snare) but a hybrid (Tangled Rope) where legitimate extraction is
 *   bounded by the obligation to maintain cosmic balance. This reading
 *   emphasizes that failed reciprocity justifies resistance or withdrawal of
 *   support — it is the mechanism by which the constraint operates as an
 *   actual ceiling on extraction rather than merely a legitimating fiction.
 *   The reciprocity framing enables extraction by grounding it in cosmic
 *   necessity rather than power, but the mutual obligation structure creates
 *   measurable constraints on how much can be extracted before legitimacy
 *   collapses.
 *
 * KEY AGENTS:
 *   - Agricultural Commoner: Primary victim (powerless/trapped) — bears taxation and corvée labor; lacks exit capacity; reciprocal obligations exist in theory but enforcement is one-directional
 *   - Pharaonic Institution: Primary beneficiary (institutional/arbitrage) — extracts through taxation and labor conscription; legitimacy constrained by obligation to provide justice and stability; arbitrage derives from ability to invoke or suspend reciprocity framing
 *   - Priestly Administrative Class: Secondary beneficiary (moderate/constrained) — benefits from temple lands and redistribution; provides essential coordination (record-keeping, legitimacy certification); constrained by knowledge monopoly and institutional continuity requirements
 *   - Temples and Elite Households: Organized beneficiary (organized/constrained) — institutional land-holders with tributary obligations; coordinate risk-pooling and resource allocation; experience mixed extraction and benefit from pharaonic protection
 *   - Historical Documentation System: Piton perspective (analytical/analytical) — reciprocity preserved in inscription and teaching texts; enforcement mechanisms erode over time; theatrical emphasis increases as actual capacity declines
 *   - Comparative Political Theorist: Analytical perspective (analytical/analytical) — views Ma'at reciprocity as genuine coordination mechanism legitimizing extraction through mutual obligation rhetoric
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.38).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.48).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocity Principle: Pharaoh's Mutual Obligations").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, 'maat-reciprocity-01').
narrative_ontology:cs_kernel_codification('maat-reciprocity-01', formalized).
narrative_ontology:cs_authority_grounding('maat-reciprocity-01', lineage).
narrative_ontology:cs_interpretation_layer_present('maat-reciprocity-01').
narrative_ontology:cs_reading_relation('maat-reciprocity-01', maat_order_principle__divine_mandate_reading, influences).
narrative_ontology:cs_reading_relation('maat-reciprocity-01', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('maat-reciprocity-01', foundational, pharaoh_subject_to_reciprocal_constraint).
narrative_ontology:cs_axiom_status(pharaoh_subject_to_reciprocal_constraint, holdable).
narrative_ontology:cs_axiom_grounding('maat-reciprocity-01', pharaoh_subject_to_reciprocal_constraint, deontological).
narrative_ontology:cs_axiom('maat-reciprocity-01', foundational, failed_reciprocity_justifies_resistance).
narrative_ontology:cs_axiom_status(failed_reciprocity_justifies_resistance, holdable).
narrative_ontology:cs_axiom_grounding('maat-reciprocity-01', failed_reciprocity_justifies_resistance, deontological).
narrative_ontology:cs_reference_frame('maat-reciprocity-01', pharaonic_reciprocal_obligation_framework).
narrative_ontology:cs_drift_state('maat-reciprocity-01', late_kingdom_decline, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('maat-reciprocity-01', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaonic_legitimacy).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, elite_institutional_stability).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, commoner_resource_access).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, agricultural_surplus_redistribution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AGRICULTURAL COMMONER (SNARE) — Bound by taxation obligations and corvée labor requirements. Exit is impossible (escape to foreign lands means death or enslavement). The reciprocity norm exists in theory but enforcement of Pharaoh's counter-obligations is one-directional. Commoner bears suppression of resource alternatives and experiences high extraction with minimal exit capacity.
constraint_indexing:constraint_classification(maat_order_principle__reciprocity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIESTLY ADMINISTRATIVE CLASS (TANGLED ROPE) — Middle-tier beneficiaries of Ma'at system. Receive sustenance from temple lands and tax redistribution; provide crucial coordination function (record-keeping, liturgical maintenance, legitimacy certification). Subject to Pharaoh's authority but possess institutional continuity and knowledge monopolies that constrain Pharaoh's extraction capacity. Exit is costly (loss of office, land, status) but not impossible.
constraint_indexing:constraint_classification(maat_order_principle__reciprocity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARAONIC INSTITUTION (ROPE) — Primary beneficiary under reciprocity reading. Extractive capacity is constrained by the norm itself: legitimate taxation justified as counter-obligation for justice and stability provision. Arbitrage capacity derives from ability to invoke or suspend the reciprocity frame depending on institutional need. Coordination function: legitimize extraction through reciprocity language rather than coercive domination.
constraint_indexing:constraint_classification(maat_order_principle__reciprocity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TEMPLES AND ELITE HOUSEHOLDS (TANGLED ROPE) — Organized institutional agents with land holdings and tributary rights. Benefit from Ma'at stability and pharaonic protection against external threats. Constrained by tributary obligations and temple contributions to state projects. Possess coordination function through cooperative grain storage and resource allocation during Nile failures. Extraction runs both directions.
constraint_indexing:constraint_classification(maat_order_principle__reciprocity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL DOCUMENTATION (PITON) — The reciprocity principle is preserved in monumental inscription and priestly teaching texts but archaeological evidence shows enforcement mechanisms eroded during late Kingdom periods. Theater increases (ritual emphasis) as actual redistribution capacity declines. The constraint persists through institutional inertia and theological maintenance rather than active enforcement of mutual obligations.
constraint_indexing:constraint_classification(maat_order_principle__reciprocity_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(regional))).

% PERSPECTIVE 6: COMPARATIVE THEORIST (TANGLED ROPE) — Viewing Ma'at reciprocity as a genuine coordination mechanism that legitimizes extraction through mutual obligation rhetoric. The constraint exhibits authentic coordination function (Nile risk pooling, monument building coordination) alongside asymmetric extraction (commoner taxation without enforceable counter-obligations). The reciprocity framing enables high effective extraction by grounding it in cosmic justice rather than coercion.
constraint_indexing:constraint_classification(maat_order_principle__reciprocity_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maat_order_principle__reciprocity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maat_order_principle__reciprocity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, TR),
    TR >= 0.70.

:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reciprocity reading grounds extraction in mutual obligation rather than unilateral power, creating a legitimate but bounded extraction mechanism. The Pharaoh extracts substantial taxation and labor, but the extraction is framed as counter-obligation for justice and stability provision. The measurement trajectory shows slight increase (0.32→0.41) as administrative capacity grows, suggesting creeping extraction as enforcement of reciprocal obligations weakens. Theater ratio (0.35): Low-moderate. The reciprocity principle is preserved in functional administrative systems (record-keeping, resource redistribution mechanisms, Nile-risk pooling) rather than purely performative ritual. The principle motivates genuine coordination infrastructure rather than ornamental theater, but ceremonial emphasis increases as substantive enforcement declines. Suppression (0.48): Moderate-high. Commoners face substantial barriers to exit (no foreign refuge with security, economic dependency on Nile agriculture, legal restrictions on movement) and limited organizational capacity to resist. However, suppression is not total — periodic famine resistance, labor flight during high taxation, and unrest demonstrate that the constraint operates through partial suppression rather than absolute coercion. The trajectory shows slight increase, reflecting administrative tightening and formalization of corvée requirements over time.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits significant perspectival divergence across power positions. The commoner experiences the constraint as a Snare (trapped, high extraction, minimal benefits). The priestly administrative class experiences Tangled Rope (mixed benefits from redistribution and coordination roles, constrained by obligation to maintain legitimacy). The Pharaonic institution experiences Rope (net beneficiary, legitimate extraction justified through reciprocity language). Organized temple and elite households experience Tangled Rope (asymmetric tributary relationships with mutual risk-pooling). The piton perspective reveals that enforcement mechanisms degrade over time, increasing theater as substantive reciprocal obligations fail. The analytical perspective recognizes the genuine coordination function (Nile-risk pooling, monument-building mobilization) embedded within extraction. The perspectival gap reveals the reciprocity reading's core mechanism: legitimizing extraction through obligation language creates higher extraction ceilings than coercion alone would achieve, because commoners coordinate their own taxation when they believe reciprocal benefits are forthcoming.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural position relative to the reciprocity obligation flow. The Pharaonic institution (beneficiary + arbitrage) experiences low d, producing negative or minimal χ — they are net extractors. The commoner (victim + trapped) experiences high d (~0.95), producing maximal experienced χ despite moderate base extractiveness, because they have zero exit capacity. The priestly administrative class (mixed beneficiary/coordinator + constrained) experiences moderate d (~0.50), producing balanced χ — they are intermediate agents managing the redistribution mechanism. The organized temples (mixed beneficiary/victim + constrained) experience moderate-high d (~0.60), reflecting that the constraint imposes genuine tributary obligations despite providing protection and coordination benefits. The analytical observer (analytical context) experiences d ~0.72, recognizing the full structure without occupying a extractive or beneficiary position. The reciprocity reading's key feature is that the beneficiary's extraction is constrained not by commoner power but by the requirement to fulfill reciprocal obligations — this is captured through the bounded f(d) function rather than zero d.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_asymmetry_ambiguity,
    'Were Pharaoh''s reciprocal obligations (justice provision, resource redistribution) genuinely enforced as constraints on extraction, or was the reciprocity norm primarily legitimating rhetoric?',
    'Comparative analysis of monumental inscriptions describing obligations vs. archaeological evidence of famine response, tax relief, and redistributive capacity during Nile low periods. Cross-check against administrative papyri showing actual resource flows.',
    'If genuinely enforced: reciprocal obligations functioned as real constraint ceiling on extraction, classification remains Tangled Rope. If rhetorical: reciprocity was coordination theater masking unilateral extraction, reclassifies to Snare across more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_ambiguity, empirical, 'Whether reciprocal obligations were structurally enforced or primarily legitimating rhetoric').

omega_variable(
    commoner_exit_capacity_variance,
    'Did commoner exit capacity (fleeing to Nubia, desert refuge, Memphis refugium) vary significantly by region and historical period, affecting local extraction ceilings?',
    'Regional demographic analysis; settlement pattern changes during periods of high extraction or famine; evidence of labor flight and resettlement.',
    'If significant variance: classification varies by region and period; some areas approach Rope (mobile commoners as coordinators), others remain Snare (trapped populations). If uniform low exit: uniform Snare classification across regions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commoner_exit_capacity_variance, empirical, 'Regional and temporal variation in commoner exit capacity').

omega_variable(
    reading_contest_among_egyptologists,
    'Which sibling reading (divine_mandate_reading or distributed_maintenance_reading) best fits the textual and archaeological record for THIS reciprocity reading?',
    'Systematic review of priestly teachings (Instructions of Ptahhotep, Instruction of Amenemope), monumental inscriptions, and administrative records to identify which reading''s axioms are most frequently instantiated in actual state practice.',
    'If divine_mandate dominates the record: the reciprocity reading''s constraint ceiling is illusory — extraction is justified by separate mandate logic, not mutual obligation. If distributed_maintenance dominates: reciprocity reading may be downstream of a decentralized legitimacy system that limits centralization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_among_egyptologists, conceptual, 'Which sibling reading best fits the historical record for Ma''at principle').

omega_variable(
    cosmic_balance_mechanism_obscurity,
    'Is the cosmic balance mechanism (Ma''at as universal order requiring Pharaoh maintenance) a genuine structural constraint or theological metaphor obscuring political extraction?',
    'Linguistic and semantic analysis of Ma''at terminology across periods; correlation between cosmic-balance rhetoric and actual state capacity constraints; comparative theology of ma''at vs. other Near Eastern cosmic order concepts.',
    'If genuine structural constraint: legitimacy genuinely depends on demonstrable justice and stability provision, floor on extraction. If metaphorical: cosmic-balance language is cover story, extraction ceiling is unconstrained by legitimacy requirement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_balance_mechanism_obscurity, conceptual, 'Whether cosmic balance mechanism is structural constraint or theological metaphor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(maat_tr_t1, maat_order_principle__reciprocity_reading, theater_ratio, 1, 0.32).
narrative_ontology:measurement(maat_tr_t2, maat_order_principle__reciprocity_reading, theater_ratio, 2, 0.35).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(maat_be_t1, maat_order_principle__reciprocity_reading, base_extractiveness, 1, 0.38).
narrative_ontology:measurement(maat_be_t2, maat_order_principle__reciprocity_reading, base_extractiveness, 2, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(maat_su_t1, maat_order_principle__reciprocity_reading, suppression_requirement, 1, 0.46).
narrative_ontology:measurement(maat_su_t2, maat_order_principle__reciprocity_reading, suppression_requirement, 2, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, resource_allocation).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, nile_inundation_coordination_mechanism).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, pharaonic_monument_mobilization).

% DUAL FORMULATION NOTE:
% The Ma'at order principle kernel decomposes into three structurally distinct constraint readings with different ε values. The reciprocity reading (this constraint) emphasizes bounded extraction through mutual obligation (ε=0.38). The divine mandate reading emphasizes unconstrained extraction justified by separate mandate (ε ≈ 0.55+, Snare). The distributed maintenance reading emphasizes decentralized coordination with limited pharaonic extraction (ε ≈ 0.25, Rope). Each reading produces different classifications and different victim/beneficiary structures. The reciprocity reading is upstream of nile_inundation_coordination_mechanism and pharaonic_monument_mobilization, which are coordination mechanisms justified through reciprocity obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
