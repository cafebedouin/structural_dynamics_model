% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Law as Cognatic Succession Reversion (Non-Frankish Territories)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   The Salic Law—the Frankish prohibition on female succession codified in
 *   Lex Salica and invoked to exclude women from dynastic
 *   inheritance—presents a classic case of territorial jurisdiction collapse
 *   masquerading as natural law. This constraint story models ONE reading of
 *   the contested kernel: the cognatic reversion reading, which asserts that
 *   Salic Law was never properly binding outside the Frankish heartland and
 *   that non-Frankish territories retain the right to cognatic succession
 *   (eldest child regardless of sex). This reading competes with the
 *   immutable mandate reading (which treats Salic Law as a universal
 *   principle of legitimate dynasty) and the sovereign override reading
 *   (which permits pragmatic exceptions but preserves the underlying Salic
 *   frame). The cognatic reversion reading differs structurally: it denies
 *   Salic's universal applicability and roots legitimacy in territorial
 *   integrity and indigenous succession customs, not in Frankish dynastic
 *   supremacy. Base extractiveness (0.38) reflects that Salic enforcement
 *   does provide genuine coordination benefit (reduces succession dispute
 *   risk) but also imposes real extraction (loss of local self-determination,
 *   political control by Frankish center, exclusion of capable female heirs).
 *   Theater ratio (0.65) captures that ecclesiastical legitimators
 *   increasingly used performative proclamations ('legitimate by papal
 *   dispensation') to accommodate exceptions while preserving the illusion of
 *   Salic purity. By the later medieval period, the theater ratio is high
 *   because the rule persists largely through institutional inertia while
 *   actual practice violates it regularly.
 *
 * KEY AGENTS:
 *   - Disinherited Daughters: Primary victims (powerless/trapped) — legally barred from succession under agnatic enforcement; no exit option within a single lifetime
 *   - Cognatic Succession Coalition: Organized beneficiaries (organized/constrained) — non-Frankish territories and female-line dynasties asserting local succession norms; constrained by Frankish military but retain practical agency
 *   - Frankish Center (Agnatic Enforcer): Primary beneficiary (institutional/arbitrage) — centralizes succession rules and captures value from subordinate territories' compliance
 *   - Non-Frankish Territory Rulers: Secondary victims (moderate/constrained) — retain local authority but face extraction via Frankish interference in succession; can theoretically defect but at high cost
 *   - Ecclesiastical Legitimators: Institutional actor (institutional/arbitrage) — originally enforced Salic purity but increasingly retreat to performative accommodation by later medieval period
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees territorial integrity principle underlying the cognatic reading, but risks naturalizing what is actually a political choice about whose law applies where
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.38).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.48).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Law as Cognatic Succession Reversion (Non-Frankish Territories)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '4d5c9899-f847-4744-9f33-b0cdfeab7b5c').
narrative_ontology:cs_kernel_codification('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', fixed_text).
narrative_ontology:cs_authority_grounding('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', lineage).
narrative_ontology:cs_interpretation_layer_present('4d5c9899-f847-4744-9f33-b0cdfeab7b5c').
narrative_ontology:cs_reading_relation('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', foundational, territorial_jurisdiction_bounded_salic).
narrative_ontology:cs_axiom_status(territorial_jurisdiction_bounded_salic, holdable).
narrative_ontology:cs_axiom_grounding('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', territorial_jurisdiction_bounded_salic, deontological).
narrative_ontology:cs_axiom('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', foundational, indigenous_succession_customs_sovereignty_preserving).
narrative_ontology:cs_axiom_status(indigenous_succession_customs_sovereignty_preserving, holdable).
narrative_ontology:cs_axiom_grounding('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', indigenous_succession_customs_sovereignty_preserving, conventional).
narrative_ontology:cs_reference_frame('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', pre_frankish_succession_customs).
narrative_ontology:cs_drift_state('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', late_medieval_ecclesiastical_relaxation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4d5c9899-f847-4744-9f33-b0cdfeab7b5c', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, cognatic_succession_advocates).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, non_frankish_territorial_rulers).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, agnatic_purists).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, frankish_dynastic_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISINHERITED DAUGHTER (SNARE) — Under agnatic-strict enforcement, female heirs are legally barred from succession regardless of birth order or capability. No exit option: the exclusion is written into fundamental law. Full extraction — the constraint extracts political authority and property rights with no coordination benefit. Trapped at biographical horizon because succession rules are unchangeable within a single lifetime.
constraint_indexing:constraint_classification(salic_prohibition__cognatic_reversion_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-FRANKISH TERRITORY RULER (TANGLED ROPE) — Moderately constrained by Frankish dynastic claims but retains practical authority within their own borders. Salic Law offers coordination benefit (clarity about succession preventing civil war) but also imposes extraction (loss of local succession flexibility, external interference from Frankish center). Exit cost is high (risking Frankish military intervention or dynastic rejection) but theoretically possible at the generational level. Mixed coordination and extraction.
constraint_indexing:constraint_classification(salic_prohibition__cognatic_reversion_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRANKISH CENTER / AGNATIC ENFORCER (ROPE) — Experiences Salic Law as coordination mechanism: centralizing succession rules across diverse territories creates predictability and reduces dynastic civil wars. Benefits from enforcement through reduced military costs and stable tributary relationships. Arbitrage exit (can enforce or relax rules based on strategic need). Extraction runs toward the institutional beneficiary — the center captures value from subordinate territories' compliance.
constraint_indexing:constraint_classification(salic_prohibition__cognatic_reversion_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COGNATIC SUCCESSION COALITION (TANGLED ROPE) — Organized non-Frankish territories and female-line dynasties seeking to restore local succession norms. Constrained by Frankish military and legitimacy apparatus but organize collectively to assert cognatic primogeniture where Frankish enforcement is weak. Genuine coordination function (stabilizing inheritance predictably within non-Frankish traditions) combined with extraction (loss of Frankish protection/legitimacy when they defect). The coalition has agency — capable of negotiating or resisting — but faces high exit cost.
constraint_indexing:constraint_classification(salic_prohibition__cognatic_reversion_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ECCLESIASTICAL LEGITIMATORS (PITON) — Church authority (canon law, papal doctrine) originally used to enforce agnatic purity as 'natural law of inheritance' and 'divine order.' By the cognatic reading's own timeframe, ecclesiastical authority has largely abandoned strict Salic enforcement in favor of pragmatic local accommodation. The legitimating theater persists (declarations of legitimacy for female-line succession) but the enforcement machinery has atrophied. Piton classification: high theater_ratio (performative proclamations of legitimacy) combined with low actual power to enforce strict rules.
constraint_indexing:constraint_classification(salic_prohibition__cognatic_reversion_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TERRITORIAL INTEGRITY VIEW (ROPE) — From a civilizational/universal perspective, the cognatic reversion reading instantiates a general principle: dynastic law should be subordinate to territorial integrity and local custom. Salic Law enforcement outside Frankish core territory violates this principle (local populations lose self-determination). The constraint appears as pure coordination — the mechanism for resolving succession disputes — without the extraction component. However, the structural data reveals this is incomplete: the beneficiary/victim declarations show asymmetric extraction hidden behind the coordination frame.
constraint_indexing:constraint_classification(salic_prohibition__cognatic_reversion_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(salic_prohibition__cognatic_reversion_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(salic_prohibition__cognatic_reversion_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, TR),
    TR >= 0.70.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The cognatic reading frames Salic Law as territorially bounded—a Frankish custom imposed on non-Frankish populations, not a universal principle. The extraction magnitude reflects both genuine coordination benefit (succession clarity) and genuine harm (loss of local self-determination, exclusion of female heirs). The trajectory from 0.22 to 0.38 over the interval captures intensifying extraction as Frankish center consolidates claims to enforce Salic across wider territories. Suppression (0.48): Moderate. Barriers to resistance include Frankish military power, ecclesiastical legitimacy claims, and the absence of written competing legal texts in some non-Frankish territories. But suppression is not total: non-Frankish rulers regularly assert local law, some territories successfully practice cognatic succession despite formal Salic claims, and ecclesiastical authority increasingly permits exceptions. Theater ratio (0.65): Moderately high. By the later medieval period, ecclesiastical legitimators issue dispensations and creative interpretations (e.g., 'this female successor is legitimate because her father was legitimized by prior papal act') that preserve the appearance of Salic purity while enabling female succession in practice. The rising trajectory (0.48 → 0.65) reflects intensifying performativity as the gap between rule and practice widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a perspectival gap between the beneficiary and victim perspectives, revealing how the same structural fact (Salic Law exclusion of female successors) registers differently depending on structural position. The Frankish center sees pure coordination (Rope): succession rules prevent civil war. The disinherited daughter sees pure extraction (Snare): she is barred with no exit option. The non-Frankish ruler sees a mixed constraint (Tangled Rope): coordination benefit (predictable rules) combined with extraction (loss of local self-determination). The organized coalition sees a constraint with agency (Tangled Rope): they can negotiate or assert local law, though at cost. The ecclesiastical legitimators see a degraded institution (Piton): the rule persists through momentum while practice repeatedly violates it. The civilizational analytical observer risks seeing pure coordination (Rope) but the structural data reveals extraction hidden behind the coordination frame.
 *
 * DIRECTIONALITY LOGIC:
 *   The cognatic reading's directionality is determined by assigning agents to structural roles and computing d from their exit options and beneficiary/victim status. Frankish institutional beneficiaries with arbitrage exit (can enforce or relax rules strategically) experience low d → low χ. Disinherited daughters with trapped exit experience high d → high χ. Non-Frankish rulers with constrained exit (theoretically can resist, practically face high cost) experience moderate-high d. The organized coalition perspective shows that when powerless agents organize, the power atom upgrades, changing d and potentially changing classification. This is why the same constraint appears as Snare from the individual disinherited daughter's perspective but Tangled Rope from the organized coalition perspective — the agent's power level and exit options differ between these two perspectives, which the engine derives from context and beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The cognatic reading resolves mandatrophy by asserting that Salic Law is coordinate within Frankish core territory (genuine collective-action solution to succession risk) but extractive when imposed on non-Frankish populations (violation of territorial self-determination). The constraint is tangled_rope because it combines both functions: the coordination benefit (succession clarity) is real, and the extraction (political control by center) is real, and they are bound together in a single mechanism. Rejecting the extraction would require rejecting the coordination benefit — there is no way to get the succession clarity without accepting Frankish interference.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    frankish_jurisdiction_boundary_determination,
    'What geographic and institutional boundaries define ''Frankish territory'' such that Salic Law is binding within but inapplicable without?',
    'Historical analysis of Frankish settlement patterns, military control, dynastic marriage claims, and ecclesiastical jurisdiction; identification of legal documents asserting Salic applicability claims vs. local-law assertions',
    'If boundary is tight (ethnic Frankish core only): Salic Law is easily reframed as local Frankish custom with no universal force — cognatic reading wins immediately. If boundary is expansive (all territories under Frankish dynastic claim): Salic Law''s extractive force extends further and cognatic reversion requires explicit rejection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(frankish_jurisdiction_boundary_determination, empirical, 'Definition of Frankish jurisdiction boundary for Salic applicability').

omega_variable(
    agnatic_vs_cognatic_stability_empirical,
    'Do cognatic succession systems (eldest child regardless of sex) produce more or fewer dynastic civil wars than strict agnatic systems in comparable historical contexts?',
    'Comparative analysis of succession disputes in cognatic vs agnatic European monarchies (e.g., Castile-Aragon cognatic vs Capetian agnatic); correlation between succession rule type and civil war incidence',
    'If cognatic produces fewer wars: coordination benefit of agnatic enforcement is overstated — extraction is not compensated by stability gain. If agnatic produces fewer wars: extraction is justified as coordination cost for genuine stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agnatic_vs_cognatic_stability_empirical, empirical, 'Comparative stability of cognatic vs agnatic succession systems').

omega_variable(
    ecclesiastical_legitimacy_collapse_timeline,
    'When did Catholic canonical doctrine formally abandon the natural-law framing of Salic restriction and permit female succession as legitimate?',
    'Chronological analysis of papal letters, council decrees, and canonical commentaries; identification of the moment when ''Salic exception'' language shifts from theological justification to pragmatic accommodation',
    'Early collapse (13th century): ecclesiastical authority withdrawn before Salic enforcement was widely consolidated — the constraint was never stable. Late collapse (16th+ century): ecclesiastical legitimacy sustained Salic enforceability for centuries; its withdrawal signals major structural shift in constraint dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecclesiastical_legitimacy_collapse_timeline, empirical, 'Timeline of ecclesiastical legitimacy withdrawal from Salic doctrine').

omega_variable(
    cognatic_reading_vs_historical_practice_gap,
    'Is the cognatic reading a recovery of pre-Frankish indigenous succession norms, or a post-hoc rationalization of pragmatic female succession driven by military/economic necessity (heirless dynasties, wealthy female rulers)?',
    'Textual analysis of non-Frankish legal codes predating Frankish conquest; comparison of female succession cases — correlate female succession occurrence with (a) pre-existing local law, (b) absence of male heirs, (c) female ruler military/economic power',
    'If recovery of indigenous norms: cognatic reading is rooted in legitimate local tradition — extraction by Salic is violation of pre-existing right. If pragmatic rationalization: cognatic reading is constructed post-hoc to justify exceptions — less morally weighty as a principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognatic_reading_vs_historical_practice_gap, conceptual, 'Whether cognatic reading recovers indigenous or rationalizes pragmatic succession').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_cog_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(salic_cog_tr_t3, salic_prohibition__cognatic_reversion_reading, theater_ratio, 3, 0.62).
narrative_ontology:measurement(salic_cog_tr_t6, salic_prohibition__cognatic_reversion_reading, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(salic_cog_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(salic_cog_be_t3, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(salic_cog_be_t6, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, female_succession_ecclesiastical_legitimacy).

% DUAL FORMULATION NOTE:
% The three readings of the salic_prohibition kernel are structurally linked: they share the same territorial boundary question but arrive at different answers about what law applies where. Each reading produces a distinct constraint with distinct extractiveness values reflecting different assumptions about Frankish jurisdiction scope. Cognatic reversion (this story) assumes Salic is Frankish-bounded; immutable mandate assumes Salic is universal; sovereign override assumes Salic is default but overridable. These are not three measures of one constraint — they are three constraints instantiated by three competing readings of a single kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
