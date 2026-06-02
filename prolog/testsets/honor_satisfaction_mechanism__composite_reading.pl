% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism (Composite Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   The honor satisfaction mechanism represents a structural transformation
 *   in how disputes over personal dignity were resolved, shifting from
 *   violent retaliation to property-backed legal/contractual remedies. This
 *   composite reading models the mechanism as the simultaneous product of
 *   three distinct forces: state monopoly on violence (eliminating private
 *   dueling), bourgeois property-ownership norms (redefining honor as legally
 *   actionable damage to reputation/property), and insurance
 *   institutionalization (converting singular violent satisfaction into
 *   probabilistic actuarial settlement). The mechanism solves a genuine
 *   coordination problem — how to eliminate honor-driven violence while
 *   maintaining dispute resolution — but does so through category
 *   recategorization that excludes those without property access. The
 *   extractiveness has increased over the historical interval as insurance
 *   costs rose, property requirements became formalized, and traditional
 *   satisfaction practitioners were relegated to ceremonial roles. This
 *   reading differs from sibling readings in emphasizing that multiple
 *   independent mechanisms converged on the same extractive structure, rather
 *   than a single causal prime mover.
 *
 * KEY AGENTS:
 *   - State Apparatus: Institutional beneficiary (arbitrage) — consolidates monopoly on violence, channels disputes through property-indexed courts, collects fees and fines
 *   - Bourgeois Property Owners: Institutional beneficiary (arbitrage) — benefit from contract enforcement, property protection, and differential access to civil remedies
 *   - Insurance Vendors: Powerful beneficiary (mobile) — coordinate risk pooling while capturing premium spreads; core mechanism innovation
 *   - Lower Orders / Propertyless: Primary victims (powerless/trapped) — excluded from property-backed satisfaction channels, forced to choose between violent retaliation (criminal) or accepting humiliation (no remedy)
 *   - Honor Claimants Without Property: Secondary victims (powerless/constrained) — unable to pay insurance or access civil courts; category recategorization renders their honor unsatisfiable within legal framework
 *   - Traditional Satisfaction Practitioners: Vestigial beneficiaries turned piton (institutional/arbitrage) — maintain ceremonial honor discourse while losing functional authority; preserved through cultural inertia
 *   - Middling Merchant Class: Mixed position (moderate/constrained) — access to mechanism but at significant cost; coordination benefit real but extraction significant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.58).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.65).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism (Composite Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, 'c073cbec-698a-4b2f-9760-dc13460ea43c').
narrative_ontology:cs_kernel_codification('c073cbec-698a-4b2f-9760-dc13460ea43c', distributed).
narrative_ontology:cs_authority_grounding('c073cbec-698a-4b2f-9760-dc13460ea43c', lineage).
narrative_ontology:cs_interpretation_layer_present('c073cbec-698a-4b2f-9760-dc13460ea43c').
narrative_ontology:cs_reading_relation('c073cbec-698a-4b2f-9760-dc13460ea43c', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('c073cbec-698a-4b2f-9760-dc13460ea43c', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('c073cbec-698a-4b2f-9760-dc13460ea43c', foundational, multiple_independent_mechanisms_convergence).
narrative_ontology:cs_axiom_status(multiple_independent_mechanisms_convergence, holdable).
narrative_ontology:cs_axiom_grounding('c073cbec-698a-4b2f-9760-dc13460ea43c', multiple_independent_mechanisms_convergence, empirically_contingent).
narrative_ontology:cs_axiom('c073cbec-698a-4b2f-9760-dc13460ea43c', foundational, category_recategorization_as_extraction).
narrative_ontology:cs_axiom_status(category_recategorization_as_extraction, holdable).
narrative_ontology:cs_axiom_grounding('c073cbec-698a-4b2f-9760-dc13460ea43c', category_recategorization_as_extraction, deontological).
narrative_ontology:cs_reference_frame('c073cbec-698a-4b2f-9760-dc13460ea43c', traditional_satisfaction_authority).
narrative_ontology:cs_drift_state('c073cbec-698a-4b2f-9760-dc13460ea43c', contemporary_legal_modernity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c073cbec-698a-4b2f-9760-dc13460ea43c', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_property_owners).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_vendors).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, lower_orders).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, honor_claimants_without_property).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, traditional_satisfaction_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPOSSESSED HONOR CLAIMANT (SNARE) — Trapped between the old honor code (which requires violent satisfaction) and new property-based legal channels (which require capital they lack). No exit: cannot pursue traditional satisfaction without risking law, cannot access insurance or civil redress without property. Maximum extraction — bears full cost of mechanism transition.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__composite_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLING MERCHANT CLASS (TANGLED ROPE) — Constrained by insurance costs and property requirement barriers, but benefits from access to civil courts and contract enforcement. Genuine coordination function (property-backed dispute resolution) alongside extraction (insurance markup, legal fees, category shift that excludes non-property-holders). Mixed experience — some agency, some extraction.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__composite_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE APPARATUS (ROPE) — Experiences the composite mechanism as successful coordination: consolidating the monopoly on violence while channeling honor disputes through property-indexed civil courts. Extraction runs toward the state (fees, fines, control over violence), but the mechanism solves a genuine coordination problem — eliminating private dueling while maintaining social order. Net beneficiary with arbitrage options.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__composite_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BOURGEOIS PROPERTY OWNERS (ROPE) — Benefit from state monopoly on violence and property-backed legal channels. The mechanism coordinates their interests (contract enforcement, asset protection) while extracting value through insurance, legal fees, and differential access. Experiences the constraint as enabling their property regime — genuine coordination with embedded extraction.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__composite_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSURANCE INDUSTRY (ROPE) — Powerful actors with significant mobility. The insurance mechanism is coordination (pooling risk, replacing violent satisfaction with actuarial calculation) that they benefit from. Extraction toward them (premiums, underwriting power) but net coordination function visible from their position. Mobile enough to move between jurisdictions or business models if needed.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__composite_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL SATISFACTION PRACTITIONERS (PITON) — Dueling seconds, honor courts, satisfaction arbiters persist as vestigial institutions. They perform honor discourse but without functional authority — state monopoly on violence has displaced their role. Theater ratio high: performing honor satisfaction in ceremonial forms while actual dispute resolution flows through property-backed courts. Maintained by cultural inertia, not structural necessity.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__composite_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Observes the composite mechanism as simultaneous coordination and extraction: state monopoly solves the collective action problem of private violence; property indexing enables civil dispute resolution; insurance spreads risk. But the same mechanism excludes the propertyless, recategorizes honor claims (from personal satisfaction to contractual damages), and extracts value through fees and premiums. The coordination is genuine AND the extraction is structural.
constraint_indexing:constraint_classification(honor_satisfaction_mechanism__composite_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(honor_satisfaction_mechanism__composite_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honor_satisfaction_mechanism__composite_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, TR),
    TR >= 0.70.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mechanism extracts substantial value through multiple channels: state fees/fines, insurance premiums, legal costs, and implicit wealth transfer from propertyless to property-owners (the inability to pursue satisfaction depresses lower-class agency in disputes). But extractiveness is not maximal (snare-level 0.66+) because the mechanism does solve a genuine coordination problem — eliminating private dueling improved safety for all classes. The extractiveness measurement reflects the actual asymmetry: coordination benefit is real but unevenly distributed, and extraction is structural rather than parasitic. Suppression (0.65): High. Multiple suppressive mechanisms operate simultaneously: legal prohibition on private satisfaction (violence monopoly), property-requirement gates on civil remedies (economic exclusion), insurance cost barriers (actuarial gatekeeping), and category recategorization (honor claims no longer recognized as valid outside property framework). These are independent suppressive forces that reinforce each other. Theater ratio (0.61): Moderate-high. Traditional satisfaction practitioners perform honor discourse in ceremonial forms (seconds, formal challenges, satisfaction courts) while actual dispute resolution flows through civil courts. The theatrical component increased over the interval as traditional practices were displaced but continued as cultural performance rather than functional authority.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a complex perspectival gradient rather than binary divergence. The state and bourgeois property-owners experience Rope (genuine coordination solving violence problem). Insurance vendors experience Rope (successful risk pooling). The middling merchant class experiences Tangled Rope (access to coordination with significant extraction). The propertyless experience Snare (trapped, no exit). Traditional practitioners experience Piton (vestigial, maintaining form without function). The analytical observer must acknowledge that all six perspectives are structurally accurate from their positions — the mechanism IS coordination from the state's view AND extraction from the powerless's view. The perspectival gap reveals that the mechanism's legitimacy claim depends on beneficiary positioning: those for whom it coordinates (state/bourgeois) accept it as solution; those from whom it extracts (propertyless/traditional) experience it as imposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from beneficiary/victim status and exit options per the formula χ = ε × f(d) × σ(S). State apparatus: beneficiary + arbitrage exit → d ≈ 0.05-0.15 → low f(d) → negative effective extraction. Bourgeois property-owners: beneficiary + arbitrage exit → d ≈ 0.10-0.20 → low f(d) → moderate net coordination benefit. Powerless honor claimants: victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. Middling merchants: victim + constrained exit → d ≈ 0.65 → f(d) ≈ 1.00 → moderate extraction. The analytical observer derives d ≈ 0.72 from analytical power + analytical exit, producing f(d) ≈ 1.15 — high but not maximal extraction from the observer's position, reflecting their ability to see the structure without being trapped within it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the composite reading's Tangled Rope classification is structurally justified: the mechanism simultaneously coordinates (eliminates honor-driven violence, enables dispute resolution) and extracts (asymmetric distribution of coordination benefits, category recategorization excluding the propertyless). The six perspectives are not errors or disagreements — they are accurate structural observations from different positions. The state's Rope is correct given its beneficiary position. The snare experienced by the powerless is correct given their trapped position. The analytical observer's Tangled Rope is correct given their analytical position and the mechanism's genuine dual character. No single type is 'the truth' — the mechanism IS mixed coordination-extraction, and the perspectives reveal the asymmetry in how it functions for different agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_shift_voluntariness,
    'Was the recategorization of honor satisfaction from violent to financial-legal voluntary adoption of a superior system, or coercive normative replacement?',
    'Historical analysis of resistance, petitions, and enforcement patterns during transition. Survey of contemporaneous justifications for the shift (rational superiority vs. moral authority vs. legal decree).',
    'If voluntary: constraint is Rope (coordination). If coercive: constraint is Snare (extraction disguised as coordination). The reading''s core claim depends on whether agents experienced agency in the recategorization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_shift_voluntariness, conceptual, 'Whether the honor-to-financial recategorization was voluntary or coercive').

omega_variable(
    bourgeois_norm_causation,
    'Did bourgeois property-ownership norms drive the transition to property-backed honor satisfaction, or did the state apparatus impose the mechanism and bourgeois norms supplied cultural legitimacy post hoc?',
    'Chronological analysis: which emerged first — bourgeois property norms or state legal channels for satisfaction? Correspondence analysis: do bourgeois norm documents precede or follow legal apparatus expansion?',
    'If norms drove transition: mechanism emerges from genuine bourgeois interest (Rope for property-owners). If state imposed and norms followed: mechanism is extraction dressed in coordinating language (Snare for lower orders, Tangled Rope for middle class).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bourgeois_norm_causation, empirical, 'Whether bourgeois norms drove or legitimated the state''s mechanism').

omega_variable(
    insurance_accessibility_counterfactual,
    'If insurance had remained affordable and accessible across all classes, would the composite mechanism retain its extractive character, or would it become genuine Rope?',
    'Historical comparison with jurisdictions where insurance was subsidized or universal. Structural analysis: does extraction arise from insurance pricing, property-requirement gates, or from the mechanism itself?',
    'If extraction persists with accessible insurance: core mechanism is extractive (Snare). If extraction disappears: extraction arises from access barriers, not the mechanism (could be Rope with unjust distribution).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insurance_accessibility_counterfactual, conceptual, 'Whether extraction is intrinsic to the mechanism or arises from access barriers').

omega_variable(
    multiple_independent_pressures,
    'Did the composite mechanism result from coordinated policy design (single intentional innovation) or from independent pressures (state monopoly, bourgeois norms, insurance emergence) that converged structurally?',
    'Institutional history: trace origins of state monopoly on violence, bourgeois norm codification, and insurance institutionalization separately. Identify convergence points and whether they were explicitly coordinated.',
    'If coordinated: mechanism is a unified Tangled Rope design with coherent beneficiary. If converged independently: mechanism is a complex attractor where multiple independent dynamics create similar extraction profile — more brittle, more vulnerable to partial collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multiple_independent_pressures, empirical, 'Whether the composite mechanism resulted from design or convergence').

omega_variable(
    sibling_reading_kernel_ambiguity,
    'What is the contested kernel that the decline_reading and contraction_reading dispute with this composite_reading?',
    'Examine the shared historical phenomena all three readings address. Identify the specific factual or normative claim where they diverge. The kernel is the persisting commitment underlying the dispute.',
    'This omega documents the committer-frame structure for this reading. The kernel might be: ''the honor satisfaction mechanism''s essential function,'' ''the mechanism''s primary driver (state/bourgeois/insurance),'' or ''the mechanism''s irreversibility/contingency.'' Resolving the omega clarifies why sibling readings coexist rather than foreclose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_ambiguity, conceptual, 'Identity and structure of the contested kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_composite_tr_t0, honor_satisfaction_mechanism__composite_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(honor_composite_tr_t10, honor_satisfaction_mechanism__composite_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(honor_composite_tr_t20, honor_satisfaction_mechanism__composite_reading, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(honor_composite_be_t0, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(honor_composite_be_t10, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(honor_composite_be_t20, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(honor_composite_su_t0, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(honor_composite_su_t10, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(honor_composite_su_t20, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, dueling_prohibition_state_monopoly).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, property_indexed_legal_standing).

% DUAL FORMULATION NOTE:
% The honor satisfaction mechanism decomposes into three structurally distinct constraint stories: decline_reading (vestigial ceremonial practice), contraction_reading (category recategorization narrowing scope), and composite_reading (simultaneous operation of state/bourgeois/insurance mechanisms). Each has its own ε value and perspectives. The composite_reading is the analytical integration showing how independent forces converged. The decline and contraction readings show specific mechanistic emphases. All three link the same historical transformation but with different causal narratives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
