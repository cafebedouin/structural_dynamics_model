% ============================================================================
% CONSTRAINT STORY: zombie_reasoning_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zombie_reasoning_2026, []).

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
 *   constraint_id: zombie_reasoning_2026
 *   human_readable: The Zombie Reasoning Epistemic Snare
 *   domain: philosophical/technological
 *
 * SUMMARY:
 *   The zombie reasoning epistemic snare describes a structural extraction
 *   mechanism embedded in AI discourse terminology. Rebecca Lowe identifies
 *   that terms like 'reasoning,' 'evaluating,' and 'selecting' are used as
 *   zombie concepts — they carry linguistic prestige and philosophical weight
 *   but lack rigorous definition in the AI context. This creates an
 *   asymmetric extraction: vendors benefit from ambiguity (they can claim
 *   capabilities without precision), while practitioners, policy makers, and
 *   the epistemic commons bear costs (they must operate with corrupted
 *   terminology). The snare is maintained through suppression: correcting
 *   terminology would require vendors to downgrade capability claims, which
 *   contradicts market incentives. The constraint is not a natural feature of
 *   language development but an active extraction mechanism.
 *
 * KEY AGENTS:
 *   - AI Vendors & Capability Claimants: Primary beneficiaries (institutional/arbitrage) — exploit ambiguity to amplify market signals and defer regulatory scrutiny
 *   - Field Practitioners & Researchers: Primary victims (powerless/trapped) — forced to operate with imprecise terminology; cannot exit without abandoning the field
 *   - Policy Makers & Regulators: Secondary victims (moderate/constrained) — must make capacity assessments and safety decisions based on deliberately ambiguous terminology
 *   - Epistemic Clarity & Commons: Collective victim (powerless/trapped) — abstract but real structural damage from accumulated terminology corruption
 *   - Philosophical Critics (Lowe, etc.): Organized observers (organized/constrained) — see the full structure but lack enforcement power; face suppression when advocating for precision
 *   - Traditional Philosophy of Mind: Institutional theater (institutional/arbitrage) — machinery persists but gutted of function; terminology borrowed but concepts ignored
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zombie_reasoning_2026, 0.58).
domain_priors:suppression_score(zombie_reasoning_2026, 0.72).
domain_priors:theater_ratio(zombie_reasoning_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zombie_reasoning_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(zombie_reasoning_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(zombie_reasoning_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zombie_reasoning_2026, snare).
narrative_ontology:human_readable(zombie_reasoning_2026, "The Zombie Reasoning Epistemic Snare").
narrative_ontology:topic_domain(zombie_reasoning_2026, "philosophical/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zombie_reasoning_2026, imprecise_ai_vendors).
narrative_ontology:constraint_beneficiary(zombie_reasoning_2026, hype_amplifiers).
narrative_ontology:constraint_victim(zombie_reasoning_2026, epistemic_clarity).
narrative_ontology:constraint_victim(zombie_reasoning_2026, field_practitioners).
narrative_ontology:constraint_victim(zombie_reasoning_2026, policy_makers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD PRACTITIONERS (SNARE) — Research communities seeking precise terminology face cumulative epistemic contamination. Each use of 'reasoning' without rigorous definition corrupts downstream literature. Practitioners cannot exit without abandoning their field; they are trapped consuming ambiguous terminology. Maximum extraction: their cognitive labor subsidizes vendor narratives.
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POLICY MAKERS (SNARE) — Regulatory bodies must make capacity assessments and safety decisions based on AI capabilities. Zombie terminology creates decision paralysis: is the system 'reasoning' or executing pattern matching? Policy makers cannot escape the extraction — their regulatory legitimacy depends on understanding capabilities, but the terminology is deliberately imprecise. They bear the cost of confusion; vendors benefit from regulatory uncertainty.
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: AI VENDORS (ROPE) — Extractors experience the constraint as pure coordination: imprecise terminology creates a shared language pool that amplifies market signals. 'Reasoning' triggers investment and regulatory leniency. Vendors benefit from the ambiguity; they can arbitrage between multiple interpretations. Zero suppression from their perspective — the constraint is entirely beneficial.
constraint_indexing:constraint_classification(zombie_reasoning_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHILOSOPHICAL CRITICS (TANGLED ROPE) — Organized skeptics (Lowe et al.) see both coordination and asymmetric extraction. The constraint solves a real coordination problem: AI systems do exhibit complex behavioral outputs that require some shared vocabulary. But the terminology asymmetry means critics must spend enormous cognitive labor arguing for precision while vendors spend minimal effort marketing ambiguity. Constrained exit: exiting means surrendering the epistemic commons to increasingly corrupted language.
constraint_indexing:constraint_classification(zombie_reasoning_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PHILOSOPHY OF MIND DISCIPLINE (PITON) — The philosophical machinery (zombie arguments, thought experiments, phenomenal consciousness) persists as ritual rather than as functioning epistemic practice in AI contexts. Few AI engineers engage with actual philosophical arguments; they use zombie terminology theatrically without understanding the underlying conceptual apparatus. Philosophy's methodology has been gutted (theater_ratio >> 0.5) while the terminology persists. Maintained through inertia rather than function.
constraint_indexing:constraint_classification(zombie_reasoning_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, some ambiguity in emerging terminology is a natural property of language development. New technologies require new concepts; transient imprecision is the cost of conceptual innovation. The constraint appears immutable: you cannot forbid metaphorical language use. However, the structural data contradicts the mountain classification — suppression is high and deliberately maintained, indicating active enforcement of ambiguity rather than natural emergence.
constraint_indexing:constraint_classification(zombie_reasoning_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zombie_reasoning_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(zombie_reasoning_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zombie_reasoning_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(zombie_reasoning_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(zombie_reasoning_2026, TR),
    TR >= 0.70.

:- end_tests(zombie_reasoning_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The constraint extracts meaningful value from practitioners and policy makers. Vendors capture investment, regulatory leniency, and hire ratios by claiming 'reasoning' capabilities without precision. The extraction is measurable but not total — some transparency efforts and fact-checking constrain it. The trajectory over 6 periods (0.25 → 0.42 → 0.58) reflects increasing market penetration of capability claims and accumulating epistemic costs. Suppression (0.72): High. Barriers to terminology correction include vendor incentive structures, investor expectations, media hype cycles, and the cognitive difficulty of precise language. Vendors actively resist correction. The constraint is enforced not by formal policy but by market structure and competitive dynamics — vendors who adopt precise terminology lose market position to competitors using ambiguous claims. Theater ratio (0.68): High and rising. The philosophical machinery (zombie arguments, thought experiments) appears in AI discourse but functions theatrically rather than epistemically. Few engineers engage with actual philosophical reasoning; terminology is borrowed for prestige. The rising trajectory (0.35 → 0.52 → 0.68) reflects increasing decoupling between the philosophical scaffolding and actual AI research practice.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (vendor) and victim (practitioner) perspectives are radically divergent. Vendors see the constraint as enabling coordination — they are solving the problem of how to communicate complex AI behavior to investors and regulators. Practitioners see extraction — they must absorb terminological pollution without agency to correct it. Policy makers experience asymmetric risk: they must regulate based on vendor claims ('reasoning') but lack verification mechanisms. The organized critics see the mechanism (snare structure with high suppression), but their advocacy has minimal market impact. The piton perspective reveals that philosophical traditions have been hollowed out — terminology persists but meaning has drained away. The mountain perspective (natural language development) is a false summit — the suppression (0.72) is incompatible with natural emergence; the high theater ratio indicates active enforcement of ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary and victim positions: Vendors (beneficiaries with arbitrage options) experience low or negative extraction — the constraint subsidizes their position. They can costlessly switch between interpretations ('reasoning' in marketing, 'pattern matching' in research). Practitioners and policy makers (victims with trapped or constrained exits) experience high extraction. They cannot abandon the terminology without exiting their fields. The organized critics have constrained exits — they can produce counter-arguments but lack enforcement mechanisms to change vendor behavior. The piton perspective (theater_ratio >> suppression) suggests that philosophical terminology persists through institutional inertia even as the underlying philosophical work is abandoned.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED. The constraint exhibits all three snare signatures: (1) High extractiveness (0.58) — vendors benefit from capability claims, practitioners bear epistemic costs. (2) High suppression (0.72) — powerful market incentives prevent terminology correction; vendor resistance to precision is not passive but active. (3) Trapped exit options for victims — practitioners cannot reject the terminology without abandoning their research communities. The mandatrophy is resolved by recognizing that this is not a coordination problem (which would require both parties to benefit) but a pure extraction mechanism. The vendors experience it as coordination because they benefit; the victims experience it as extraction because they bear costs. The asymmetry is the defining feature of a snare. Rising theater ratio indicates the snare is becoming more entrenched — philosophical scaffolding is increasingly decorative rather than functional, yet persists as prestige marker.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reasoning_definition_boundary,
    'Is there a natural language boundary between ''reasoning'' (goal-directed symbolic manipulation) and ''pattern matching'' (associative statistical inference)?',
    'Formal analysis of cognitive science, neuroscience, and AI architecture literature; mapping of functional properties that distinguish reasoners from pattern-matchers',
    'If boundary exists and is clear: vendors are deliberately violating it (snare confirmed). If boundary is blurry: zombie terminology reflects legitimate conceptual confusion, and classification shifts toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reasoning_definition_boundary, empirical, 'Whether reasoning and pattern-matching have a definable boundary').

omega_variable(
    vendor_intent_disambiguation,
    'Do AI vendors use imprecise terminology due to honest conceptual confusion, or due to strategic ambiguity to exploit regulatory and investor uncertainty?',
    'Institutional analysis: internal documentation, training materials, competitive strategy documents; comparative study of vendor marketing language vs internal research language',
    'If honest confusion: snare classification requires downgrade to tangled_rope (both benefit and extract). If strategic: snare confirmed with high confidence (extraction is intentional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_intent_disambiguation, empirical, 'Whether terminology imprecision is intentional or due to confusion').

omega_variable(
    terminology_correction_feasibility,
    'Can the AI field adopt precise terminology (e.g., ''output selection,'' ''heuristic search,'' ''statistical inference'') as a replacement without market collapse?',
    'Hypothetical policy analysis: what would be required for all vendors to adopt precise terminology? Cost-benefit analysis for vendors vs public benefit. Historical precedent from other fields (medical, pharmaceutical) that adopted precise terminology.',
    'If feasible: snare is maintained by suppression (artificial barrier to correction). If infeasible: the constraint is rooted in cognitive/linguistic limits (shifts toward mountain). If partially feasible but blocked by incentives: confirms snare with active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminology_correction_feasibility, empirical, 'Whether precise terminology is technically feasible').

omega_variable(
    downstream_harm_quantification,
    'What is the measurable downstream epistemic and institutional cost of zombie reasoning terminology in policy decisions, research direction, and capital allocation?',
    'Longitudinal study of policy decisions made under zombie terminology vs decisions made with precise terminology; impact analysis on research resource allocation; regulatory cost analysis',
    'If harm is severe (>$10B misallocated, >50% of policies based on confused models): suppression is very high, snare is severe. If harm is modest: snare classification may be overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(downstream_harm_quantification, empirical, 'Quantification of downstream costs from terminology confusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zombie_reasoning_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zombr_tr_t0, zombie_reasoning_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(zombr_tr_t3, zombie_reasoning_2026, theater_ratio, 3, 0.52).
narrative_ontology:measurement(zombr_tr_t6, zombie_reasoning_2026, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(zombr_be_t0, zombie_reasoning_2026, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(zombr_be_t3, zombie_reasoning_2026, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(zombr_be_t6, zombie_reasoning_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zombie_reasoning_2026, information_standard).
narrative_ontology:affects_constraint(zombie_reasoning_2026, ai_capability_assessment_bottleneck).
narrative_ontology:affects_constraint(zombie_reasoning_2026, regulatory_uncertainty_extraction).

% DUAL FORMULATION NOTE:
% The zombie reasoning snare is distinct from but upstream of regulatory uncertainty extraction. Zombie terminology enables vendors to exploit regulatory ambiguity; the snare is the root mechanism that creates the conditions for downstream regulatory extraction. Affects AI capability assessment because practitioners making empirical claims about system abilities must operate within the corrupted terminology space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zombie_reasoning_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
