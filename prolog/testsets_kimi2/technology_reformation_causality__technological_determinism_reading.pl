% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press Technological Determinism (Mass Vernacular Distribution)
 *   domain: history/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story models the technological determinism reading of the
 *   printing pressâReformation relationship. Within this reading, the
 *   physical-economic properties of movable type printing â drastically
 *   lower marginal reproduction cost, speed, and the feasibility of
 *   vernacular type â constitute a fixed technological constraint that made
 *   mass vernacular scripture distribution inevitable. The Reformation is
 *   treated as a downstream adaptation to this physical fact rather than as
 *   an independent cause. The constraint governs the sixteenth-century
 *   European textual economy: agents who control or adopt the press operate
 *   within a cost structure that renders manuscript alternatives economically
 *   irrational, while agents whose authority depended on textual scarcity
 *   face an existential disruption that they cannot suppress by edict.
 *
 * KEY AGENTS:
 *   - vernacular_reformers: Primary beneficiary (moderate/mobile) â gains mass distribution channel for theological agenda
 *   - commercial_printers: Secondary beneficiary (moderate/mobile) â profits from surge in vernacular demand
 *   - lay_vernacular_readers: Tertiary beneficiary (powerless/constrained) â gains affordable vernacular scripture access
 *   - catholic_hierarchy: Primary target (institutional/constrained) â loses Latin interpretive monopoly and textual control
 *   - scribal_monasteries: Secondary target (organized/trapped) â lose economic and spiritual identity bound to manuscript production
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.32).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.18).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press Technological Determinism (Mass Vernacular Distribution)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec').
narrative_ontology:cs_kernel_codification('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', fixed_text).
narrative_ontology:cs_authority_grounding('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', expertise).
narrative_ontology:cs_interpretation_layer_present('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec').
narrative_ontology:cs_reading_relation('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', foundational, technology_determines_religious_outcomes).
narrative_ontology:cs_axiom_status(technology_determines_religious_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', technology_determines_religious_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', foundational, mass_distribution_inevitability).
narrative_ontology:cs_axiom_status(mass_distribution_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', mass_distribution_inevitability, empirically_contingent).
narrative_ontology:cs_reference_frame('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', technological_infrastructure_primacy).
narrative_ontology:cs_drift_state('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', post_social_history_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('790fdfd2-2fcb-4b83-b0b6-b8f0994ca0ec', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, commercial_printers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, lay_vernacular_readers).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, catholic_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, scribal_monasteries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the press to mass-produce vernacular scriptures and polemical pamphlets. Their theological agenda gains geographic reach because the technology makes reproduction cheap and fast relative to manuscript copying. They do not control the technology but are its primary adopters and beneficiaries.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_reformers, beneficiary,
    moderate, biographical, mobile, continental).

% Own and operate printing presses in urban centers; profit from the surge in demand for vernacular religious texts. They sell to dispersed markets and their business model depends on the volume enabled by movable type.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, commercial_printers, beneficiary,
    moderate, biographical, mobile, continental).

% Gain access to vernacular religious texts at prices far below manuscript copies. Private reading and interpretation become possible outside parish structures, though they depend on printers and reformers for supply and on literacy for access.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, lay_vernacular_readers, beneficiary,
    powerless, biographical, constrained, regional).

% Hold a monopoly on textual interpretation and liturgical language (Latin). The press erodes their information control by enabling lay vernacular reading outside clerical oversight. They issue bans and burn books, but cannot cost-effectively out-produce or out-distribute the presses.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, catholic_hierarchy, payer,
    institutional, generational, constrained, continental).

% Produce handwritten manuscripts as a core religious and economic activity. The press collapses demand for their product and devalues their skill. They cannot pivot to print without capital and training, and their institutional identity is fused with the old mode of production.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, scribal_monasteries, payer,
    organized, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the production and distribution of identical vernacular texts across geographically dispersed populations at a fraction of manuscript cost, solving the coordination problem of shared religious knowledge without centralized scribal infrastructure.
% TRANSFER_FUNCTION: Moves textual reproduction from high-cost monastic scriptoria to low-cost urban print shops; moves interpretive authority and access from the Latin-reading clerical hierarchy to vernacular-reading laity and reformers.
% ABSENT_VOICES: Monastic copyists and lay Catholics who preferred parish-mediated religion; their exclusion from the printed textual market was economic (they could not afford to commission print runs) and linguistic (their religiosity was not encoded in vernacular type).
% DISAPPEARANCE_RATIONALE: If movable type printing vanished overnight, the Reformation's mass textual base would collapse; manuscript production could not scale to meet vernacular demand, and the Church's information monopoly would reconstitute itself. The European religious landscape would rearrange back toward centralized textual control.
% FOUNDING_PROBLEM: Pre-print manuscript culture imposed severe constraints on textual reproduction: high cost, low speed, Latin linguistic monopoly, and geographic concentration in monastic centers, limiting religious discourse to clerical elites.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the medieval book attest to the manuscript bottleneck from outside the reformer community; Catholic polemicists also acknowledged the press's speed but argued the bottleneck was a necessary guard on orthodoxy.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.32, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Accessibility collapse is 0.92 because once movable type and paper are available, manuscript copying for mass distribution becomes economically impossible â the alternative does not merely weaken but collapses under the physics of cost. Resistance is 0.15 because the Church's censorship and prohibition cannot overcome the order-of-magnitude cost advantage of print; resistance is rear-guard and symbolic. Extractiveness is 0.32 because the technology extracts from the Church's prior monopoly by dissolving the artificial scarcity on which it rested; the extraction is not actively enforced by the press but emerges from its physical capability. Suppression is 0.18 because the constraint persists by physical possibility and economic superiority, not by coercion. Theater ratio is 0.08 â the press is a functional technology with minimal performative overlay. The divergence between the mountain claim and non-zero extraction is intentional: the engine should detect whether a physical-technological constraint with beneficiaries is a false summit.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer and printer seats, the press is a liberating coordination mechanism that collapses information costs and enables mass religious participation. From the Church and scribal seats, the identical physical arrangement is an existential threat that extracts their centuries-old monopoly on interpretation and reproduction. The engine computes this asymmetry from the structural data: same scope, same interval, but opposite directionality derived from beneficiary versus victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers, printers, and lay readers are beneficiaries (directionality near 0.0) because the technology subsidizes their textual production and consumption. The Catholic hierarchy and scribal orders are targets (directionality near 1.0) because the cost reduction destroys the economic basis of their control. The asymmetry is structural: one side gains access at negligible marginal cost, the other loses a monopoly maintained by high reproduction costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the press as pure extraction (a snare) or pure coordination (a rope). The press has a genuine coordination effect â it enables widespread text sharing â but it asymmetrically disrupts the prior Church monopoly. However, the disruption emerges from physical capability rather than active enforcement, and the Church's control had itself depended on artificial scarcity (a prior snare). Therefore the press is structurally a mountain that exposes a prior extraction structure, not a tangled rope. Mandatrophy is resolved by asking whether the constraint persists because of enforcement: it does not. It persists because it is physically possible and economically dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_invented_technology,
    'Is the printing press a physical-economic inevitability (like a natural law) or a constructed technology whose deployment was contingent on specific social and economic conditions?',
    'Comparative history of press adoption (e.g., Chinese woodblock and movable type precedents without analogous religious fragmentation) to test whether the technology alone is sufficient for the outcome.',
    'If deployment was contingent on social conditions, the mountain claim is a false summit and should reclassify as tangled_rope or rope; if the technology is genuinely autonomous, the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_invented_technology, conceptual, 'Whether the press is a natural limit or a socially contingent invention').

omega_variable(
    extraction_mechanism_ambiguity,
    'Does the press actively extract from the Church monopoly, or merely reveal that the monopoly was always a snare maintained by artificial scarcity?',
    'Economic analysis of pre-print manuscript costs versus post-print price collapse to determine whether the Church''s control was economically viable without scarcity.',
    'If the press only reveals artificial scarcity, its epsilon is lower and it functions more like a mountain exposing a prior snare rather than an extractive constraint in its own right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_ambiguity, empirical, 'Whether the press extracts or merely exposes prior extraction').

omega_variable(
    agency_contingency,
    'Would the Reformation have occurred without Luther and reformer agency, given the press?',
    'Counterfactual historiography and examination of non-reformist uses of print (Catholic print, scientific print) to test whether reformer agency was necessary for the religious outcome.',
    'If reformer agency was necessary, the inevitability claim is overstated and the constraint is less deterministic than the reading asserts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agency_contingency, empirical, 'Whether reformer agency is contingent or redundant to the technology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_ref_det_tr_t0, technology_reformation_causality__technological_determinism_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tech_ref_det_tr_t20, technology_reformation_causality__technological_determinism_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(tech_ref_det_tr_t40, technology_reformation_causality__technological_determinism_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(tech_ref_det_tr_t60, technology_reformation_causality__technological_determinism_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(tech_ref_det_tr_t80, technology_reformation_causality__technological_determinism_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement(tech_ref_det_tr_t100, technology_reformation_causality__technological_determinism_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(tech_ref_det_be_t0, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(tech_ref_det_be_t20, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(tech_ref_det_be_t40, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(tech_ref_det_be_t60, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement(tech_ref_det_be_t80, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement(tech_ref_det_be_t100, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 100, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__technological_determinism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the technology_reformation_causality kernel. The sibling readings instantiate structurally distinct constraints from the same historical proposition: agency-focused and co-constitution framings. Each carries a distinct epsilon and stakeholder geometry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
