% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualism for Socialist Transformation
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'democratic gradualism' reading of
 *   the broader 'manifesto_revolutionary_method' kernel. It describes the
 *   belief and practice that socialism is achievable through democratic
 *   electoral majorities and gradual institutional reform, with working-class
 *   power exercised primarily through existing democratic structures. This
 *   reading emphasizes institutional continuity with liberal democracy,
 *   positioning social democratic parties and trade unions as key
 *   beneficiaries, while revolutionary militants are often suppressed or
 *   delegitimized as 'adventurist'. The moderate extractiveness (0.40)
 *   reflects the inherent limitations and co-optation risks within existing
 *   structures, which can slow or dilute radical transformation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.65).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism for Socialist Transformation").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'c2e696e5-3ffa-43c1-b86e-a195e8e81c3d').
narrative_ontology:cs_kernel_codification('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', formalized).
narrative_ontology:cs_authority_grounding('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', practice).
narrative_ontology:cs_interpretation_layer_present('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d').
narrative_ontology:cs_reading_relation('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_axiom('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', foundational, democratic_legitimacy_is_primary).
narrative_ontology:cs_axiom_status(democratic_legitimacy_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', democratic_legitimacy_is_primary, deontological).
narrative_ontology:cs_axiom('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', foundational, state_is_instrument_for_change).
narrative_ontology:cs_axiom_status(state_is_instrument_for_change, holdable).
narrative_ontology:cs_axiom_grounding('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', state_is_instrument_for_change, instrumental).
narrative_ontology:cs_reference_frame('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', parliamentary_socialist_path).
narrative_ontology:cs_drift_state('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', post_neoliberal_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('c2e696e5-3ffa-43c1-b86e-a195e8e81c3d', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_left).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These parties operate within existing democratic structures, advocating for gradual reforms and electoral victories as the primary means to achieve socialist goals. They benefit from the legitimacy and institutional access afforded by this approach.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, constrained, national).

% Trade unions often align with social democratic parties, using collective bargaining and political lobbying within the democratic framework to improve workers' conditions. They benefit from the institutional stability and reformist potential of this path.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions, beneficiary,
    organized, biographical, constrained, national).

% A broader coalition of left-leaning voters and activists who believe in the power of the ballot box and incremental policy changes. They are beneficiaries of a system that validates their chosen method of political engagement.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_left, beneficiary,
    moderate, biographical, mobile, national).

% Advocates for immediate, often extra-legal, revolutionary change. They are delegitimized and often actively suppressed by the state and mainstream political discourse, bearing the cost of being excluded from the 'acceptable' path to socialism.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    powerless, immediate, trapped, local).

% Groups that seek social change outside of formal electoral politics, through direct action, protests, or community organizing. While not always revolutionary, their methods are often marginalized or co-opted by the emphasis on democratic gradualism, making them 'payers' in terms of political influence.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_movements, payer,
    moderate, biographical, constrained, national).

% The overarching political framework within which democratic gradualism operates. It provides the structures (elections, parliament, legal system) that this reading utilizes, and it actively enforces the boundaries of legitimate political action, often suppressing revolutionary alternatives.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates working-class political action and broader left-wing movements within existing democratic frameworks to achieve incremental reforms, build electoral power, and eventually transition to a socialist society.
% TRANSFER_FUNCTION: Transfers political legitimacy, resources, and public attention to established democratic institutions, political parties, and reformist policies, while simultaneously marginalizing and delegitimizing revolutionary or extra-parliamentary approaches.
% ABSENT_VOICES: Revolutionary theorists and activists (e.g., anarchists, council communists, vanguardists) are structurally excluded from the legitimate discourse on achieving socialism. They would argue that the existing democratic structures are inherently capitalist and cannot be reformed to achieve genuine liberation.
% DISAPPEARANCE_RATIONALE: If the belief in democratic gradualism vanished overnight, working-class movements and the broader left would likely fragment. This could lead to widespread political instability, a surge in more radical (and potentially violent) revolutionary attempts, or a retreat into political apathy, as the perceived 'safe' path to change would be gone. The political landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The problem of achieving a more just and equitable society (socialism) without resorting to violent revolution, building on existing democratic gains, and avoiding the authoritarian pitfalls observed in historical revolutionary states.
% FOUNDING_PROBLEM_CORROBORATION: Historians of social democracy, political scientists studying comparative political systems, and mainstream media outlets often corroborate the historical efficacy and ongoing relevance of democratic gradualism as a path for social change, distinct from revolutionary alternatives. This perspective is also supported by the historical record of social democratic reforms in various countries.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.40) because while democratic gradualism aims for social change, it often faces significant resistance from entrenched interests and the structural limitations of the capitalist state, leading to compromises that dilute its transformative potential. Suppression is high (0.65) because this reading actively delegitimizes and often politically marginalizes (or even legally suppresses) alternative, more radical revolutionary paths, framing them as dangerous or unfeasible. The theater ratio is moderate (0.40) as democratic processes can involve significant performative aspects, but there is also genuine political contest and policy implementation. Accessibility collapse (0.55) reflects that while revolutionary alternatives exist, the dominant discourse and institutional structures make them difficult to pursue effectively. Resistance (0.50) comes from both the capitalist status quo and from revolutionary factions who reject this path.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social democratic parties and trade unions, democratic gradualism is the only pragmatic and legitimate path to social justice, offering real gains within a stable framework. From the perspective of revolutionary militants, this same constraint is a 'tangled rope' or 'snare' that co-opts and neutralizes genuine revolutionary potential, trapping movements within a system designed to perpetuate capitalism.
 *
 * DIRECTIONALITY LOGIC:
 *   Social democratic parties, trade unions, and the electoral left are beneficiaries (low directionality) as this constraint validates their chosen methods and provides institutional avenues for their influence. Revolutionary militants and extra-parliamentary movements are targets (high directionality) as their alternative approaches are suppressed or marginalized by the emphasis on democratic gradualism. The liberal democratic state acts as an agenda-setter, defining the legitimate playing field.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_gradualism,
    'Is democratic gradualism truly capable of achieving a socialist transformation, or does it primarily function to manage capitalism and absorb revolutionary pressures?',
    'Longitudinal comparative studies of social democratic governments'' ability to fundamentally alter capitalist property relations and power structures over multiple decades, compared to their stated goals.',
    'If found to primarily manage capitalism, the constraint''s effective extractiveness would be higher, and its coordination function for achieving socialism would be re-evaluated as largely theatrical or co-opted. If found to be genuinely transformative, its extractiveness would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_gradualism, empirical, 'Whether gradual reform can achieve fundamental systemic change.').

omega_variable(
    legitimacy_of_state_power,
    'Is the liberal democratic state a neutral instrument that can be captured and wielded for socialist ends, or is it inherently structured to defend capitalist interests, making genuine socialist transformation through it impossible?',
    'Conceptual analysis of state theory (e.g., instrumentalist vs. structuralist views) combined with historical case studies of state responses to radical socialist movements.',
    'If the state is inherently capitalist, the ''democratic gradualism'' reading''s foundational axioms would be challenged, potentially shifting its classification towards a ''snare'' for those genuinely seeking socialism, as the path would be structurally blocked. If neutral, its ''rope'' or ''tangled_rope'' classification would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_state_power, conceptual, 'The inherent nature and neutrality of the liberal democratic state.').

omega_variable(
    suppression_of_alternatives_justification,
    'Is the suppression of revolutionary and extra-parliamentary alternatives by the democratic gradualist framework a necessary defense of democratic stability, or a mechanism to protect the existing power structures and prevent more radical change?',
    'Analysis of historical instances of state repression against socialist movements, distinguishing between actions against genuine threats to democracy versus actions against legitimate political dissent.',
    'If primarily a defense of existing power, the suppression metric''s justification would be re-evaluated, potentially increasing the perceived extractiveness and snare-like qualities of the constraint. If primarily a defense of democracy, the suppression would be seen as a legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives_justification, preference, 'Justification for suppressing revolutionary alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(mani_tr_t60, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(mani_tr_t80, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(mani_tr_t120, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 120, 0.4).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(mani_be_t60, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 60, 0.37).
narrative_ontology:measurement(mani_be_t80, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 80, 0.39).
narrative_ontology:measurement(mani_be_t120, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 120, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(mani_su_t60, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(mani_su_t80, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 80, 0.64).
narrative_ontology:measurement(mani_su_t120, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 120, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, capitalist_property_rights).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_electoral_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
