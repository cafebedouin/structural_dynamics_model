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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This constraint story instantiates the 'democratic gradualism' reading of
 *   the 'manifesto_revolutionary_method' kernel. It describes the belief and
 *   political strategy that socialism can be achieved through democratic
 *   electoral majorities and gradual institutional reform, with working-class
 *   power exercised through existing democratic structures. This reading
 *   emphasizes institutional continuity and reform over revolutionary
 *   rupture, often leading to the marginalization of more radical
 *   alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.65).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism for Socialist Transformation").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'cf2e556b-8521-4160-8af4-898312d74f75').
narrative_ontology:cs_kernel_codification('cf2e556b-8521-4160-8af4-898312d74f75', formalized).
narrative_ontology:cs_authority_grounding('cf2e556b-8521-4160-8af4-898312d74f75', lineage).
narrative_ontology:cs_interpretation_layer_present('cf2e556b-8521-4160-8af4-898312d74f75').
narrative_ontology:cs_reading_relation('cf2e556b-8521-4160-8af4-898312d74f75', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf2e556b-8521-4160-8af4-898312d74f75', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('cf2e556b-8521-4160-8af4-898312d74f75', foundational, democratic_legitimacy_is_primary).
narrative_ontology:cs_axiom_status(democratic_legitimacy_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('cf2e556b-8521-4160-8af4-898312d74f75', democratic_legitimacy_is_primary, deontological).
narrative_ontology:cs_axiom('cf2e556b-8521-4160-8af4-898312d74f75', foundational, state_is_transformable).
narrative_ontology:cs_axiom_status(state_is_transformable, holdable).
narrative_ontology:cs_axiom_grounding('cf2e556b-8521-4160-8af4-898312d74f75', state_is_transformable, empirically_contingent).
narrative_ontology:cs_reference_frame('cf2e556b-8521-4160-8af4-898312d74f75', parliamentary_socialist_path).
narrative_ontology:cs_drift_state('cf2e556b-8521-4160-8af4-898312d74f75', post_neoliberal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cf2e556b-8521-4160-8af4-898312d74f75', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, reformist_intellectuals).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_voters).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, capitalist_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary political actors advocating for and implementing democratic gradualism. They benefit from the legitimacy and stability of working within existing democratic structures, gaining electoral power and influence.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the institutional access and legislative reforms achieved through democratic gradualism, which can improve workers' rights and conditions. Their power is channeled through established political processes.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions, beneficiary,
    organized, biographical, constrained, national).

% Are coordinated into electoral action, benefiting from social reforms but also bearing the costs of slow change, political compromises, and the marginalization of more radical alternatives that might offer quicker, albeit riskier, transformation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_voters, payer,
    moderate, biographical, constrained, national).

% Are actively marginalized and suppressed as 'adventurist' or 'utopian' by the proponents of democratic gradualism. They bear the cost of political exclusion and the delegitimization of their preferred methods for social change.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, excluded,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer).

% The existing institutional framework through which gradual reform is sought. It provides the arena for political action and benefits from the stability and legitimacy conferred by this approach, even as it undergoes reform.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Concede some reforms and bear some costs (e.g., higher taxes, regulations) but ultimately benefit from the stability and continuity of the existing system, which democratic gradualism seeks to transform rather than overthrow violently.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, capitalist_elites, payer,
    powerful, generational, arbitrage, global).

% Provide the theoretical justification and policy frameworks for democratic gradualism, gaining academic and political influence through their contributions to the strategy.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, reformist_intellectuals, beneficiary,
    moderate, biographical, mobile, global).

% Advocate for direct action, protests, and other non-electoral means of change. They are often sidelined or actively opposed by proponents of democratic gradualism, bearing the cost of being outside the mainstream political process.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_movements, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_movements, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates working-class political action and broader social movements within existing democratic and parliamentary frameworks to achieve social and economic reforms, aiming for a socialist transformation without violent rupture.
% TRANSFER_FUNCTION: Transfers political legitimacy, resources, and activist energy from radical, extra-parliamentary, or revolutionary movements to established democratic institutions, social democratic parties, and trade unions, in exchange for the promise and gradual implementation of reforms.
% ABSENT_VOICES: Revolutionary militants and various extra-parliamentary movements are structurally excluded or actively marginalized. They would argue that democratic gradualism co-opts and defangs genuine revolutionary potential, ultimately serving to stabilize capitalism rather than transcend it.
% DISAPPEARANCE_RATIONALE: If the belief in democratic gradualism vanished overnight, working-class political action would likely fragment into more radical, extra-parliamentary, or even violent forms. This would lead to significant social and political upheaval, as the primary mechanism for managing class conflict within existing systems would be removed.
% FOUNDING_PROBLEM: The problem of how to achieve a more just, socialist society without resorting to violent revolution, civil war, or authoritarianism, by leveraging and reforming existing democratic institutions and processes.
% FOUNDING_PROBLEM_CORROBORATION: Historians of social democracy, political scientists studying democratic transitions, and some segments of the working class who have benefited from reforms within the system corroborate the founding problem's historical context and ongoing relevance. However, revolutionary theorists and some contemporary activists contest its efficacy in the face of persistent capitalist power.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates working-class political action within democratic systems (benefiting social democratic parties and unions) while simultaneously extracting from and suppressing more radical, non-electoral approaches (victims: revolutionary militants, extra-parliamentary movements). The moderate extractiveness (0.40) reflects the compromises and slow pace of change inherent in gradualism, which can be seen as a cost to those seeking more rapid transformation. The high suppression (0.65) indicates the active delegitimization and marginalization of revolutionary alternatives, which is necessary to maintain the coherence of the gradualist strategy. Theater ratio is low (0.15) as it represents a genuine political strategy, not mere performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social democratic parties and trade unions, democratic gradualism is a legitimate and effective path to social justice, offering tangible gains within a stable framework. From the perspective of revolutionary militants, it is a co-optive mechanism that defangs genuine revolutionary potential, channeling energy into reforms that ultimately stabilize capitalism. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Social democratic parties and trade unions are beneficiaries, as the constraint channels political power and resources through them. Reformist intellectuals also benefit by providing theoretical justification. Revolutionary militants and extra-parliamentary movements are victims, as their methods are suppressed and their political space is constrained. Working-class voters and capitalist elites are payers, bearing the costs of slow change and reforms respectively, but also benefiting from the stability of the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (ignoring the suppression of alternatives) or a Snare (ignoring the genuine coordination function and reforms achieved). The ongoing contestation of its founding problem status (live vs. dead) and the observed drift in extractiveness and suppression over time are key for lifecycle drift detection, indicating that the constraint's function may be shifting from pure coordination to more extractive management of political dissent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_gradualism,
    'Is democratic gradualism structurally capable of achieving a socialist transformation, or does it primarily function to reform and stabilize capitalism?',
    'Longitudinal historical analysis comparing outcomes in states pursuing gradualist paths versus those pursuing revolutionary paths, focusing on fundamental changes in property relations and class power.',
    'If primarily stabilizing capitalism, the constraint''s effective extractiveness from the working class (as a whole) would be higher, and its coordination function for genuine socialist transformation would be lower, potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_gradualism, empirical, 'The ultimate transformative capacity of gradualist methods.').

omega_variable(
    co_optation_vs_integration,
    'Is the marginalization of revolutionary alternatives a necessary coordination function for democratic stability, or a co-optation mechanism that defangs genuine social change?',
    'Comparative political analysis of democratic systems that tolerate a wider range of political methods versus those that actively suppress them, assessing long-term stability and social progress.',
    'If primarily co-optation, the suppression metric would be interpreted as more extractive, and the constraint''s overall classification would lean more towards a Snare. If necessary coordination, the suppression would be seen as a legitimate cost of maintaining a stable political process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_optation_vs_integration, conceptual, 'The nature of suppression of radical alternatives.').

omega_variable(
    state_neutrality_ambiguity,
    'Is the liberal democratic state a neutral arena for class struggle, or an inherently capitalist institution that limits the scope of gradualist transformation?',
    'Theoretical and empirical analysis of state autonomy and its relationship to capitalist interests, particularly during periods of significant social democratic governance.',
    'If the state is inherently capitalist, the ''state_is_transformable'' axiom of this reading would be challenged, potentially leading to a re-evaluation of the constraint''s long-term viability and its capacity for non-extractive coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_neutrality_ambiguity, conceptual, 'The structural nature of the liberal democratic state in relation to socialist transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(mani_tr_t1930, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(mani_tr_t1960, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(mani_tr_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(mani_tr_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(mani_be_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(mani_be_t1930, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1930, 0.35).
narrative_ontology:measurement(mani_be_t1960, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1960, 0.38).
narrative_ontology:measurement(mani_be_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(mani_be_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2020, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement(mani_su_t1930, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(mani_su_t1960, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(mani_su_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(mani_su_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2020, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_electoral_system).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, capitalist_property_rights).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'manifesto_revolutionary_method' kernel, focusing on democratic gradualism. It is distinct from the 'vanguard_rupture_reading' and 'council_communist_reading' which propose different methods for achieving socialism, primarily differing on the role of the state, party, and existing democratic structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
