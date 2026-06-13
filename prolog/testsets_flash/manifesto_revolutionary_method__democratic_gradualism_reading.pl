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
 *   This constraint represents the belief that socialism is achievable
 *   through democratic electoral majorities and gradual institutional reform,
 *   with working-class power exercised through existing democratic
 *   structures. It is a specific reading of the broader
 *   'manifesto_revolutionary_method' kernel, emphasizing continuity with
 *   liberal democratic processes over revolutionary rupture. The constraint
 *   is claimed as a Rope due to its genuine coordination function for
 *   reformist movements, but its moderate extractiveness and suppression
 *   reflect the costs borne by those who advocate for more radical,
 *   non-parliamentary paths.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.3).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism for Socialist Transformation").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, '2d9ec18d-b561-405d-aaeb-04ff3fae6906').
narrative_ontology:cs_kernel_codification('2d9ec18d-b561-405d-aaeb-04ff3fae6906', formalized).
narrative_ontology:cs_authority_grounding('2d9ec18d-b561-405d-aaeb-04ff3fae6906', lineage).
narrative_ontology:cs_interpretation_layer_present('2d9ec18d-b561-405d-aaeb-04ff3fae6906').
narrative_ontology:cs_reading_relation('2d9ec18d-b561-405d-aaeb-04ff3fae6906', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d9ec18d-b561-405d-aaeb-04ff3fae6906', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('2d9ec18d-b561-405d-aaeb-04ff3fae6906', foundational, democratic_legitimacy_is_foundational).
narrative_ontology:cs_axiom_status(democratic_legitimacy_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('2d9ec18d-b561-405d-aaeb-04ff3fae6906', democratic_legitimacy_is_foundational, deontological).
narrative_ontology:cs_axiom('2d9ec18d-b561-405d-aaeb-04ff3fae6906', foundational, gradual_reform_is_effective).
narrative_ontology:cs_axiom_status(gradual_reform_is_effective, holdable).
narrative_ontology:cs_axiom_grounding('2d9ec18d-b561-405d-aaeb-04ff3fae6906', gradual_reform_is_effective, empirically_contingent).
narrative_ontology:cs_reference_frame('2d9ec18d-b561-405d-aaeb-04ff3fae6906', parliamentary_socialist_tradition).
narrative_ontology:cs_drift_state('2d9ec18d-b561-405d-aaeb-04ff3fae6906', contemporary_neoliberal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2d9ec18d-b561-405d-aaeb-04ff3fae6906', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_working_class).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and implements socialist policies through parliamentary means, relying on electoral victories and coalition building. Benefits from the legitimacy of the democratic process and the marginalization of more radical alternatives.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, constrained, national).

% Works within existing legal frameworks to improve working conditions and wages, often aligning with social democratic parties. Benefits from the stability of democratic institutions and the ability to negotiate within established channels.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions, beneficiary,
    organized, biographical, constrained, national).

% Participates in the democratic process through voting and activism, seeking to elect representatives who will enact socialist reforms. Benefits from incremental improvements in social welfare and labor rights.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_working_class, beneficiary,
    moderate, biographical, mobile, local).

% Advocates for immediate, non-parliamentary overthrow of the capitalist state. Is often suppressed or marginalized by the state apparatus, which is legitimized by the democratic gradualist approach. Their actions are framed as 'adventurist' or 'undemocratic'.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    powerless, immediate, trapped, local).

% Seeks change through direct action, protests, and community organizing outside of formal electoral politics. Faces state repression and public disapproval, which are often justified by the democratic gradualist narrative as upholding order and legitimate process.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_movements, payer,
    moderate, biographical, constrained, national).

% The existing institutional framework through which gradualist reforms are pursued. It provides the arena for political contestation but also sets limits on the pace and scope of change, often resisting fundamental transformation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of diverse working-class and progressive movements towards a common goal of socialist transformation through a unified electoral strategy and incremental policy changes within existing democratic institutions.
% TRANSFER_FUNCTION: Transfers political legitimacy and resources from revolutionary or extra-parliamentary movements to established social democratic parties and trade unions, in exchange for the promise of gradual, democratic change.
% ABSENT_VOICES: Those who believe that the existing democratic structures are inherently capitalist and cannot be reformed to achieve true socialism are excluded. Their voices are often dismissed as utopian or dangerous, and their methods are actively suppressed by the state apparatus that the gradualist approach legitimizes.
% DISAPPEARANCE_RATIONALE: If the belief in democratic gradualism vanished, the political landscape would fragment. Social democratic parties would lose their core mandate, trade unions would face pressure to adopt more confrontational tactics, and revolutionary movements would likely gain significant traction, leading to widespread political instability and potentially violent confrontations.
% FOUNDING_PROBLEM: The historical problem of achieving social justice and economic equality without resorting to violent revolution or authoritarianism, seeking a path that respects individual liberties and democratic norms.
% FOUNDING_PROBLEM_CORROBORATION: Historians of socialist movements and political scientists corroborate that the tension between revolutionary and reformist paths has been a live problem since the 19th century. Contemporary political discourse and ongoing debates within left-wing parties continue to attest to its relevance, even outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).

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
 *   The extractiveness (0.40) stems from the opportunity costs and delays inherent in gradual reform, as well as the co-optation of radical energies into electoral channels. Suppression (0.30) is present in the marginalization and occasional repression of revolutionary movements by the state, which is legitimized by the democratic gradualist framework. The theater ratio (0.15) is low, indicating that the democratic process is largely functional, though it may sometimes serve to contain rather than accelerate fundamental change. Accessibility collapse (0.45) is moderate, as alternative paths exist but are actively discouraged or suppressed. Resistance (0.20) is low, as the dominant political forces largely accept this method.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social democratic parties, this is a legitimate and effective path to social justice. From the perspective of revolutionary militants, it is a snare that co-opts and defangs genuine revolutionary potential, perpetuating capitalist structures under a democratic guise. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Social democratic parties and trade unions are clear beneficiaries, as their power and legitimacy are directly tied to this method. The electoral working class also benefits from the incremental gains. Revolutionary militants and extra-parliamentary movements are victims, as their methods are delegitimized and suppressed. The liberal democratic state acts as an agenda-setter, providing the framework and enforcing its boundaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_gradualism,
    'Is democratic gradualism genuinely capable of achieving fundamental socialist transformation, or does it merely reform capitalism without overcoming it?',
    'Long-term historical analysis of states that have pursued this path: do they achieve a qualitative shift to socialism, or do they remain capitalist welfare states?',
    'If it only reforms capitalism, the extractiveness for revolutionary movements is higher, as their suppression is for naught. If it genuinely transforms, the extractiveness is justified as a necessary cost of peaceful transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_gradualism, empirical, 'The actual transformative power of democratic gradualism.').

omega_variable(
    legitimacy_of_state_suppression,
    'Is the suppression of revolutionary and extra-parliamentary movements by the democratic state a legitimate defense of democratic process, or an extractive act to protect existing power structures?',
    'Analysis of state actions against these movements: are they proportional to actual threats to democracy, or do they disproportionately target dissent that challenges the status quo?',
    'If legitimate, the suppression is a necessary cost of coordination. If extractive, it highlights the Snare-like qualities of the democratic gradualism for those outside its bounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_state_suppression, conceptual, 'The justification for state suppression of non-parliamentary movements.').

omega_variable(
    kernel_reading_delta_vanguard_rupture,
    'This constraint is the ''democratic_gradualism_reading'' of the ''manifesto_revolutionary_method'' kernel. How would the classification change if the ''vanguard_rupture_reading'' were adopted?',
    'The ''vanguard_rupture_reading'' would likely classify the existing democratic state as a Snare, with high extractiveness and suppression, and the vanguard party as the primary beneficiary/agenda-setter. The ''democratic_gradualism_reading'' would be seen as a form of false consciousness or co-optation.',
    'A shift to the ''vanguard_rupture_reading'' would invert the beneficiary/victim structure and significantly increase the perceived extractiveness and suppression of the existing political system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta_vanguard_rupture, conceptual, 'Impact of adopting the ''vanguard_rupture_reading'' on classification.').

omega_variable(
    kernel_reading_delta_council_communist,
    'This constraint is the ''democratic_gradualism_reading'' of the ''manifesto_revolutionary_method'' kernel. How would the classification change if the ''council_communist_reading'' were adopted?',
    'The ''council_communist_reading'' would likely classify both the existing democratic state and the vanguard party (from the ''vanguard_rupture_reading'') as Snares, as both represent centralized power structures that prevent direct workers'' democracy. The ''democratic_gradualism_reading'' would be seen as a less direct, but still ultimately centralizing, path.',
    'A shift to the ''council_communist_reading'' would lead to a re-evaluation of all centralized political structures as extractive, emphasizing direct, decentralized forms of power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta_council_communist, conceptual, 'Impact of adopting the ''council_communist_reading'' on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(mani_tr_t1950, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(mani_tr_t2000, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(mani_tr_t2024, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(mani_be_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(mani_be_t1950, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(mani_be_t2000, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(mani_be_t2024, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(mani_su_t1950, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(mani_su_t2000, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(mani_su_t2024, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'manifesto_revolutionary_method' kernel, focusing on democratic and gradual transformation. It is structurally distinct from the 'vanguard_rupture_reading' and 'council_communist_reading' which represent alternative methods of socialist transformation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
