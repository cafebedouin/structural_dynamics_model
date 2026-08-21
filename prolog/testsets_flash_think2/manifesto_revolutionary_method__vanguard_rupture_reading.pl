% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__vanguard_rupture_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: manifesto_revolutionary_method__vanguard_rupture_reading
 *   human_readable: Vanguard Party Seizure of State Power (Vanguard Rupture Reading)
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This constraint, the 'vanguard_rupture_reading' of the
 *   'manifesto_revolutionary_method' kernel, posits that revolutionary
 *   transformation necessitates the organized seizure of state power by a
 *   vanguard party, establishing a 'dictatorship of the proletariat' as a
 *   transitional state. This reading emphasizes centralized control and the
 *   suppression of alternative political pathways to achieve a communist
 *   society. The high extractiveness and suppression reflect the coercive
 *   nature of this transitional state, which, while claiming a coordination
 *   function (guiding the revolution), operates with significant asymmetric
 *   extraction from those it governs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.75).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.85).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure of State Power (Vanguard Rupture Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, '8aac7c89-cee4-4799-869b-6baae3eb5a07').
narrative_ontology:cs_kernel_codification('8aac7c89-cee4-4799-869b-6baae3eb5a07', formalized).
narrative_ontology:cs_authority_grounding('8aac7c89-cee4-4799-869b-6baae3eb5a07', lineage).
narrative_ontology:cs_interpretation_layer_present('8aac7c89-cee4-4799-869b-6baae3eb5a07').
narrative_ontology:cs_reading_relation('8aac7c89-cee4-4799-869b-6baae3eb5a07', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_reading_relation('8aac7c89-cee4-4799-869b-6baae3eb5a07', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_axiom('8aac7c89-cee4-4799-869b-6baae3eb5a07', foundational, vanguard_party_historical_necessity).
narrative_ontology:cs_axiom_status(vanguard_party_historical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('8aac7c89-cee4-4799-869b-6baae3eb5a07', vanguard_party_historical_necessity, empirically_contingent).
narrative_ontology:cs_axiom('8aac7c89-cee4-4799-869b-6baae3eb5a07', foundational, dictatorship_of_proletariat_transitional_necessity).
narrative_ontology:cs_axiom_status(dictatorship_of_proletariat_transitional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('8aac7c89-cee4-4799-869b-6baae3eb5a07', dictatorship_of_proletariat_transitional_necessity, instrumental).
narrative_ontology:cs_reference_frame('8aac7c89-cee4-4799-869b-6baae3eb5a07', historical_materialist_necessity).
narrative_ontology:cs_drift_state('8aac7c89-cee4-4799-869b-6baae3eb5a07', post_soviet_collapse_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('8aac7c89-cee4-4799-869b-6baae3eb5a07', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, international_communist_movement).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, bourgeoisie).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The core leadership and ideological interpreters of the revolutionary movement. They seize state power, guide the 'dictatorship of the proletariat,' and benefit directly from centralized control and ideological authority.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres, agenda_setter,
    institutional, generational, arbitrage, global).

% The bureaucratic and technical structures that implement the vanguard party's economic and social directives. They benefit from centralized control over resources and the elimination of market competition.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, generational, constrained, national).

% Other communist parties and movements globally that gain ideological validation, material support, and strategic guidance from the success of a vanguard-led revolution. Their legitimacy is bolstered by this model.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, international_communist_movement, beneficiary,
    organized, generational, mobile, global).

% Individuals and groups advocating for multi-party democracy, freedom of speech, and alternative political pathways. They are actively suppressed, denied political voice, and often face imprisonment, exile, or elimination under the dictatorship of the proletariat.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    powerless, immediate, trapped, national).

% Independent trade unions, workers' councils, and other self-organized labor groups that seek direct democratic control over production. They are either co-opted into party-controlled structures or suppressed to prevent challenges to the vanguard's authority.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    powerless, immediate, trapped, local).

% The capitalist class, including property owners, industrialists, and financiers. They are dispossessed of their assets, stripped of political power, and targeted for elimination as a class enemy during the revolutionary transition.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, bourgeoisie, payer,
    powerless, immediate, trapped, national).

% Socialist thinkers and movements who believe in achieving socialism through democratic electoral processes and gradual reforms within existing state structures. Their methods are explicitly rejected as 'reformist' and 'bourgeois' by the vanguard reading.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, democratic_gradualists, excluded,
    organized, biographical, mobile, global).

% Advocates for direct worker control through federated councils (soviets) as the primary form of governance, rejecting both the capitalist state and the vanguard party's centralized authority. They are dismissed as 'utopian' or 'anarchist' by the vanguard reading.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, council_communists, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes political and economic power under the vanguard party to effectively dismantle capitalist structures, suppress counter-revolution, and guide society through the transitional phase of the dictatorship of the proletariat towards communism.
% TRANSFER_FUNCTION: Transfers all political authority and economic control from existing state and civil society institutions to the vanguard party and its state apparatus, extracting resources and labor for state-directed development.
% ABSENT_VOICES: Political pluralists, autonomous worker organizations, and other socialist tendencies (e.g., democratic gradualists, council communists) are actively suppressed or excluded from the revolutionary process, as their alternative pathways are deemed counter-revolutionary or ineffective.
% DISAPPEARANCE_RATIONALE: If the vanguard party's seizure of state power and the subsequent dictatorship vanished overnight, the state would likely collapse into chaos or be immediately replaced by alternative political forces, leading to a radically different trajectory for social transformation.
% FOUNDING_PROBLEM: The perceived inability of spontaneous worker movements or gradual democratic reforms to overcome entrenched capitalist power and achieve a communist society, necessitating a disciplined, centralized force to lead the revolution.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is primarily attested by adherents within the vanguard party's own ideological tradition, citing historical failures of reformism and spontaneous uprisings. External corroboration from independent historians or political scientists often frames it as a post-hoc rationalization for authoritarian power consolidation.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) stems from the party's appropriation of economic and political power, directing resources and labor according to its plan, often at the expense of individual liberties and economic autonomy. Suppression (0.85) is severe due to the explicit 'dictatorship' and the active elimination of political opposition and autonomous organizations. Theater ratio is low (0.15) because the enforcement and ideological control are genuinely functional, not merely performative. Accessibility collapse is high (0.80) as alternative political and economic systems are systematically dismantled. Resistance (0.70) is also high, reflecting the inevitable opposition to such a coercive transformation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the vanguard party, this constraint is a necessary, albeit temporary, coordination mechanism to achieve a higher social good. From the perspective of the victims and excluded groups, it is a highly extractive and suppressive snare, leveraging a coordination narrative to justify authoritarian control. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Vanguard party cadres and the state planning apparatus are clear beneficiaries, gaining direct power and control. The international communist movement also benefits from ideological validation. Political pluralists, autonomous worker organizations, and the bourgeoisie are direct targets and victims, facing suppression and dispossession. Democratic gradualists and council communists are excluded, their alternative visions actively rejected.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transitional_state_duration_ambiguity,
    'Is the ''dictatorship of the proletariat'' genuinely a temporary, transitional state, or does it tend towards permanent authoritarianism?',
    'Empirical observation of historical cases: if no historical instance has successfully transitioned from dictatorship of the proletariat to a stateless, classless society, the claim of ''transitional'' is empirically falsified.',
    'If proven to be non-transitional, the constraint''s coordination function (guiding to communism) collapses, reclassifying it as a pure snare of permanent extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transitional_state_duration_ambiguity, empirical, 'Whether the ''transitional'' nature of the dictatorship of the proletariat is borne out by history.').

omega_variable(
    alternative_pathways_efficacy,
    'Are alternative, non-vanguard-led pathways (e.g., democratic gradualism, council communism) genuinely incapable of achieving socialist transformation, or are they suppressed to maintain party power?',
    'Comparative historical analysis of successful and unsuccessful revolutionary movements, including those that did not adopt the vanguard model, and analysis of the mechanisms used to suppress alternatives within vanguard-led states.',
    'If alternative pathways are shown to be viable and suppressed primarily for party power, the constraint''s coordination claim is weakened, increasing its effective extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathways_efficacy, empirical, 'The true efficacy of alternative revolutionary pathways versus their suppression by the vanguard.').

omega_variable(
    vanguard_party_legitimacy_grounding,
    'Is the vanguard party''s claim to historical necessity and exclusive guidance genuinely accepted by the proletariat it claims to represent, or is it imposed through coercion and ideological control?',
    'Sociological studies of popular support, analysis of dissent and resistance within vanguard-led states, and examination of the mechanisms of ideological indoctrination versus genuine consent.',
    'If legitimacy is primarily coercive, the constraint''s claimed coordination function is further undermined, reinforcing its classification as a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vanguard_party_legitimacy_grounding, empirical, 'The basis of the vanguard party''s legitimacy: consent vs. coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mani_tr_t6, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(mani_tr_t12, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(mani_tr_t18, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(mani_tr_t24, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(mani_be_t6, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(mani_be_t12, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(mani_be_t18, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 18, 0.73).
narrative_ontology:measurement(mani_be_t24, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(mani_su_t6, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(mani_su_t12, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 12, 0.83).
narrative_ontology:measurement(mani_su_t18, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 18, 0.85).
narrative_ontology:measurement(mani_su_t24, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 24, 0.85).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
