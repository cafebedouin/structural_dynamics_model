% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment: Militia-Conditioned Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'militia-conditioned' reading of the
 *   Second Amendment, where the prefatory clause ('A well regulated Militia,
 *   being necessary to the security of a free State') defines the scope of
 *   the operative clause ('the right of the people to keep and bear Arms') to
 *   a collective defense context. This interpretation permits comprehensive
 *   state regulation of firearms, viewing private possession as subservient
 *   to the collective good. The claimed type is 'tangled_rope' because it
 *   serves a genuine coordination function (public safety) but also involves
 *   asymmetric extraction from gun owners through regulation and restriction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.65).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.75).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment: Militia-Conditioned Reading").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, 'a62d09f3-9575-47d8-84dc-9df1ecb5a79b').
narrative_ontology:cs_kernel_codification('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', fixed_text).
narrative_ontology:cs_authority_grounding('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', lineage).
narrative_ontology:cs_interpretation_layer_present('a62d09f3-9575-47d8-84dc-9df1ecb5a79b').
narrative_ontology:cs_reading_relation('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', second_amendment_boundary__insurrectionist_reading, forecloses).
narrative_ontology:cs_axiom('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', foundational, militia_clause_defines_scope).
narrative_ontology:cs_axiom_status(militia_clause_defines_scope, holdable).
narrative_ontology:cs_axiom_grounding('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', militia_clause_defines_scope, conventional).
narrative_ontology:cs_axiom('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', foundational, state_police_power_supremacy).
narrative_ontology:cs_axiom_status(state_police_power_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', state_police_power_supremacy, conventional).
narrative_ontology:cs_reference_frame('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', collective_defense_originalism).
narrative_ontology:cs_drift_state('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', contemporary_individual_right_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a62d09f3-9575-47d8-84dc-9df1ecb5a79b', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, general_public).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_owners_subject_to_restriction).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_manufacturers_retailers).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, state_police_power_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, collective_security_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These bodies are empowered to enact comprehensive firearms regulations, interpreting the Second Amendment as primarily concerned with collective defense. They benefit from the ability to address public safety concerns through legislation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_legislatures, agenda_setter,
    institutional, generational, mobile, national).

% Benefits from the perceived increase in public safety and reduction in gun violence that comprehensive firearms regulation aims to achieve. Experiences a more ordered society, but also potentially reduced access to firearms for personal use.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).

% Actively lobby for and support regulations based on this reading, seeing it as essential for community well-being. They benefit from the legitimacy granted to their policy goals.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear the costs of compliance with regulations, including restrictions on types of firearms, magazine capacity, and carry permits. Their ability to 'keep and bear Arms' for individual purposes is curtailed, leading to a sense of extraction.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_owners_subject_to_restriction, payer,
    moderate, biographical, constrained, national).

% Face market restrictions, product bans, and increased regulatory burdens, impacting their business models and profitability. They bear direct economic costs from this reading's enforcement.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_manufacturers_retailers, payer,
    powerful, biographical, constrained, national).

% Argue for an expansive individual right to bear arms, independent of militia service. They are structurally excluded from the core interpretive framework of this reading, their arguments often dismissed as misinterpretations of the text.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, individual_right_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicate challenges to firearms regulations, interpreting the Second Amendment. While they are the ultimate arbiters, their role here is to observe and apply the legal framework, not to set the policy agenda directly.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:fixing_cost_class(second_amendment_boundary__militia_conditioned_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate public safety and order by establishing a clear constitutional basis for state and federal governments to regulate firearms, ensuring that armed capacity serves collective defense rather than individual whim.
% TRANSFER_FUNCTION: Transfers significant regulatory authority over firearms from individuals to state and federal governments, and transfers the costs of compliance and restricted access to gun owners and the firearms industry, in exchange for perceived public safety benefits.
% ABSENT_VOICES: Advocates for an individual, unrestricted right to bear arms are largely absent from the foundational interpretive framework of this reading. They would argue that the right is inherent and not conditioned by militia service, and that current regulations are unconstitutional infringements.
% DISAPPEARANCE_RATIONALE: If this militia-conditioned reading vanished overnight, the constitutional basis for comprehensive firearms regulation would collapse. State and federal governments would lose significant police power over firearms, leading to a dramatic shift towards unregulated or minimally regulated gun ownership, fundamentally altering public safety and social order.
% FOUNDING_PROBLEM: The founding problem was to define the role of armed citizens in a new republic, balancing the need for collective security (via a militia) with concerns about standing armies and individual liberty, ensuring that the right to bear arms served the common good.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians, legal scholars, and public safety organizations corroborate that the debate over the Second Amendment's scope, particularly regarding collective versus individual rights, remains a live and contested issue, reflecting ongoing societal concerns about both security and liberty. Early judicial interpretations and historical context support the militia-conditioned view.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.65) is substantial because gun owners face significant restrictions and costs, while the general public benefits from perceived safety. Suppression (0.75) is high due to active legal enforcement and the judicial backing this reading historically received. Theater ratio (0.15) is low, as the regulatory efforts are genuinely aimed at public safety, not merely performative. Accessibility collapse (0.60) reflects that alternatives to regulated gun ownership are legally foreclosed, but the political and legal debate over this foreclosure remains active. Resistance (0.70) is high, driven by strong advocacy from individual rights groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state legislatures and public safety advocates, this reading functions as a legitimate 'rope' or 'scaffold' for public order. However, from the perspective of gun owners and the firearms industry, it operates as a 'snare' or 'tangled_rope', extracting rights and economic opportunities. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislatures and public safety advocates are clear beneficiaries, gaining regulatory power and perceived safety. The general public is also a beneficiary, though they may indirectly bear some costs. Gun owners and the firearms industry are the primary targets/payers, facing direct restrictions and economic burdens. Individual right advocates are excluded, as their core premise is rejected by this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_vs_individual_scope,
    'Is the Second Amendment''s primary purpose collective defense through a militia, or an individual right to bear arms independent of militia service?',
    'Further Supreme Court rulings that explicitly re-evaluate the historical and textual basis of the prefatory and operative clauses, or a constitutional amendment clarifying the right''s scope.',
    'If resolved towards an individual right, this reading''s legitimacy would collapse, leading to a reclassification towards a Snare for the state; if reaffirmed as collective, its Rope/Tangled Rope classification would strengthen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_vs_individual_scope, conceptual, 'The core interpretive ambiguity of the Second Amendment''s scope.').

omega_variable(
    efficacy_of_regulation,
    'Do comprehensive firearms regulations, enabled by this reading, demonstrably reduce gun violence and enhance public safety without disproportionately infringing on legitimate self-defense?',
    'Longitudinal empirical studies comparing public safety outcomes in jurisdictions with varying regulatory regimes, controlling for confounding socioeconomic factors.',
    'Strong empirical evidence of efficacy would bolster the coordination function and reduce perceived extraction; weak or negative evidence would undermine its legitimacy and push it towards a Snare for gun owners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_regulation, empirical, 'Empirical effectiveness of regulations in achieving public safety goals.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of individual gun ownership primarily structural (legal barriers) or internalized (social norms against gun ownership)?',
    'Post-exit suppression trajectory: if suppression persists after legal restrictions are removed (e.g., through cultural shifts), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gun ownership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1900, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1934, 0.12).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(seco_tr_t1986, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1986, 0.18).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(seco_be_t1900, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(seco_be_t1934, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1934, 0.5).
narrative_ontology:measurement(seco_be_t1968, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1968, 0.58).
narrative_ontology:measurement(seco_be_t1986, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1986, 0.62).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2008, 0.68).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1900, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(seco_su_t1934, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1934, 0.6).
narrative_ontology:measurement(seco_su_t1968, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement(seco_su_t1986, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1986, 0.75).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2008, 0.8).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_boundary' kernel, each representing a distinct structural interpretation of the Second Amendment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
