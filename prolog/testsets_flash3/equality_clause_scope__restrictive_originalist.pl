% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive Originalist Reading of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the 'restrictive originalist' reading of the
 *   equality clause, which interprets its scope as limited to propertied
 *   white males as political actors within the 18th-century social contract
 *   framework. This reading is one of several competing interpretations of
 *   the kernel 'equality_clause_scope'. It is characterized by a narrow
 *   beneficiary set and a high legitimacy threshold for expanding franchise
 *   and rights claims beyond the original intent, often requiring
 *   constitutional amendments.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.65).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.78).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.65).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive Originalist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '27b46215-a139-499b-abdc-4770d1c62ca7').
narrative_ontology:cs_kernel_codification('27b46215-a139-499b-abdc-4770d1c62ca7', fixed_text).
narrative_ontology:cs_authority_grounding('27b46215-a139-499b-abdc-4770d1c62ca7', lineage).
narrative_ontology:cs_interpretation_layer_present('27b46215-a139-499b-abdc-4770d1c62ca7').
narrative_ontology:cs_reading_relation('27b46215-a139-499b-abdc-4770d1c62ca7', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_reading_relation('27b46215-a139-499b-abdc-4770d1c62ca7', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('27b46215-a139-499b-abdc-4770d1c62ca7', foundational, original_public_meaning_supremacy).
narrative_ontology:cs_axiom_status(original_public_meaning_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('27b46215-a139-499b-abdc-4770d1c62ca7', original_public_meaning_supremacy, conventional).
narrative_ontology:cs_axiom('27b46215-a139-499b-abdc-4770d1c62ca7', foundational, amendment_as_sole_constitutional_change).
narrative_ontology:cs_axiom_status(amendment_as_sole_constitutional_change, holdable).
narrative_ontology:cs_axiom_grounding('27b46215-a139-499b-abdc-4770d1c62ca7', amendment_as_sole_constitutional_change, conventional).
narrative_ontology:cs_reference_frame('27b46215-a139-499b-abdc-4770d1c62ca7', eighteenth_century_social_contract).
narrative_ontology:cs_drift_state('27b46215-a139-499b-abdc-4770d1c62ca7', contemporary_civil_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('27b46215-a139-499b-abdc-4770d1c62ca7', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_males_historical).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, originalist_legal_scholars).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, racial_minorities).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_propertied_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the original political actors, they were the primary beneficiaries of the equality clause, which secured their rights and political participation within the social contract framework. Their status was elevated and protected by this interpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_males_historical, beneficiary,
    institutional, generational, arbitrage, national).

% Excluded from the original scope of equality, they bore the cost of limited political and civil rights. Their claims for equal treatment were not recognized under this framework, requiring separate and often arduous struggles for recognition.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women, payer,
    powerless, generational, identity_locked, national).

% Systematically denied equality and often subjected to chattel slavery, they were the most severely impacted victims of this restrictive interpretation. Their personhood and rights were fundamentally denied, requiring constitutional amendments and civil rights movements to challenge.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, racial_minorities, payer,
    powerless, generational, identity_locked, national).

% While not as severely excluded as women or racial minorities, they faced limitations on political participation (e.g., voting rights tied to property ownership) that were consistent with this narrow view of equality. Their path to full political agency was constrained.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_propertied_citizens, payer,
    moderate, biographical, constrained, national).

% Advocate for and interpret the equality clause strictly according to its perceived original public meaning in the 18th century. They actively enforce this narrow scope through legal arguments and judicial appointments, shaping constitutional jurisprudence.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_legal_scholars, agenda_setter,
    institutional, generational, analytical, national).

% Argue for a universal application of equality, seeing the original intent as morally flawed and historically contingent. Their arguments are often dismissed or marginalized within the restrictive originalist framework, requiring them to seek change through legislative or amendment processes.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansive_universalist_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a clear, albeit narrow, framework for political participation and rights among the founding generation, ensuring stability and order within the nascent republic's social contract.
% TRANSFER_FUNCTION: Transferred political power, legal recognition, and social status to propertied white males, while denying or limiting these to women, racial minorities, and non-propertied citizens.
% ABSENT_VOICES: Women, enslaved people, and indigenous populations were entirely absent from the constitutional convention and subsequent ratification debates; their perspectives would have fundamentally challenged the narrow scope of equality and the very foundation of the social contract.
% DISAPPEARANCE_RATIONALE: If this restrictive originalist reading vanished, the entire edifice of constitutional interpretation would shift. Arguments for universal rights would gain immediate and profound legal traction, overturning precedents and fundamentally altering the basis of civil rights law. The historical exclusions would lose their constitutional justification.
% FOUNDING_PROBLEM: To establish a stable republican government by defining the rights and political roles of citizens, ensuring order and preventing tyranny, while preserving existing social hierarchies and property relations.
% FOUNDING_PROBLEM_CORROBORATION: Historians and civil rights advocates widely corroborate that the original problem of establishing a stable republic for a narrow demographic has been superseded by the imperative of universal human rights. The persistence of this restrictive reading is seen as maintaining historical power imbalances, not solving a live problem for the broader populace.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.65) because this reading systematically denies rights and political agency to large segments of the population, channeling benefits to a narrow group. Suppression is also high (0.78) as this interpretation requires active legal and social enforcement to maintain historical exclusions against challenges from excluded groups. The theater ratio is moderate (0.20) as the 'coordination' story of a stable social contract serves to legitimize the underlying extraction. The temporal measurements show a gradual decrease in extractiveness and suppression over time, reflecting the impact of subsequent amendments and social movements, but the core restrictive interpretation still carries significant weight.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the original beneficiaries and current originalist scholars, this reading provides a stable, principled interpretation of the Constitution. From the perspective of historically excluded groups, it is a mechanism of ongoing oppression and denial of fundamental rights. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white males (historical) and originalist legal scholars are the primary beneficiaries and agenda-setters, respectively, as this reading directly serves their interests and interpretive framework. Women, racial minorities, and non-propertied citizens are the victims, bearing the costs of exclusion and limited rights. Expansive universalist advocates are excluded, as their arguments are fundamentally at odds with this reading's core tenets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_evolving_values,
    'Is the ''original intent'' of the equality clause a fixed, discoverable historical fact, or is it a construct reinterpreted through contemporary lenses and values?',
    'Extensive historical and linguistic analysis of 18th-century legal and political discourse, coupled with philosophical debate on the nature of constitutional interpretation.',
    'If fixed, this restrictive originalist reading gains stronger legitimacy. If a construct, its claims to historical fidelity are weakened, opening space for more expansive interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_evolving_values, conceptual, 'Ambiguity regarding the fixity and discoverability of original intent.').

omega_variable(
    amendment_as_sole_expansion_mechanism,
    'Is constitutional amendment the sole legitimate mechanism for expanding the scope of equality, or can judicial interpretation adapt the clause to evolving societal understandings of rights?',
    'Analysis of constitutional theory, judicial precedent, and the practicalities of the amendment process versus the need for timely rights recognition.',
    'If amendment is sole, this reading''s high legitimacy threshold for expansion is reinforced. If judicial adaptation is legitimate, the restrictive scope is challenged by interpretive flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_as_sole_expansion_mechanism, preference, 'Debate over the legitimate mechanisms for constitutional evolution regarding equality.').

omega_variable(
    historical_exclusion_as_foundational,
    'Is the historical exclusion of certain groups from the original equality framework an incidental historical fact, or is it foundational to the very definition of ''equality'' within this reading?',
    'Deep historical and philosophical inquiry into the conceptual underpinnings of 18th-century social contract theory and its inherent limitations.',
    'If incidental, the reading might be reformed to include more groups without abandoning originalism. If foundational, the reading is inherently discriminatory and cannot be reconciled with universalist claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_exclusion_as_foundational, conceptual, 'Whether historical exclusions are incidental or foundational to the restrictive originalist definition of equality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__restrictive_originalist, theater_ratio, 0, 0.2).
narrative_ontology:measurement(equa_tr_t60, equality_clause_scope__restrictive_originalist, theater_ratio, 60, 0.25).
narrative_ontology:measurement(equa_tr_t120, equality_clause_scope__restrictive_originalist, theater_ratio, 120, 0.3).
narrative_ontology:measurement(equa_tr_t180, equality_clause_scope__restrictive_originalist, theater_ratio, 180, 0.35).
narrative_ontology:measurement(equa_tr_t240, equality_clause_scope__restrictive_originalist, theater_ratio, 240, 0.4).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__restrictive_originalist, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(equa_be_t60, equality_clause_scope__restrictive_originalist, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(equa_be_t120, equality_clause_scope__restrictive_originalist, base_extractiveness, 120, 0.55).
narrative_ontology:measurement(equa_be_t180, equality_clause_scope__restrictive_originalist, base_extractiveness, 180, 0.5).
narrative_ontology:measurement(equa_be_t240, equality_clause_scope__restrictive_originalist, base_extractiveness, 240, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__restrictive_originalist, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(equa_su_t60, equality_clause_scope__restrictive_originalist, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(equa_su_t120, equality_clause_scope__restrictive_originalist, suppression_requirement, 120, 0.6).
narrative_ontology:measurement(equa_su_t180, equality_clause_scope__restrictive_originalist, suppression_requirement, 180, 0.5).
narrative_ontology:measurement(equa_su_t240, equality_clause_scope__restrictive_originalist, suppression_requirement, 240, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, civil_rights_legislation_legitimacy).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, voting_rights_act_interpretation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'equality_clause_scope' kernel. It represents the restrictive originalist interpretation, which limits equality to propertied white males. It is linked to 'expansive_universalist' and 'progressive_textualist' readings, which offer broader interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
