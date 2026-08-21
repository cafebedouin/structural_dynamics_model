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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive Originalist Reading of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the 'restrictive originalist' reading of
 *   constitutional equality clauses, which interprets equality as applying
 *   primarily to propertied white males as political actors within the
 *   18th-century social contract framework. This reading asserts that any
 *   expansion of rights or suffrage beyond this original scope requires
 *   explicit constitutional amendment, rather than judicial reinterpretation.
 *   It is one reading of the broader 'equality_clause_scope' kernel, which is
 *   contested by 'expansive_universalist' and 'progressive_textualist'
 *   readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.85).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.9).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.85).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive Originalist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '5d5a7e0d-921b-40bd-9ef7-35700cf93436').
narrative_ontology:cs_kernel_codification('5d5a7e0d-921b-40bd-9ef7-35700cf93436', fixed_text).
narrative_ontology:cs_authority_grounding('5d5a7e0d-921b-40bd-9ef7-35700cf93436', lineage).
narrative_ontology:cs_interpretation_layer_present('5d5a7e0d-921b-40bd-9ef7-35700cf93436').
narrative_ontology:cs_reading_relation('5d5a7e0d-921b-40bd-9ef7-35700cf93436', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('5d5a7e0d-921b-40bd-9ef7-35700cf93436', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('5d5a7e0d-921b-40bd-9ef7-35700cf93436', foundational, original_intent_supremacy).
narrative_ontology:cs_axiom_status(original_intent_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('5d5a7e0d-921b-40bd-9ef7-35700cf93436', original_intent_supremacy, conventional).
narrative_ontology:cs_axiom('5d5a7e0d-921b-40bd-9ef7-35700cf93436', foundational, limited_political_community).
narrative_ontology:cs_axiom_status(limited_political_community, holdable).
narrative_ontology:cs_axiom_grounding('5d5a7e0d-921b-40bd-9ef7-35700cf93436', limited_political_community, conventional).
narrative_ontology:cs_reference_frame('5d5a7e0d-921b-40bd-9ef7-35700cf93436', founding_era_social_contract).
narrative_ontology:cs_drift_state('5d5a7e0d-921b-40bd-9ef7-35700cf93436', contemporary_civil_rights_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('5d5a7e0d-921b-40bd-9ef7-35700cf93436', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_males).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_white_males).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_propertied_males).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, expansive_universalist_advocates).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, progressive_textualist_advocates).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_intent_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, states_rights_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, limited_government_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically defined the political community and held exclusive political power, benefiting from the limited scope of equality. They set the terms of the social contract and enforced its boundaries.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_males, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, propertied_white_males, beneficiary).

% Denied political equality, suffrage, and often property rights, bearing the direct costs of exclusion from the political community. Their voices were historically absent from the constitutional discourse.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, women, excluded).

% Systematically excluded from political participation, citizenship, and basic rights, often subjected to chattel slavery or other forms of legal subjugation. They bore the most severe costs of this restrictive interpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_white_males, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, non_white_males, excluded).

% Initially denied suffrage and full political participation based on property qualifications, though their status evolved more quickly than other excluded groups. They paid the cost of limited political agency.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_propertied_males, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, non_propertied_males, excluded).

% Interpret constitutional equality clauses strictly according to the perceived original intent of the framers, thereby maintaining a narrow scope of application and requiring explicit amendments for rights expansion. They actively enforce this interpretation through judicial decisions.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, restrictive_originalist_judges, agenda_setter,
    institutional, generational, analytical, national).

% Challenge the restrictive interpretation, arguing for a universal application of equality based on inherent human rights. They bear the costs of sustained advocacy and legal battles against the entrenched originalist view.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansive_universalist_advocates, observer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, expansive_universalist_advocates, payer).

% Argue that while the text contains an equality principle, its application expands through democratic processes (amendments) rather than judicial reinterpretation of original intent. They bear the costs of advocating for legislative and amendment-based change.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, progressive_textualist_advocates, observer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, progressive_textualist_advocates, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To define the legitimate political community and establish a stable social contract among its members, ensuring their liberties and property rights within the 18th-century framework.
% TRANSFER_FUNCTION: Transfers political power, legal rights, and social status to propertied white males, while denying these to women, non-white males, and non-propertied males, thereby concentrating authority and resources.
% ABSENT_VOICES: Women, non-white males, and non-propertied males were structurally excluded from the constitutional convention and early interpretive processes. If present, they would have argued for a broader, more inclusive definition of equality and political participation.
% DISAPPEARANCE_RATIONALE: If this restrictive originalist interpretation vanished overnight, the foundational understanding of constitutional equality would be radically altered. The legal basis for historical exclusions would dissolve, leading to a fundamental reordering of political rights, citizenship, and the distribution of power, necessitating a complete re-evaluation of constitutional jurisprudence.
% FOUNDING_PROBLEM: To establish a stable republican government and secure the liberties of the 'people' (understood as propertied white males) against both internal factionalism and external threats, while managing the existing social hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars (outside the originalist movement) corroborate the historical context of limited suffrage and rights, and the framers' intent to create a government for a specific demographic. Originalist proponents assert the problem of maintaining constitutional fidelity to this original understanding is still live, citing the need for judicial restraint against evolving social norms.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Base extractiveness is high (0.85) because this interpretation fundamentally denies political and legal equality to large segments of the population, concentrating power and rights. Suppression is also high (0.90) as this interpretation was historically enforced through legal structures, social norms, and violence, actively preventing excluded groups from claiming equal status or participating politically. Theater ratio is low (0.10) because the enforcement of this interpretation was direct and functional, not primarily performative. Accessibility collapse is high (0.88) as legal and social structures severely limited alternatives for those excluded. Resistance is high (0.75) reflecting the continuous struggle by marginalized groups to challenge and overcome this restrictive interpretation throughout history.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of propertied white males, this constraint was a legitimate and necessary foundation for a stable republic. From the perspective of excluded groups, it was a mechanism of profound injustice and extraction. The engine's classification will highlight this divergence by computing high extraction for victim seats and low/negative extraction for beneficiary seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white males are the clear beneficiaries and agenda-setters, as the constraint was designed to secure their position. Women, non-white males, and non-propertied males are the primary victims, bearing the costs of exclusion. Restrictive originalist judges act as agenda-setters by actively interpreting and enforcing this narrow scope. Advocates for other readings are observers who bear the cost of challenging the entrenched interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_knowability,
    'Is the ''original intent'' of the framers regarding equality truly knowable and consistently applicable across centuries, or is it a construct influenced by contemporary interpretive biases?',
    'Extensive historical and linguistic analysis of primary sources, coupled with meta-analysis of interpretive methodologies to identify anachronistic projections. However, full resolution is likely impossible due to the nature of historical inquiry.',
    'If original intent is largely unknowable or a construct, the legitimacy of this reading as a ''mountain'' of constitutional law is undermined, shifting it towards a ''snare'' or ''tangled_rope'' maintained by interpretive power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_knowability, conceptual, 'The epistemic status of ''original intent'' as a fixed interpretive anchor.').

omega_variable(
    amendment_impact_on_originalism,
    'How do subsequent constitutional amendments (e.g., 13th, 14th, 15th, 19th, 26th) alter or supersede the ''original intent'' of the initial equality clauses within a restrictive originalist framework?',
    'Legal analysis of the amendments'' text and ratification history, and their explicit or implicit impact on the scope of equality. This is a matter of ongoing legal debate and judicial interpretation.',
    'If amendments are seen as fundamentally altering the original scope, the ''restrictive originalist'' reading must either adapt to incorporate these changes as new ''original intents'' or acknowledge its own historical obsolescence regarding the initial clauses. If it denies their impact, its internal consistency is challenged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_impact_on_originalism, conceptual, 'The effect of later amendments on the originalist interpretation of equality.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''restrictive_originalist'' reading of the ''equality_clause_scope'' kernel. What specific structural elements would change if an ''expansive_universalist'' or ''progressive_textualist'' reading were adopted?',
    'Comparative legal analysis of judicial decisions and legislative actions under each reading. The impact on beneficiary/victim sets, extractiveness, and suppression would be directly observable.',
    'An ''expansive_universalist'' reading would dramatically broaden the beneficiary set and reduce extractiveness/suppression. A ''progressive_textualist'' reading would also broaden the beneficiary set, but emphasize democratic processes for change, potentially maintaining some suppression against judicial activism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural differences between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1787, equality_clause_scope__restrictive_originalist, theater_ratio, 1787, 0.1).
narrative_ontology:measurement(equa_tr_t1865, equality_clause_scope__restrictive_originalist, theater_ratio, 1865, 0.12).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__restrictive_originalist, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(equa_tr_t1965, equality_clause_scope__restrictive_originalist, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(equa_tr_t2024, equality_clause_scope__restrictive_originalist, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 1787, 0.85).
narrative_ontology:measurement(equa_be_t1865, equality_clause_scope__restrictive_originalist, base_extractiveness, 1865, 0.8).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__restrictive_originalist, base_extractiveness, 1920, 0.75).
narrative_ontology:measurement(equa_be_t1965, equality_clause_scope__restrictive_originalist, base_extractiveness, 1965, 0.7).
narrative_ontology:measurement(equa_be_t2024, equality_clause_scope__restrictive_originalist, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 1787, 0.9).
narrative_ontology:measurement(equa_su_t1865, equality_clause_scope__restrictive_originalist, suppression_requirement, 1865, 0.85).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__restrictive_originalist, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement(equa_su_t1965, equality_clause_scope__restrictive_originalist, suppression_requirement, 1965, 0.75).
narrative_ontology:measurement(equa_su_t2024, equality_clause_scope__restrictive_originalist, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, voting_rights_restrictions).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, property_qualifications_for_suffrage).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, citizenship_definitions).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, gender_based_legal_distinctions).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'equality_clause_scope' kernel. It is linked to sibling readings ('expansive_universalist', 'progressive_textualist') which offer alternative interpretations of the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
