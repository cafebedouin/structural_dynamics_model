% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Bear Arms for Self-Defense
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, where the operative clause guarantees an individual's right to
 *   possess firearms for personal self-defense, independent of militia
 *   service. This reading has gained prominence in recent decades, shaping
 *   firearms policy and legal challenges. It is a reading of a contested
 *   kernel, where other interpretations (collective security, civic virtue)
 *   offer different structural implications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.65).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.75).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment: Individual Right to Bear Arms for Self-Defense").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27').
narrative_ontology:cs_kernel_codification('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', fixed_text).
narrative_ontology:cs_authority_grounding('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', lineage).
narrative_ontology:cs_interpretation_layer_present('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27').
narrative_ontology:cs_reading_relation('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', foundational, individual_self_defense_is_fundamental_right).
narrative_ontology:cs_axiom_status(individual_self_defense_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', individual_self_defense_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', foundational, militia_clause_is_prefatory_not_limiting).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory_not_limiting, holdable).
narrative_ontology:cs_axiom_grounding('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', militia_clause_is_prefatory_not_limiting, conventional).
narrative_ontology:cs_reference_frame('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', post_heller_jurisprudence).
narrative_ontology:cs_drift_state('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', contemporary_mass_shooting_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('1b7db0ed-a9c7-4fa4-9f67-cc555cd42e27', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, disarmed_populations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, public_safety_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who own firearms for self-defense, recreation, or collection. They benefit from the broad protection of gun ownership rights and actively resist any attempts to restrict these rights. Their identity is often tied to gun ownership as a fundamental liberty.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Manufacturers, distributors, and retailers of firearms and ammunition. They benefit directly from the expansive interpretation of gun rights, which drives demand for their products. They actively lobby against gun control measures.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Individuals legally prohibited from owning firearms (e.g., convicted felons, those with domestic violence restraining orders) or those who choose not to own them but are exposed to increased gun violence. They bear the costs of the broad availability of firearms without the means of self-defense, and their voices are often excluded from policy debates.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, disarmed_populations, payer,
    powerless, immediate, trapped, local).

% Organizations and individuals who advocate for stricter gun control measures to reduce gun violence. They bear the costs of legislative inaction and the societal impact of widespread firearm access, constantly working against the prevailing interpretation of the Second Amendment.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, public_safety_advocates, payer,
    organized, generational, constrained, national).

% The courts, particularly the Supreme Court, interpret the Second Amendment and establish legal precedents that define the scope of gun rights. They are the primary arbiters of this constraint's meaning and enforcement, shaping its impact on all other stakeholders.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Elected representatives tasked with creating laws. Their ability to enact gun control legislation is severely constrained by judicial interpretations of the Second Amendment, often leading to legislative gridlock on firearms policy.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, legislature, excluded,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for individual self-defense and ensures access to firearms for this purpose, theoretically coordinating individual security against threats.
% TRANSFER_FUNCTION: Transfers the right to bear arms to individuals, and implicitly transfers the burden of managing gun violence and its consequences to the broader public and public safety institutions.
% ABSENT_VOICES: Victims of gun violence and their families, as well as those who advocate for a more restrictive interpretation of gun rights, are often marginalized in policy debates, their concerns overridden by the dominant individual rights framework.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment vanished, the legal landscape for firearms would fundamentally shift. Gun control legislation would likely become much more expansive, the firearms industry would face severe restrictions, and the societal approach to gun violence would be radically altered.
% FOUNDING_PROBLEM: The founding problem was to ensure the security of a free state, including the right of the people to keep and bear arms, often understood in the context of a citizen militia.
% FOUNDING_PROBLEM_CORROBORATION: Individual gun owners and the firearms industry argue the problem of individual self-defense remains live. Public safety advocates and many legal scholars argue the original problem of militia service is largely obsolete in its founding context, and the current interpretation has expanded beyond its original intent, leading to new problems. Historical analysis and legal scholarship from outside the benefiting parties corroborate the shift in interpretation and the contested status of the founding problem.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading prioritizes individual gun ownership, leading to a high societal cost in terms of gun violence and public safety concerns, which are borne by the general public and specific victim groups. Suppression is also high (0.75) due to the active legal and political efforts required to resist gun control measures and maintain broad access to firearms. The 'individual right' reading actively suppresses alternative interpretations and legislative efforts to restrict gun ownership. Theater ratio is low (0.20) as the constraint is actively enforced and defended, with real consequences, rather than being merely performative.
 *
 * PERSPECTIVAL GAP:
 *   Individual gun owners experience this as a fundamental protection of liberty, while public safety advocates and disarmed populations experience it as a source of insecurity and a barrier to effective public policy. The legal system, particularly the judiciary, acts as the agenda-setter, mediating these conflicting perspectives through interpretation and precedent.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are primary beneficiaries (d near 0.0) as the constraint protects their right to own and sell firearms, respectively. Disarmed populations (e.g., felons, domestic abusers) and public safety advocates are victims (d near 1.0) as they bear the costs of increased gun violence and the inability to enact stricter gun control. The general public is a mixed beneficiary/victim, benefiting from the theoretical right to self-defense but bearing the costs of widespread firearm access.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, under this reading, is to protect individual self-defense. However, the high extractiveness and suppression suggest that this mandate has been leveraged to create a system that disproportionately benefits specific groups while imposing significant costs on others. It prevents mislabeling by highlighting the active enforcement and suppression required to maintain this specific interpretation, rather than treating it as a natural or universally beneficial right.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine individual right, or is it a reading of the Second Amendment that prioritizes individual ownership over collective security?',
    'Historical and legal scholarship on original intent, and judicial rulings that explicitly reconcile or reject competing interpretations.',
    'If primarily a reading, its classification shifts from a fundamental right (closer to Mountain) to a constructed constraint (Tangled Rope or Snare) whose beneficiaries are identifiable and whose persistence depends on active enforcement and suppression of alternative readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the ''second_amendment_text'' kernel, specifically the ''individual_right_reading''. Sibling readings (''collective_security_reading'', ''originalist_civic_virtue_reading'') would shift the beneficiary/victim structure and the perceived extractiveness.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of gun control efforts structural (legal precedent, legislative gridlock) or internalized (ideological commitment to gun ownership as a core liberty)?',
    'Analysis of public opinion shifts after major gun violence events vs. legislative outcomes; if legislative outcomes remain static despite public opinion shifts, structural suppression is dominant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as it operates through self-reinforcing ideological commitment. If structural, legal and political reforms could more readily alter the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gun control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__individual_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t10, second_amendment_text__individual_right_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(seco_tr_t20, second_amendment_text__individual_right_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(seco_tr_t30, second_amendment_text__individual_right_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__individual_right_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t10, second_amendment_text__individual_right_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(seco_be_t20, second_amendment_text__individual_right_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(seco_be_t30, second_amendment_text__individual_right_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__individual_right_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(seco_su_t10, second_amendment_text__individual_right_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(seco_su_t20, second_amendment_text__individual_right_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(seco_su_t30, second_amendment_text__individual_right_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, self_defense_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_text' kernel. It focuses on the individual right to bear arms for self-defense, distinct from the 'collective_security_reading' and 'originalist_civic_virtue_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
