% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment Boundary: Individual Right Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'individual right' reading of the
 *   Second Amendment, where the operative clause establishes a pre-existing
 *   individual right to bear arms, and the prefatory militia clause states a
 *   purpose but does not limit the right's scope. This reading has gained
 *   prominence through judicial interpretation, particularly since the late
 *   20th century, leading to significant constitutional protection for
 *   private firearm possession and a corresponding limitation on state
 *   regulatory power. The constraint is claimed as a 'tangled_rope' because
 *   it provides a coordination function for gun owners and manufacturers
 *   while simultaneously extracting significant costs from victims of gun
 *   violence and public safety advocates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.75).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Boundary: Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, 'deefd7f7-fa8a-436e-8da4-5e4d2187cf6a').
narrative_ontology:cs_kernel_codification('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', fixed_text).
narrative_ontology:cs_authority_grounding('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', lineage).
narrative_ontology:cs_interpretation_layer_present('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a').
narrative_ontology:cs_reading_relation('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', foundational, individual_right_pre_exists_state).
narrative_ontology:cs_axiom_status(individual_right_pre_exists_state, holdable).
narrative_ontology:cs_axiom_grounding('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', individual_right_pre_exists_state, deontological).
narrative_ontology:cs_axiom('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', foundational, militia_clause_is_prefatory_only).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory_only, holdable).
narrative_ontology:cs_axiom_grounding('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', militia_clause_is_prefatory_only, conventional).
narrative_ontology:cs_reference_frame('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', post_heller_individual_right).
narrative_ontology:cs_drift_state('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('deefd7f7-fa8a-436e-8da4-5e4d2187cf6a', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_manufacturers_and_retailers).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, suicide_completers_with_firearm_access).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, public_safety_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who assert a constitutional right to own firearms for self-defense, sport, and other purposes, largely unconstrained by state regulation. Their identity is often fused with this right, making any perceived infringement a fundamental challenge.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Businesses that profit from the sale and distribution of firearms and accessories. This reading provides a strong constitutional shield against regulation that would limit their market, ensuring a robust demand for their products.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_manufacturers_and_retailers, beneficiary,
    powerful, generational, arbitrage, national).

% Individuals and communities directly harmed by gun violence, particularly in mass casualty events. They bear the ultimate cost of permissive firearm access, with no effective exit from the risk.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims, payer,
    powerless, immediate, trapped, local).

% Individuals, predominantly women, who are at increased risk of homicide when firearms are present in domestic disputes. They are trapped by the immediate threat and the legal framework that prioritizes firearm access.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, local).

% Individuals in crisis who have immediate access to firearms, which significantly increases the lethality of suicide attempts. The ease of access, protected by this reading, contributes to tragic outcomes.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, suicide_completers_with_firearm_access, payer,
    powerless, immediate, trapped, local).

% Organizations and individuals who lobby for stricter gun control measures to reduce violence. They bear the cost of continuous legislative and legal battles against a constitutionally entrenched right, facing significant political and financial hurdles.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_safety_advocates, payer,
    organized, generational, constrained, national).

% Government bodies tasked with enacting laws for public safety. Under this reading, their ability to regulate firearms is severely curtailed, often leading to legal challenges and preemption by federal courts.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_legislatures, agenda_setter,
    institutional, biographical, constrained, regional).

% Judicial bodies that interpret the Second Amendment and adjudicate challenges to firearms regulations. This reading empowers them to strike down state and federal laws deemed to infringe on individual gun ownership.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the legal framework for individual firearm ownership, providing a clear (though contested) standard for what constitutes a protected right, reducing ambiguity for owners and manufacturers.
% TRANSFER_FUNCTION: Transfers the burden of gun violence from individual firearm owners and the firearms industry to the general public, particularly victims of gun violence, by limiting the state's ability to regulate access.
% ABSENT_VOICES: Future generations who will inherit the consequences of current firearms policy, and those who are silenced by gun violence itself, are absent from the direct legal and political discourse, though their interests are represented by public safety advocates.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment vanished overnight, state and federal governments would immediately move to enact comprehensive firearms regulations, significantly altering the landscape of gun ownership, manufacturing, and public safety. The firearms market would contract, and the legal battles would shift to different constitutional grounds.
% FOUNDING_PROBLEM: The founding problem was to ensure the right of the people to keep and bear arms, particularly in the context of a well-regulated militia, for the security of a free state.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historians outside the immediate beneficiary groups attest that the original intent was primarily tied to militia service, while contemporary proponents of this reading argue it was always an individual right. Supreme Court precedent (Heller, McDonald) has affirmed the individual right, but historical and textual debates persist among constitutional scholars.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because the costs of gun violence (deaths, injuries, societal fear) are borne by victims and society at large, while the benefits of unrestricted access accrue to a specific group. Suppression (0.75) is also high, as legislative efforts to regulate firearms are actively suppressed by judicial review and political lobbying, limiting alternatives for public safety. The theater ratio (0.20) is relatively low, as the enforcement (judicial rulings, lobbying) is genuinely aimed at protecting the perceived right, not merely performing a function. Accessibility collapse (0.40) is moderate, as some regulatory alternatives exist but are severely constrained. Resistance (0.80) is high, reflecting the intense and ongoing efforts by public safety advocates to challenge this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of firearms owners and manufacturers, this reading is a 'rope' or even a 'mountain'—a fundamental right that coordinates their activities and protects them from arbitrary state power. From the perspective of victims and public safety advocates, it operates as a 'snare' or 'tangled_rope,' extracting immense costs and suppressing efforts to mitigate harm. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Firearms owners and manufacturers are clear beneficiaries (low d) as the constraint shields their activities. Victims of gun violence and public safety advocates are clear targets (high d) as they bear the costs and face suppressed alternatives. State legislatures and federal courts act as agenda-setters, interpreting and enforcing the constraint, but their actions are heavily influenced by the beneficiaries' political power and the constraint's judicial entrenchment.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging the genuine coordination function for firearm owners while simultaneously recognizing the asymmetric extraction from other groups. It avoids treating the constitutional right as a 'mountain' (natural law) by documenting its active enforcement and the identifiable beneficiaries and victims, which are hallmarks of a constructed constraint. The 'contested' status of the founding problem further highlights that the constraint's current operation may have drifted from its original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_individual_right,
    'What types of arms and what contexts of use are protected by the individual right, and what limits can be placed on them without infringing the right?',
    'Further Supreme Court rulings clarifying the ''dangerous and unusual weapons'' test and the scope of ''sensitive places'' or ''responsible gun ownership'' regulations.',
    'A narrower interpretation would reduce extractiveness from victims by allowing more regulation; a broader interpretation would increase extractiveness by further shielding firearm access.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_individual_right, conceptual, 'Ambiguity regarding the precise boundaries of the individual right to bear arms.').

omega_variable(
    causal_link_to_violence,
    'To what extent does the permissive legal framework (stemming from this reading) directly cause or exacerbate gun violence, versus other socio-economic factors?',
    'Longitudinal epidemiological studies, comparative analyses across jurisdictions with different gun laws, and robust statistical modeling controlling for confounding variables.',
    'Stronger evidence of direct causation would strengthen the case for re-evaluating the constraint''s extractiveness and suppression, potentially shifting its classification towards a ''snare'' for victims. Weaker evidence might support a ''rope'' framing by emphasizing other factors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_link_to_violence, empirical, 'The degree to which this reading''s legal effects contribute to gun violence outcomes.').

omega_variable(
    identity_fusion_durability,
    'How deeply is the identity of ''firearms owner'' fused with the constitutional right, and how resistant is this fusion to shifts in legal interpretation or social norms?',
    'Sociological studies of gun culture, psychological research on identity formation and political behavior, and analysis of responses to past and potential legal changes.',
    'If identity fusion is extremely durable, efforts to alter the constraint will face entrenched resistance, increasing the effective suppression. If it is more malleable, the constraint may be more amenable to change through shifts in public discourse or legal precedent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_durability, empirical, 'The strength and malleability of identity-lock for firearms owners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_boundary__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1985, second_amendment_boundary__individual_right_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_boundary__individual_right_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_boundary__individual_right_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__individual_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_boundary__individual_right_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(seco_be_t1985, second_amendment_boundary__individual_right_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(seco_be_t2000, second_amendment_boundary__individual_right_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__individual_right_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(seco_be_t2016, second_amendment_boundary__individual_right_reading, base_extractiveness, 2016, 0.67).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__individual_right_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_boundary__individual_right_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(seco_su_t1985, second_amendment_boundary__individual_right_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(seco_su_t2000, second_amendment_boundary__individual_right_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__individual_right_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(seco_su_t2016, second_amendment_boundary__individual_right_reading, suppression_requirement, 2016, 0.73).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__individual_right_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
