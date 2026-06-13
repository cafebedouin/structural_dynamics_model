% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment Individual Right to Bear Arms
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, asserting that the right to keep and bear arms is a
 *   pre-existing individual liberty protected against federal infringement.
 *   This reading has gained significant legal traction, particularly since
 *   the late 20th century, and has profound implications for gun control
 *   legislation. It is one of several competing interpretations of the Second
 *   Amendment's meaning.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.65).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.4).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment Individual Right to Bear Arms").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'a83bef3b-e4e1-4869-bec0-4a60e8ec356f').
narrative_ontology:cs_kernel_codification('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', fixed_text).
narrative_ontology:cs_authority_grounding('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', lineage).
narrative_ontology:cs_interpretation_layer_present('a83bef3b-e4e1-4869-bec0-4a60e8ec356f').
narrative_ontology:cs_reading_relation('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', second_amendment_arms_right__civic_republican_reading, influences).
narrative_ontology:cs_axiom('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', foundational, individual_right_precedes_government).
narrative_ontology:cs_axiom_status(individual_right_precedes_government, holdable).
narrative_ontology:cs_axiom_grounding('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', individual_right_precedes_government, deontological).
narrative_ontology:cs_axiom('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', foundational, militia_clause_is_prefatory).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory, holdable).
narrative_ontology:cs_axiom_grounding('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', militia_clause_is_prefatory, conventional).
narrative_ontology:cs_reference_frame('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', founding_era_individual_liberty).
narrative_ontology:cs_drift_state('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a83bef3b-e4e1-4869-bec0-4a60e8ec356f', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_regulatory_agencies).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_legislatures_seeking_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their right to own firearms for self-defense and other lawful purposes is affirmed and protected against federal infringement, making them beneficiaries of this reading. They actively resist any legislation that restricts gun ownership.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Their ability to enact and enforce comprehensive federal gun control measures is significantly curtailed by this interpretation, forcing them to operate within narrow constitutional limits. They bear the cost of legislative and judicial challenges to their authority.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% While the individual right reading primarily constrains federal action, its principles often influence state-level jurisprudence, making it harder for states to enact strict gun control. They bear the political and legal costs of this constraint.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_legislatures_seeking_prohibition, payer,
    institutional, generational, constrained, national).

% Benefits from a broad interpretation of gun rights that ensures a robust market for firearms and accessories, with fewer federal restrictions on manufacturing, sales, and distribution. They actively lobby to maintain this reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    organized, generational, arbitrage, national).

% Advocate for stricter gun control measures to reduce violence. This reading of the Second Amendment structurally limits the policy options available to them, making their legislative goals difficult to achieve within the current legal framework.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_violence_prevention_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the historical, textual, and jurisprudential arguments for and against this reading, and its implications for constitutional theory and public policy. They are not directly affected by the constraint's operation but provide critical analysis.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the understanding of the Second Amendment as a fundamental individual right, providing a consistent legal framework for gun ownership and limiting government intervention.
% TRANSFER_FUNCTION: Transfers the power to regulate firearms from federal and, by extension, state governments to individual citizens, ensuring their ability to possess arms. It also transfers economic benefits to the firearms industry by protecting market access.
% ABSENT_VOICES: Advocates for a collective or civic republican right, who would argue that the individual right reading distorts the original intent and undermines public safety, are marginalized in the dominant legal discourse shaped by this reading.
% DISAPPEARANCE_RATIONALE: If this individual right reading vanished, federal and state governments would immediately gain significantly more power to regulate and potentially prohibit firearms, leading to a complete restructuring of gun laws, the firearms market, and the political landscape around gun rights.
% FOUNDING_PROBLEM: The founding problem was to ensure that citizens retained the means to resist potential government tyranny and to participate in a well-regulated militia, reflecting a fear of centralized power and a belief in armed citizenry.
% FOUNDING_PROBLEM_CORROBORATION: Individual gun rights advocates and the firearms industry attest the problem is live, citing ongoing threats to liberty. Gun control advocates and some constitutional scholars argue the original problem (militia for defense against federal overreach) is largely dead in the modern context, and the constraint now serves different interests. Supreme Court jurisprudence (e.g., Heller, McDonald) has largely corroborated the individual right, though its scope remains debated.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading significantly curtails the government's power to regulate firearms, effectively extracting regulatory authority from federal and state entities. Suppression (0.40) is moderate; while there's active resistance to gun control, the legal framework itself suppresses alternative interpretations and legislative efforts. Theater ratio (0.10) is low, as the enforcement of this right is direct and consequential, not merely performative. Resistance (0.80) is high, reflecting the intense political and legal battles over gun control, with strong advocacy from gun rights groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual gun owners and the firearms industry, this reading is a fundamental protection of liberty (beneficiary seat). From the perspective of federal and state regulatory agencies, it is a significant impediment to public safety measures (payer seat). The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are clear beneficiaries (low d) as their interests are directly protected and advanced. Federal and state regulatory agencies, particularly those seeking stricter gun control, are targets (high d) as their authority is curtailed. Gun violence prevention advocates are excluded, as their policy goals are structurally suppressed by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting an individual right to bear arms) is actively defended and expanded by its beneficiaries, preventing mandatrophy. The 'contested' status of the founding problem reflects the ongoing debate about whether the original intent aligns with modern interpretations, but the constraint's function as an individual right protection is very much 'live' for its beneficiaries. The classification as a Tangled Rope reflects both its genuine coordination function (providing a clear, albeit contested, framework for gun ownership) and its asymmetric extraction of regulatory power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Does the ''individual right'' reading accurately reflect the original intent of the Second Amendment''s framers, or is it a modern reinterpretation?',
    'Further historical and textual analysis, potentially new scholarly consensus, or a constitutional amendment clarifying the intent.',
    'If proven to be a modern reinterpretation, the legitimacy of the ''individual right'' reading could be challenged, potentially shifting its classification towards a Snare or Tangled Rope if its persistence relies more on active enforcement of a novel interpretation than on historical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, conceptual, 'Ambiguity regarding the historical grounding of the individual right interpretation.').

omega_variable(
    public_safety_vs_individual_right_balance,
    'What is the optimal balance between the individual right to bear arms and the state''s interest in public safety, and how does this reading affect that balance?',
    'Empirical studies on the effects of gun control laws on violence, comparative analysis with other nations, and ongoing legislative and judicial processes to define ''reasonable'' regulations.',
    'If the ''individual right'' reading is found to severely impede effective public safety measures without a commensurate increase in individual liberty, it could be re-evaluated as more extractive (Snare-like) from the perspective of the broader public. If it is found to strike a reasonable balance, its Rope-like qualities would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_safety_vs_individual_right_balance, preference, 'The normative trade-off between individual gun rights and public safety.').

omega_variable(
    scope_of_arms_definition,
    'What types of ''arms'' are protected by this individual right reading, and how does this definition evolve with technological advancements?',
    'Further Supreme Court rulings clarifying the types of weapons protected (e.g., assault weapons, high-capacity magazines) and the application of ''common use'' tests.',
    'A narrow definition of ''arms'' would reduce the extractiveness on regulatory bodies, allowing more control over specific weapon types. A broad definition would increase extractiveness, further limiting government action.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_arms_definition, empirical, 'The evolving definition of ''arms'' under the individual right interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_arms_right__individual_right_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_arms_right__individual_right_reading, theater_ratio, 1980, 0.06).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_arms_right__individual_right_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_arms_right__individual_right_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__individual_right_reading, theater_ratio, 2008, 0.09).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_arms_right__individual_right_reading, theater_ratio, 2016, 0.095).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__individual_right_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(seco_be_t1980, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(seco_be_t1990, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(seco_be_t2000, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(seco_be_t2016, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2016, 0.63).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(seco_su_t1980, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(seco_su_t1990, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(seco_su_t2000, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement(seco_su_t2016, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2016, 0.39).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, federal_gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, state_gun_control_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one of three competing readings of the Second Amendment. Its individual right interpretation directly influences and often forecloses the policy space for the collective and civic republican readings, as well as federal and state gun control efforts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
