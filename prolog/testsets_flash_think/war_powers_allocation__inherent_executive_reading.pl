% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: President's Inherent War Powers (Inherent Executive Reading)
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint represents the 'inherent executive' reading of US war
 *   powers, where the Commander-in-Chief clause grants the President inherent
 *   authority to deploy military force in defense of national interests
 *   without prior congressional authorization. This reading frames
 *   congressional authorization as a courtesy or political expediency, not a
 *   constitutional requirement. It is one reading of the broader
 *   'war_powers_allocation' kernel, which is contested among different
 *   constitutional interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.78).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.65).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "President's Inherent War Powers (Inherent Executive Reading)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '4f0f6947-c78c-42ba-9821-38a8fc8a94e0').
narrative_ontology:cs_kernel_codification('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', fixed_text).
narrative_ontology:cs_authority_grounding('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', lineage).
narrative_ontology:cs_interpretation_layer_present('4f0f6947-c78c-42ba-9821-38a8fc8a94e0').
narrative_ontology:cs_reading_relation('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', foundational, executive_unity_in_foreign_affairs).
narrative_ontology:cs_axiom_status(executive_unity_in_foreign_affairs, holdable).
narrative_ontology:cs_axiom_grounding('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', executive_unity_in_foreign_affairs, deontological).
narrative_ontology:cs_axiom('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', foundational, decisive_action_in_national_interest).
narrative_ontology:cs_axiom_status(decisive_action_in_national_interest, holdable).
narrative_ontology:cs_axiom_grounding('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', decisive_action_in_national_interest, instrumental).
narrative_ontology:cs_reference_frame('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', post_wwii_presidential_supremacy).
narrative_ontology:cs_drift_state('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', contemporary_era_of_global_threats, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4f0f6947-c78c-42ba-9821-38a8fc8a94e0', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, the_president).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, us_congress).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, american_public).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, unitary_executive_theory).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, presidential_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As Commander-in-Chief, the President asserts and exercises the authority to deploy military force in defense of national interests without prior congressional authorization, interpreting this power as inherent to the office. This seat benefits from decisiveness and flexibility in foreign policy.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, the_president, agenda_setter,
    institutional, biographical, arbitrage, global).

% The various departments and agencies within the executive branch implement presidential directives regarding military deployments. They benefit from clear, centralized authority and the ability to act swiftly, avoiding potential delays or political obstacles from Congress.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch, beneficiary,
    institutional, generational, constrained, global).

% Constitutionally vested with the power to declare war, Congress finds its authority bypassed or reduced to post-facto authorization or appropriations. This seat bears the cost of diminished constitutional power and political accountability for military actions it did not explicitly authorize.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, us_congress, payer,
    institutional, generational, constrained, national).

% The public bears the human and financial costs of military interventions initiated under this interpretation of executive power. While benefiting from perceived national security, they experience a reduction in democratic accountability for war-making decisions.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, american_public, payer,
    moderate, generational, constrained, national).

% The Supreme Court is the ultimate arbiter of constitutional disputes but has historically been reluctant to intervene in war powers conflicts between the political branches, often deeming them 'political questions' outside judicial purview.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the United States to respond swiftly and decisively to perceived national security threats and conduct foreign policy with agility, particularly in a complex global environment requiring rapid decision-making.
% TRANSFER_FUNCTION: Transfers the primary authority for initiating military force from the legislative branch (Congress) to the executive branch (President), along with the associated political capital, resource allocation, and accountability for military actions.
% ABSENT_VOICES: Constitutional scholars advocating for a strict interpretation of congressional war powers, and segments of the public who demand greater democratic accountability for military interventions, are often marginalized in the executive's framing of this authority.
% DISAPPEARANCE_RATIONALE: If the President's inherent authority to deploy force without prior authorization vanished, the executive would be constitutionally compelled to seek explicit congressional approval for most military actions, fundamentally altering the speed, scope, and political dynamics of US foreign policy and military engagement.
% FOUNDING_PROBLEM: The need for a single, decisive actor to respond effectively to immediate threats and conduct foreign policy in a complex and often dangerous international arena, particularly in an era of rapid communication and global challenges.
% FOUNDING_PROBLEM_CORROBORATION: Executive branch legal opinions and historical practice (especially post-WWII) support the 'live' status of this problem and the necessity of this interpretation. However, congressional resolutions, scholarly critiques, and public opinion polls often contest this, arguing the problem is exaggerated or that the solution has led to an overreach of executive power, with corroboration from independent legal analysis and historical records of congressional attempts to reclaim war powers.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because this reading significantly diminishes Congress's constitutional role in war-making, effectively transferring substantial power to the executive. Suppression (0.65) is moderate-high; while Congress can and does resist, the executive's ability to act unilaterally often proceeds despite legislative objections, and the judiciary largely defers. Theater ratio (0.40) reflects the executive's practice of 'consulting' with Congress or seeking post-facto authorizations, which often serve to legitimize actions already taken or planned, rather than genuinely seeking prior approval. Accessibility collapse (0.70) is high for Congress, as its constitutional alternatives to executive action are largely collapsed by this interpretation. Resistance (0.55) is moderate, as Congress frequently attempts to reassert its authority through legislation or resolutions, though often with limited success.
 *
 * PERSPECTIVAL GAP:
 *   From the executive's perspective, this constraint is a necessary mechanism for effective national security and foreign policy, perhaps even a 'rope' coordinating swift action. From Congress's perspective, it is a 'snare' that extracts its constitutional authority. The engine's computation of per-seat classifications will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and the executive branch are clear beneficiaries (low directionality), gaining significant power and flexibility. Congress and the American public are the primary targets (high directionality), losing constitutional checks and democratic accountability over war-making. The Supreme Court acts as an analytical observer, largely avoiding direct intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging the executive's coordination function (swift response) while highlighting the asymmetric extraction from Congress. It avoids treating the expansion of executive power as a 'natural' evolution by documenting the active enforcement and the victims of this interpretation. The 'contested' status of the founding problem further underscores the ongoing debate about whether the original problem (decisive action) still justifies the current scope of executive power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent constitutional principle, or merely one interpretation of the ''war_powers_allocation'' kernel?',
    'Analysis of judicial precedent, legislative history, and executive branch legal opinions to determine if the ''inherent executive'' view is a standalone doctrine or a contested reading within a broader constitutional framework.',
    'If a standalone principle, its classification stands on its own. If a reading, its classification is understood in relation to sibling readings, and its legitimacy is tied to the contest over the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''war_powers_allocation'' kernel.').

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is the President''s inherent authority to deploy force a legitimate interpretation of the Commander-in-Chief power, or an accretion of power that bypasses constitutional checks?',
    'A definitive Supreme Court ruling on the scope of presidential war powers, or a constitutional amendment clarifying the allocation of war powers.',
    'If deemed legitimate, the extractiveness from Congress might be re-evaluated as a necessary constitutional allocation. If deemed an accretion, the classification as a Tangled Rope or Snare would be strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity regarding the constitutional basis of inherent executive war powers.').

omega_variable(
    functional_necessity_vs_constitutional_design,
    'Is the executive''s unilateral action truly necessary for national security in the contemporary era, or does it primarily serve to bypass constitutional checks and balances?',
    'Empirical studies comparing outcomes of unilateral executive actions versus congressionally authorized military interventions, and analysis of the speed and effectiveness of legislative responses to crises.',
    'If necessity is strongly demonstrated, it might lend more weight to the coordination function. If found to be primarily a bypass, the extractive nature would be further emphasized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_necessity_vs_constitutional_design, empirical, 'Whether unilateral executive action is functionally necessary or a constitutional bypass.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_powers_allocation__inherent_executive_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(war__tr_t1965, war_powers_allocation__inherent_executive_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(war__tr_t1985, war_powers_allocation__inherent_executive_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(war__tr_t2001, war_powers_allocation__inherent_executive_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(war__tr_t2010, war_powers_allocation__inherent_executive_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(war__tr_t2024, war_powers_allocation__inherent_executive_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(war__be_t1965, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(war__be_t1985, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement(war__be_t2001, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2001, 0.75).
narrative_ontology:measurement(war__be_t2010, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2010, 0.77).
narrative_ontology:measurement(war__be_t2024, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement(war__su_t1965, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(war__su_t1985, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(war__su_t2001, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2001, 0.63).
narrative_ontology:measurement(war__su_t2010, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement(war__su_t2024, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'war_powers_allocation' kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
