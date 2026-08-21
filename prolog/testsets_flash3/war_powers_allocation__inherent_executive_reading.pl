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
 *   human_readable: Inherent Executive War Powers (Commander-in-Chief Reading)
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint represents the 'inherent executive authority' reading of
 *   U.S. war powers, where the President's Commander-in-Chief role is
 *   interpreted as granting broad power to deploy force without prior
 *   congressional authorization. Congressional authorization is treated as a
 *   political nicety, not a constitutional prerequisite. This reading has
 *   gained prominence over decades, particularly in the post-WWII era,
 *   leading to a significant shift in the balance of power towards the
 *   executive branch in matters of war. The constraint is claimed as a
 *   'tangled_rope' because it purports to coordinate national security
 *   responses while simultaneously extracting authority from Congress.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.65).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.2).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers (Commander-in-Chief Reading)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '3631d3fa-28d7-44ec-8372-7baef9c021d2').
narrative_ontology:cs_kernel_codification('3631d3fa-28d7-44ec-8372-7baef9c021d2', fixed_text).
narrative_ontology:cs_authority_grounding('3631d3fa-28d7-44ec-8372-7baef9c021d2', lineage).
narrative_ontology:cs_interpretation_layer_present('3631d3fa-28d7-44ec-8372-7baef9c021d2').
narrative_ontology:cs_reading_relation('3631d3fa-28d7-44ec-8372-7baef9c021d2', war_powers_allocation__congressional_primacy_reading, influences).
narrative_ontology:cs_reading_relation('3631d3fa-28d7-44ec-8372-7baef9c021d2', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('3631d3fa-28d7-44ec-8372-7baef9c021d2', foundational, commander_in_chief_inherent_authority).
narrative_ontology:cs_axiom_status(commander_in_chief_inherent_authority, holdable).
narrative_ontology:cs_axiom_grounding('3631d3fa-28d7-44ec-8372-7baef9c021d2', commander_in_chief_inherent_authority, conventional).
narrative_ontology:cs_axiom('3631d3fa-28d7-44ec-8372-7baef9c021d2', secondary, executive_unity_in_foreign_affairs).
narrative_ontology:cs_axiom_status(executive_unity_in_foreign_affairs, holdable).
narrative_ontology:cs_axiom_grounding('3631d3fa-28d7-44ec-8372-7baef9c021d2', executive_unity_in_foreign_affairs, instrumental).
narrative_ontology:cs_reference_frame('3631d3fa-28d7-44ec-8372-7baef9c021d2', post_wwii_executive_expansion).
narrative_ontology:cs_drift_state('3631d3fa-28d7-44ec-8372-7baef9c021d2', contemporary_global_threat_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3631d3fa-28d7-44ec-8372-7baef9c021d2', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, the_president).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch_agencies).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, the_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As Commander-in-Chief, the President claims inherent authority to deploy military force to protect national interests, viewing congressional authorization as a political courtesy rather than a constitutional requirement. This reading grants maximum flexibility and speed in foreign policy and military action.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, the_president, agenda_setter,
    institutional, biographical, constrained, national).

% Agencies like the Department of Defense and State Department benefit from the President's expanded authority, as it streamlines decision-making and execution of foreign policy and military operations, reducing bureaucratic hurdles and potential delays from Congress.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch_agencies, beneficiary,
    institutional, generational, constrained, national).

% Congress, particularly its war powers committees, sees its constitutional role in authorizing military force diminished. Its primary leverage becomes post-facto appropriations or political pressure, rather than pre-emptive authorization. This reading shifts the burden of restraint from the executive to the legislature.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress, payer,
    institutional, generational, constrained, national).

% The public bears the costs and consequences of military deployments initiated without full legislative debate or explicit consent, including financial burdens, human casualties, and long-term foreign policy commitments. Their ability to influence war decisions is primarily through elections, which are often too slow to affect immediate deployments.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, the_public, payer,
    organized, generational, constrained, national).

% The Supreme Court generally avoids adjudicating war powers disputes between the executive and legislative branches, often citing the 'political question' doctrine. While it holds ultimate interpretive authority, its practical role in constraining executive action on war powers is minimal.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables rapid, decisive executive action in foreign policy and military deployments, allowing the nation to respond quickly to perceived threats or opportunities without the slower, more deliberative process of legislative authorization.
% TRANSFER_FUNCTION: Transfers decision-making authority over military force deployment from Congress to the President, effectively shifting the constitutional check on war initiation from the legislative to the executive branch.
% ABSENT_VOICES: Constitutional scholars advocating for a strict interpretation of congressional war powers, and segments of the public concerned about unchecked executive power, are often marginalized in the immediate aftermath of a presidential military action. Their voices are heard in academic debate and protest, but rarely directly constrain executive action.
% DISAPPEARANCE_RATIONALE: If the inherent executive authority reading vanished, presidents would face immediate and significant legal and political challenges for any military action not explicitly authorized by Congress. This would fundamentally alter the balance of power, requiring a return to a more robust congressional role in war initiation, and likely slowing down or preventing certain types of military interventions.
% FOUNDING_PROBLEM: The U.S. Constitution established a system of separated powers for war-making, dividing authority between the President (Commander-in-Chief) and Congress (power to declare war, raise armies). The inherent executive reading emerged to address perceived needs for swift, decisive action in a complex global environment.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the executive branch and some legal scholars argue that the need for rapid response to evolving global threats remains live. Critics in Congress and other legal scholars contend that the original problem of balancing powers is still live, but the executive's reading has distorted the intended solution, creating a new problem of executive overreach. Independent historical analysis of military interventions and constitutional debates supports the contested nature of the problem's status.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the degree to which the executive branch can unilaterally commit the nation to military action, bypassing legislative checks. Suppression (0.20) is relatively low because Congress retains some avenues for resistance (e.g., appropriations, public hearings), but these are often reactive and less effective than pre-emptive authorization. The theater ratio (0.40) indicates that a significant portion of congressional 'oversight' becomes performative, with debates and resolutions often occurring after the fact, serving more to legitimize or protest an accomplished fact than to genuinely constrain action. The increasing trend in extractiveness and theater ratio over the interval reflects the historical expansion of executive power in this domain.
 *
 * PERSPECTIVAL GAP:
 *   From the executive's perspective, this reading is a necessary 'rope' for effective national security in a fast-moving world. From Congress's perspective, it's a 'snare' that has eroded its constitutional prerogatives. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and executive agencies are clear beneficiaries, gaining flexibility and authority (low directionality). Congress and the public are victims, losing their constitutional check on war-making and bearing the consequences of executive decisions (high directionality). The Supreme Court, as an observer, maintains an analytical distance, rarely intervening.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the executive's unilateral action as pure coordination. While rapid response can be a coordination function, the consistent bypassing of Congress, coupled with the transfer of authority, indicates significant extraction. The 'tangled_rope' classification captures this hybrid nature, acknowledging a coordination claim while highlighting the asymmetric extraction of power. The 'live' status of the founding problem, coupled with the 'world_rearranges' verdict, suggests that while the original problem of national security is real, the current solution has created new problems of accountability and democratic deficit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is the ''inherent executive authority'' reading a faithful interpretation of the Commander-in-Chief clause, or an expansion of presidential power beyond original intent?',
    'A definitive Supreme Court ruling on the scope of inherent executive war powers, or a constitutional amendment clarifying the allocation of war powers.',
    'If deemed an expansion, the constraint''s legitimacy would be severely undermined, likely leading to reclassification as a snare or piton for the executive, and a significant increase in resistance from Congress. If affirmed, it would solidify the current power balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity regarding the constitutional basis of inherent executive war powers.').

omega_variable(
    effectiveness_vs_accountability_tradeoff,
    'Does the increased speed and decisiveness afforded by inherent executive authority genuinely lead to more effective national security outcomes, or does it lead to more frequent, less scrutinized, and potentially less successful military interventions?',
    'Longitudinal empirical studies comparing outcomes of unilateral executive military actions versus congressionally authorized actions, controlling for threat levels and geopolitical context.',
    'If unilateral actions are shown to be less effective or more costly, the coordination justification for this reading would weaken, increasing its perceived extractiveness and potentially shifting it towards a snare. If more effective, it would strengthen the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_vs_accountability_tradeoff, empirical, 'Trade-off between executive speed and military effectiveness/accountability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__inherent_executive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__inherent_executive_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__inherent_executive_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__inherent_executive_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__inherent_executive_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__inherent_executive_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__inherent_executive_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__inherent_executive_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__inherent_executive_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__inherent_executive_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__inherent_executive_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__inherent_executive_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__inherent_executive_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__inherent_executive_reading, base_extractiveness, 60, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__inherent_executive_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__inherent_executive_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__inherent_executive_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__inherent_executive_reading, suppression_requirement, 30, 0.19).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__inherent_executive_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__inherent_executive_reading, suppression_requirement, 50, 0.19).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__inherent_executive_reading, suppression_requirement, 60, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'war_powers_allocation' kernel. Its operation directly influences the practical relevance and enforcement of the 'congressional_primacy_reading' and 'functional_accommodation_reading' by setting a precedent for executive action.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
