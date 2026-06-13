% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment: Collective Right Reading (State Militia Authority)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'collective right' reading of the Second
 *   Amendment, which interprets the amendment as protecting the right of
 *   states to maintain militias, rather than an individual's right to own
 *   firearms. This reading emphasizes the prefatory clause ('A well regulated
 *   Militia, being necessary to the security of a free State') as controlling
 *   the operative clause ('the right of the people to keep and bear Arms,
 *   shall not be infringed'). Under this interpretation, individual gun
 *   ownership is not a constitutionally protected right but is subject to
 *   state regulation in service of the militia's function. This reading was
 *   dominant in U.S. jurisprudence for much of the 19th and early 20th
 *   centuries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.25).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment: Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:emerges_naturally(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '2b1b5c72-871f-45eb-a5b7-f5311676afae').
narrative_ontology:cs_kernel_codification('2b1b5c72-871f-45eb-a5b7-f5311676afae', fixed_text).
narrative_ontology:cs_authority_grounding('2b1b5c72-871f-45eb-a5b7-f5311676afae', lineage).
narrative_ontology:cs_interpretation_layer_present('2b1b5c72-871f-45eb-a5b7-f5311676afae').
narrative_ontology:cs_reading_relation('2b1b5c72-871f-45eb-a5b7-f5311676afae', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('2b1b5c72-871f-45eb-a5b7-f5311676afae', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('2b1b5c72-871f-45eb-a5b7-f5311676afae', foundational, militia_clause_controls_operative_clause).
narrative_ontology:cs_axiom_status(militia_clause_controls_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('2b1b5c72-871f-45eb-a5b7-f5311676afae', militia_clause_controls_operative_clause, conventional).
narrative_ontology:cs_axiom('2b1b5c72-871f-45eb-a5b7-f5311676afae', foundational, individual_arms_bearing_is_not_a_right).
narrative_ontology:cs_axiom_status(individual_arms_bearing_is_not_a_right, holdable).
narrative_ontology:cs_axiom_grounding('2b1b5c72-871f-45eb-a5b7-f5311676afae', individual_arms_bearing_is_not_a_right, deontological).
narrative_ontology:cs_reference_frame('2b1b5c72-871f-45eb-a5b7-f5311676afae', founding_era_state_sovereignty).
narrative_ontology:cs_drift_state('2b1b5c72-871f-45eb-a5b7-f5311676afae', contemporary_jurisprudence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2b1b5c72-871f-45eb-a5b7-f5311676afae', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militias).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, individual_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constitutional recognition of their authority to maintain and regulate militias, allowing them to control armed forces for public order and defense without individual gun ownership rights interfering with this power. This reading grants states broad regulatory authority over firearms.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Benefit from their constitutional status as the object of the Second Amendment's protection, ensuring their existence and the state's power to arm and regulate them. Their existence is tied to state authority, not individual rights.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_militias, beneficiary,
    organized, biographical, constrained, regional).

% Are subject to state regulation of firearms, as this reading does not grant them an individual right to bear arms. They bear the cost of potentially restrictive gun laws without a constitutional claim to ownership, beyond what the state permits for militia service.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_citizens, payer,
    powerless, biographical, constrained, local).

% Interprets the Second Amendment, and under this reading, would uphold state authority over individual claims. Their role is to adjudicate disputes, but this reading limits the scope of individual challenges.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Are excluded from the constitutional protection they seek, as this reading denies an individual right to bear arms. Their advocacy would be directed at legislative change rather than constitutional challenge.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, gun_rights_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between state governments and their need to maintain organized militias for public safety and defense, ensuring a framework for collective security.
% TRANSFER_FUNCTION: Transfers the authority over firearms regulation from individuals to state governments, ensuring that the state's power to organize and arm its militia is paramount.
% ABSENT_VOICES: Individual gun owners and gun rights advocacy groups are effectively absent from the constitutional conversation regarding the Second Amendment under this reading, as their claims are not recognized as constitutionally protected. They would argue for individual liberty and self-defense.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment vanished, state governments would lose a clear constitutional basis for their broad regulatory authority over firearms, potentially leading to a shift towards individual rights interpretations and a significant rearrangement of gun control laws across the nation.
% FOUNDING_PROBLEM: The founding problem was to ensure the security of a free state by providing for well-regulated militias, preventing both federal overreach and the need for a standing army, while also addressing concerns about potential insurrections or foreign invasions.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, particularly those focusing on original intent and the historical context of the amendment, corroborate that the primary concern was the collective security of the states through militias. This view is supported by early state militia laws and constitutional debates, from outside the direct beneficiaries of this reading.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_scope__collective_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_scope__collective_right_reading),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading primarily defines institutional authority rather than extracting from individuals. Suppression is also low (0.25) as it reflects the state's inherent power to regulate for collective security, not active coercion against a recognized individual right. Theater ratio is low (0.1) as the focus is on genuine state authority. Accessibility collapse is high (0.8) because, from this perspective, alternatives to state-controlled militia organization for collective security are largely collapsed. Resistance is low (0.1) because this reading was largely uncontested in its historical period.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments and organized militias, this reading is a Mountain, an unchangeable constitutional principle that secures their authority. From the perspective of individual citizens, it is a constraint that limits their perceived rights, but within this reading, their claims are not constitutionally recognized, so the constraint is still perceived as a Mountain or Rope by the system, not a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militias are clear beneficiaries, as the constraint affirms their authority and existence. Individual citizens are payers, as their ability to own firearms is subject to state regulation. Gun rights advocates are excluded, as their core claim is not recognized by this reading. The federal judiciary acts as an agenda-setter, interpreting and enforcing this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling state regulatory authority as extraction from individuals by clearly defining the scope of the right as collective. It avoids the pitfall of assuming an individual right where the text, in this interpretation, does not grant one, thus preventing a 'false summit' of individual liberty claims under a collective right.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Is the historical ''original intent'' of the Second Amendment truly limited to a collective right, or does it encompass an individual right tied to militia service, or even an unqualified individual right?',
    'Further historical and linguistic analysis of founding-era documents, debates, and state constitutions, with consensus among constitutional historians.',
    'If original intent is found to support an individual right (even a qualified one), this reading''s ''emerges_naturally'' claim would be weakened, and its classification might shift towards a constructed constraint (e.g., a Snare if it actively suppresses individual rights).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, empirical, 'Ambiguity regarding the historical original intent of the Second Amendment''s scope.').

omega_variable(
    militia_relevance_drift,
    'Given the modern military and law enforcement structures, is the concept of a ''well-regulated militia'' as understood in the founding era still functionally relevant to ''the security of a free State''?',
    'Sociological and military analysis of the role of civilian militias in contemporary national security and public order, and judicial re-evaluation of the amendment''s purpose in light of these changes.',
    'If the militia concept is deemed obsolete, the foundational premise of this reading would be undermined, potentially leading to a reinterpretation that either expands individual rights or allows for broader state regulatory power without the militia justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_relevance_drift, conceptual, 'The functional relevance of the ''well-regulated militia'' clause in modern context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 1791, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__collective_right_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement(seco_be_t1830, second_amendment_scope__collective_right_reading, base_extractiveness, 1830, 0.12).
narrative_ontology:measurement(seco_be_t1870, second_amendment_scope__collective_right_reading, base_extractiveness, 1870, 0.13).
narrative_ontology:measurement(seco_be_t1900, second_amendment_scope__collective_right_reading, base_extractiveness, 1900, 0.14).
narrative_ontology:measurement(seco_be_t1939, second_amendment_scope__collective_right_reading, base_extractiveness, 1939, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__collective_right_reading, suppression_requirement, 1791, 0.2).
narrative_ontology:measurement(seco_su_t1830, second_amendment_scope__collective_right_reading, suppression_requirement, 1830, 0.21).
narrative_ontology:measurement(seco_su_t1870, second_amendment_scope__collective_right_reading, suppression_requirement, 1870, 0.22).
narrative_ontology:measurement(seco_su_t1900, second_amendment_scope__collective_right_reading, suppression_requirement, 1900, 0.23).
narrative_ontology:measurement(seco_su_t1939, second_amendment_scope__collective_right_reading, suppression_requirement, 1939, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
