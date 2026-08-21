% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment: Individual Right Conditioned on Civic Militia Participation
 *   domain: Constitutional Law / Political Theory / Rights Jurisprudence
 *
 * SUMMARY:
 *   This constraint story represents the 'civic right' reading of the Second
 *   Amendment, which interprets the right to bear arms as an individual right
 *   conditioned on participation in a well-regulated militia. This reading
 *   emphasizes the civic duty aspect, allowing for state regulation to ensure
 *   the militia's effectiveness. It stands in contrast to readings that
 *   assert an unconditioned individual right or a purely collective state
 *   right.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.6).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.55).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment: Individual Right Conditioned on Civic Militia Participation").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "Constitutional Law / Political Theory / Rights Jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '3f7f0806-d96b-4878-abc4-a7e7c543f647').
narrative_ontology:cs_kernel_codification('3f7f0806-d96b-4878-abc4-a7e7c543f647', fixed_text).
narrative_ontology:cs_authority_grounding('3f7f0806-d96b-4878-abc4-a7e7c543f647', lineage).
narrative_ontology:cs_interpretation_layer_present('3f7f0806-d96b-4878-abc4-a7e7c543f647').
narrative_ontology:cs_reading_relation('3f7f0806-d96b-4878-abc4-a7e7c543f647', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('3f7f0806-d96b-4878-abc4-a7e7c543f647', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_axiom('3f7f0806-d96b-4878-abc4-a7e7c543f647', foundational, individual_right_conditioned_on_civic_duty).
narrative_ontology:cs_axiom_status(individual_right_conditioned_on_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('3f7f0806-d96b-4878-abc4-a7e7c543f647', individual_right_conditioned_on_civic_duty, deontological).
narrative_ontology:cs_axiom('3f7f0806-d96b-4878-abc4-a7e7c543f647', foundational, state_power_to_regulate_arms_for_militia).
narrative_ontology:cs_axiom_status(state_power_to_regulate_arms_for_militia, holdable).
narrative_ontology:cs_axiom_grounding('3f7f0806-d96b-4878-abc4-a7e7c543f647', state_power_to_regulate_arms_for_militia, conventional).
narrative_ontology:cs_reference_frame('3f7f0806-d96b-4878-abc4-a7e7c543f647', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('3f7f0806-d96b-4878-abc4-a7e7c543f647', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3f7f0806-d96b-4878-abc4-a7e7c543f647', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, citizens_unwilling_to_serve).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, federal_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, militia_eligible_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These citizens possess the right to bear arms, but it is conditioned on their potential or actual participation in a civic militia. They benefit from the right but bear the cost of civic duty and potential state regulation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, militia_eligible_citizens, payer).

% States retain significant authority to organize and regulate militias, and by extension, the arms-bearing of their citizens. They benefit from a framework that supports local defense and public order, but must manage the associated civic duties.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_governments, agenda_setter,
    institutional, generational, mobile, national).

% Individuals who do not wish to participate in a militia may find their right to bear arms curtailed or subject to conditions they find onerous. They bear the cost of the civic duty without necessarily receiving the full benefit of the right as they might conceive it.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, citizens_unwilling_to_serve, payer,
    powerless, immediate, constrained, national).

% The federal government's power to regulate firearms may be constrained by the emphasis on state militias and individual participation, leading to complex jurisdictional challenges and limitations on national policy.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Advocates for an absolute individual right to bear arms, unconnected to militia service, find their interpretation excluded by this reading. They actively resist this framing in legal and political discourse.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, individual_right_advocates, excluded,
    organized, biographical, mobile, national).

% Advocates for a purely collective right, where the Second Amendment only protects state authority to maintain militias and not individual ownership, also find their interpretation excluded. They too resist this framing.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, collective_right_advocates, excluded,
    organized, biographical, mobile, national).

% Academics and legal experts who analyze the historical context and legal implications of the Second Amendment, providing critical commentary on its various interpretations without direct participation in its enforcement or benefit.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure a well-regulated militia, composed of armed citizens, is available for public security and defense, thereby coordinating individual arms-bearing with civic duty.
% TRANSFER_FUNCTION: Transfers the duty of militia participation and acceptance of state regulation to citizens in exchange for the right to bear arms; transfers regulatory authority to states for public safety.
% ABSENT_VOICES: Advocates for an unconditioned individual right to bear arms, and those for a purely collective state right, are structurally excluded from this reading's framework, as their core premises are rejected by its logic.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the legal and social framework governing firearm ownership, state regulatory power, and the concept of a citizen militia would collapse, leading to a profound reorganization of constitutional law and public policy.
% FOUNDING_PROBLEM: The need to balance individual liberty with collective security, specifically ensuring a ready citizen militia for defense against foreign invasion or domestic insurrection, without relying on a standing army perceived as a threat to liberty.
% FOUNDING_PROBLEM_CORROBORATION: Historical documents from the founding era, contemporary legal scholars, and state defense force charters corroborate the intent to link arms-bearing with civic duty. Debates about local defense, federal power, and citizen arms persist, indicating the problem is still live, though its specific manifestations have evolved.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is moderate because while individuals gain a right, it comes with the cost of civic duty and potential regulation. Suppression (0.55) is moderate, reflecting the state's power to regulate arms for militia purposes, which can suppress certain types of ownership or use. Theater ratio (0.1) is low, as this reading is actively debated and applied, not merely performative. Accessibility collapse (0.45) is moderate, as alternatives (unregulated ownership or purely collective state control) are constrained by this interpretation. Resistance (0.7) is high, reflecting the ongoing legal and political contestation from proponents of other readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments, this reading provides a functional framework for public safety and defense, balancing rights with responsibilities. For citizens unwilling to serve, it imposes a cost on their right. Advocates for other readings perceive this as an illegitimate curtailment of rights or an overreach of individual claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible citizens are beneficiaries as they possess the right, but also payers due to the associated duties and regulations. State governments are agenda-setters and beneficiaries, gaining regulatory authority and a framework for defense. Citizens unwilling to serve and the federal government (whose regulatory power is limited by state militia focus) are victims. Individual and collective right advocates are excluded, as their core premises are incompatible with this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by explicitly linking the right to a civic function. If the militia function were to atrophy completely, and the constraint persisted solely as a regulated individual right, it would drift towards a Snare (if regulation became purely extractive) or a Piton (if the regulatory function became theatrical).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_relevance_ambiguity,
    'Is the concept of a ''well-regulated militia'' as understood at the founding still functionally relevant in contemporary society, or has its meaning fundamentally shifted?',
    'Empirical analysis of modern state defense forces and their operational capacity, compared to historical militia roles, alongside legal scholarship on evolving constitutional interpretation.',
    'If the militia concept is deemed largely obsolete, the ''conditioned'' aspect of the right might lose its grounding, pushing the constraint towards a more purely individual right (lower extraction) or a purely state-controlled one (higher suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_relevance_ambiguity, empirical, 'The functional relevance of the ''well-regulated militia'' clause.').

omega_variable(
    regulatory_scope_ambiguity,
    'What is the legitimate scope of state regulation over arms-bearing under this ''civic right'' reading, and where does it become unduly burdensome or extractive?',
    'Judicial review of specific state regulations, balancing public safety interests against the individual''s conditioned right, informed by historical practice and contemporary needs.',
    'If regulatory scope is found to be excessively broad, the constraint''s extractiveness would be re-evaluated upward, potentially shifting its classification towards a Snare for those subject to the regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_scope_ambiguity, conceptual, 'The boundary between legitimate regulation and extractive burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__civic_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1850, second_amendment_scope__civic_right_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_scope__civic_right_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_scope__civic_right_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_scope__civic_right_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_scope__civic_right_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__civic_right_reading, base_extractiveness, 1791, 0.45).
narrative_ontology:measurement(seco_be_t1850, second_amendment_scope__civic_right_reading, base_extractiveness, 1850, 0.5).
narrative_ontology:measurement(seco_be_t1900, second_amendment_scope__civic_right_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(seco_be_t1950, second_amendment_scope__civic_right_reading, base_extractiveness, 1950, 0.58).
narrative_ontology:measurement(seco_be_t2000, second_amendment_scope__civic_right_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(seco_be_t2024, second_amendment_scope__civic_right_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__civic_right_reading, suppression_requirement, 1791, 0.4).
narrative_ontology:measurement(seco_su_t1850, second_amendment_scope__civic_right_reading, suppression_requirement, 1850, 0.45).
narrative_ontology:measurement(seco_su_t1900, second_amendment_scope__civic_right_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(seco_su_t1950, second_amendment_scope__civic_right_reading, suppression_requirement, 1950, 0.52).
narrative_ontology:measurement(seco_su_t2000, second_amendment_scope__civic_right_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(seco_su_t2024, second_amendment_scope__civic_right_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the Second Amendment's scope, each modeled as a separate constraint due to differing ε values and structural properties. This 'civic right' reading emphasizes the link between individual arms and militia service.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
