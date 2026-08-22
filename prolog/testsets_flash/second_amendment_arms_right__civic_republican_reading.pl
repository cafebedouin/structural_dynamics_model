% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Arms Right (Civic Republican Reading)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the civic republican reading of the Second
 *   Amendment, which interprets the right to bear arms as intrinsically
 *   linked to the duty of citizens to participate in a well-regulated militia
 *   for the common defense and republican self-governance. It is neither a
 *   purely individual right nor solely a state prerogative. This reading
 *   emphasizes the 'well-regulated militia' clause as central to the right's
 *   purpose, implying a civic obligation and justifying moderate regulation
 *   related to training and qualification. The constraint's claimed type is
 *   'rope' because it aims to coordinate civic duty with individual capacity
 *   for collective benefit, but its operation involves some extraction (e.g.,
 *   compliance costs) and requires active enforcement of regulations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.35).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.2).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Arms Right (Civic Republican Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, 'cd64fcf4-38cb-4200-8cd7-a063f06520d3').
narrative_ontology:cs_kernel_codification('cd64fcf4-38cb-4200-8cd7-a063f06520d3', fixed_text).
narrative_ontology:cs_authority_grounding('cd64fcf4-38cb-4200-8cd7-a063f06520d3', lineage).
narrative_ontology:cs_interpretation_layer_present('cd64fcf4-38cb-4200-8cd7-a063f06520d3').
narrative_ontology:cs_reading_relation('cd64fcf4-38cb-4200-8cd7-a063f06520d3', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd64fcf4-38cb-4200-8cd7-a063f06520d3', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('cd64fcf4-38cb-4200-8cd7-a063f06520d3', foundational, armed_citizenry_for_free_state).
narrative_ontology:cs_axiom_status(armed_citizenry_for_free_state, holdable).
narrative_ontology:cs_axiom_grounding('cd64fcf4-38cb-4200-8cd7-a063f06520d3', armed_citizenry_for_free_state, deontological).
narrative_ontology:cs_axiom('cd64fcf4-38cb-4200-8cd7-a063f06520d3', foundational, well_regulated_militia_is_civic_duty).
narrative_ontology:cs_axiom_status(well_regulated_militia_is_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('cd64fcf4-38cb-4200-8cd7-a063f06520d3', well_regulated_militia_is_civic_duty, conventional).
narrative_ontology:cs_reference_frame('cd64fcf4-38cb-4200-8cd7-a063f06520d3', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('cd64fcf4-38cb-4200-8cd7-a063f06520d3', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cd64fcf4-38cb-4200-8cd7-a063f06520d3', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, civic_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governance).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, unqualified_citizens).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, federal_regulatory_overreach).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, civic_militia_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These citizens are both beneficiaries of the right to bear arms for civic purposes and payers through their duty to maintain proficiency and participate in a well-regulated militia. Their right is tied to a civic obligation, distinguishing them from purely individual gun owners.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, civic_militia_members, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, civic_militia_members, payer).

% The abstract concept of a self-governing republic benefits from an armed citizenry capable of collective defense, serving as a check on potential tyranny and ensuring civic participation in security. This is a vindicated proposition, not an agent that collects rents.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, republican_self_governance, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_arms_right__civic_republican_reading, republican_self_governance).

% These authorities are tasked with regulating arms to ensure a 'well-regulated militia' while respecting the civic right. Their power is constrained by the need to foster, rather than suppress, civic participation in defense. They enforce training and qualification standards.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, federal_regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Citizens who do not meet civic-republican standards for arms proficiency or responsible use may be denied access to certain arms or activities. They bear the cost of regulatory requirements designed to ensure a 'well-regulated' citizenry.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, unqualified_citizens, payer,
    powerless, immediate, constrained, local).

% The abstract concept of excessive federal regulation that infringes on the civic right to bear arms. This is a potential outcome that the civic republican reading seeks to prevent, rather than an agent.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, federal_regulatory_overreach, payer,
    analytical, biographical, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_arms_right__civic_republican_reading, federal_regulatory_overreach).

% Advocates who view the Second Amendment as a purely individual right, unburdened by civic duty or militia service, find their interpretation marginalized by this reading. They would argue against any regulation tied to militia service.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, individual_rights_advocates, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the right of citizens to bear arms with the civic duty to participate in a well-regulated militia, ensuring collective security and preventing both state monopoly on force and individual anarchy.
% TRANSFER_FUNCTION: Transfers the responsibility for collective defense, in part, to an armed citizenry, while imposing costs of training and regulation on those citizens. It also transfers a degree of power from the federal government to the armed populace.
% ABSENT_VOICES: Pure individual-rights advocates, who would argue against any civic duty or regulatory burden on gun ownership, are largely absent from the core interpretive framework of this reading. Their arguments are often reframed as 'misunderstandings' of the civic purpose.
% DISAPPEARANCE_RATIONALE: If this civic republican understanding of the Second Amendment vanished, the balance between individual liberty and collective security would fundamentally shift. Either the right would become purely individual (leading to less regulation but potentially more chaos) or purely state-controlled (leading to a disarmed populace and potential for tyranny), fundamentally altering the nature of republican governance.
% FOUNDING_PROBLEM: The founding problem was how to secure a free state against both internal insurrection and external threats, without relying on a standing army that could become an instrument of tyranny, by empowering a virtuous, armed citizenry.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era, political theorists, and some contemporary legal scholars outside of specific advocacy groups corroborate that the founding problem of balancing civic virtue, individual liberty, and collective security remains relevant, albeit in modern forms. They attest that the original intent was not purely individual or purely state-centric.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the costs imposed on citizens for training, licensing, and adherence to regulations, which are seen as necessary for the 'well-regulated' aspect. Suppression (0.20) is low, as the reading aims to empower citizens, not disarm them, but it does suppress unqualified or irresponsible ownership. Theater ratio (0.10) is low, as the civic purpose is genuinely pursued, though some performative aspects of 'militia' might exist. Accessibility collapse (0.40) is moderate, as alternatives to armed civic participation are not entirely foreclosed but are less central to this vision of self-governance. Resistance (0.30) is moderate, coming from those who prefer a purely individual or purely state-centric interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civic militia members, the constraint is a beneficial coordination mechanism that empowers them while requiring responsible participation. From the perspective of individual rights advocates, it is an extractive constraint that burdens a fundamental liberty with unnecessary civic duties. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Civic militia members are dual beneficiaries (right to bear arms) and payers (duty to train, comply with regulations), placing their directionality near symmetric. Republican self-governance is a beneficiary (abstract concept). Federal regulatory authorities are agenda-setters, balancing regulation with civic empowerment. Unqualified citizens are targets of regulation (payers). Individual rights advocates are excluded, as their framing is outside this reading's core.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_duty_enforcement_legitimacy,
    'To what extent can the ''civic duty'' aspect of this right be legitimately enforced through federal regulation without undermining the ''right to keep and bear arms''?',
    'Judicial rulings on specific regulatory schemes, public acceptance of militia training requirements, and empirical studies on the impact of such regulations on civic participation.',
    'If enforcement is deemed illegitimate or counterproductive, the reading''s ''rope'' classification could drift towards ''tangled_rope'' or ''snare'' for citizens, as the coordination function is undermined by excessive extraction or suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_duty_enforcement_legitimacy, conceptual, 'Ambiguity regarding the balance between civic duty and individual right in enforcement.').

omega_variable(
    militia_relevance_in_modern_era,
    'Is the concept of a ''well-regulated militia'' as understood by this reading still relevant for national security and republican self-governance in the modern era?',
    'Expert analysis from military strategists, political scientists, and constitutional scholars on the role of citizen militias versus professional armed forces in contemporary defense and security.',
    'If the militia concept is deemed obsolete, the civic republican reading''s justification for the right could erode, potentially leading to its reclassification towards a ''piton'' (if maintained theatrically) or a ''snare'' (if used to justify extraction without genuine civic purpose).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_relevance_in_modern_era, empirical, 'The contemporary relevance of the ''well-regulated militia'' clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1850, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1900, 0.09).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1791, 0.2).
narrative_ontology:measurement(seco_be_t1850, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1850, 0.25).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(seco_be_t1950, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement(seco_be_t2000, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(seco_su_t1850, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1850, 0.15).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement(seco_su_t1950, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1950, 0.19).
narrative_ontology:measurement(seco_su_t2000, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
