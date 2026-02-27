% ============================================================================
% CONSTRAINT STORY: hhs_fetal_tissue_research_ban_2019
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hhs_fetal_tissue_research_ban_2019, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hhs_fetal_tissue_research_ban_2019
 *   human_readable: 2019 HHS Ban on Fetal Tissue Research Funding
 *   domain: political/scientific
 *
 * SUMMARY:
 *   In June 2019, the U.S. Department of Health and Human Services (HHS)
 *   imposed a ban on federal funding for research involving newly acquired
 *   fetal tissue, derived from elective abortions. This policy decision
 *   sparked significant debate, pitting the scientific community against
 *   anti-abortion groups. The ban significantly impacted fetal tissue
 *   research, which is vital for understanding and treating diseases such as
 *   HIV, Parkinson's, and Alzheimer's. This constraint impacts parties on
 *   both sides of the political spectrum.
 *
 * KEY AGENTS:
 *   - Fetal Tissue Research Community: Primary victim (powerless/trapped) - unable to continue vital work.
 *   - Anti-Abortion Groups: Primary beneficiary (institutional/arbitrage) - aligns with their values and goals.
 *   - HHS Policy Makers: Constrained by political pressures (institutional/constrained).
 *   - Patients requiring FT-derived therapies: Victim, but possibly mobile (moderate/mobile)
 *   - Competing Research Areas: Beneficiary, but with possible constraint (powerful/mobile)
 *   - Analytical Observer: Sees the tangled web of factors (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hhs_fetal_tissue_research_ban_2019, 0.65).
domain_priors:suppression_score(hhs_fetal_tissue_research_ban_2019, 0.7).
domain_priors:theater_ratio(hhs_fetal_tissue_research_ban_2019, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, extractiveness, 0.65).
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hhs_fetal_tissue_research_ban_2019, snare).
narrative_ontology:human_readable(hhs_fetal_tissue_research_ban_2019, "2019 HHS Ban on Fetal Tissue Research Funding").
narrative_ontology:topic_domain(hhs_fetal_tissue_research_ban_2019, "political/scientific").

domain_priors:requires_active_enforcement(hhs_fetal_tissue_research_ban_2019).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hhs_fetal_tissue_research_ban_2019, anti_abortion_groups).
narrative_ontology:constraint_beneficiary(hhs_fetal_tissue_research_ban_2019, competing_research_areas).
narrative_ontology:constraint_victim(hhs_fetal_tissue_research_ban_2019, fetal_tissue_research_community).
narrative_ontology:constraint_victim(hhs_fetal_tissue_research_ban_2019, patients_requiring_ft_derived_therapies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Researchers reliant on fetal tissue are trapped, with limited alternative funding sources or research avenues. The ban significantly hinders their work and ability to make advancements.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% These groups benefit from the ban, as it aligns with their goals and values. They perceive the ban as a necessary step to prevent unethical research practices and potentially benefit through increased political influence and donations. They have alternative avenues for political action.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% HHS policy makers are constrained by political pressures from both sides of the debate. They gain political capital from implementing the ban, but risk alienating the scientific community, creating a tangled rope.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Patients are harmed because therapies are slowed, but they also may be supported by groups who disagree with the original research and seek alternative paths.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Some areas of medical research that do not rely on fetal tissue may benefit from increased funding and attention as a result of the ban. They have constrained exit, since their field may be strengthened by the ban, though there may be some moral reservations.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From a civilizational perspective, the ban represents a complex interplay of ethical considerations, political influences, and scientific progress. This highlights a hybrid of political extraction and the potential for advancing alternative medical methods.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hhs_fetal_tissue_research_ban_2019_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hhs_fetal_tissue_research_ban_2019, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hhs_fetal_tissue_research_ban_2019_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): Substantial resources and opportunities extracted from researchers. Suppression (0.70): The ban suppresses research by limiting funding and creating barriers to access fetal tissue. Theater ratio (0.30): Limited performative activity since it is a straightforward ban on funding.
 *
 * PERSPECTIVAL GAP:
 *   The fetal tissue research community experiences the ban as a snare because they lose funding. Anti-abortion groups see it as a rope because it aligns with their goals. An analytical observer might classify this as a tangled rope because of competing political factors.
 *
 * DIRECTIONALITY LOGIC:
 *   Researchers reliant on fetal tissue research are trapped. Anti-abortion groups benefit from the ban by having a win in policy that advances their ethical stance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_weighting,
    'How should the ethical concerns surrounding fetal tissue research be weighed against the potential for medical advancements?',
    'Public discourse and ethical frameworks, guided by expert panels and legal precedents.',
    'Classification could shift towards a rope or scaffold if ethical concerns are addressed through alternative research methods and policies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_weighting, preference, 'Ethical concerns vs medical advancement').

omega_variable(
    alternative_funding,
    'To what extent will alternative funding sources emerge to support research affected by the ban?',
    'Monitoring public and private funding trends, tracking research output in related fields.',
    'Availability of alternative funding may reduce the snare classification and shift it towards a piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding, empirical, 'Alternative funding emergence').

omega_variable(
    longterm_scientific_impact,
    'What will be the long-term impact of the ban on medical progress and patient outcomes?',
    'Longitudinal studies comparing outcomes for patients with conditions that could be treated with fetal tissue-derived therapies vs. those that cannot.',
    'Significant negative impact would reinforce the snare classification, while limited impact might suggest a scaffold or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(longterm_scientific_impact, empirical, 'Ban''s impact on medical progress and patient outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hhs_fetal_tissue_research_ban_2019, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hhs__tr_t0, hhs_fetal_tissue_research_ban_2019, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hhs__tr_t2, hhs_fetal_tissue_research_ban_2019, theater_ratio, 2, 0.3).
narrative_ontology:measurement(hhs__tr_t4, hhs_fetal_tissue_research_ban_2019, theater_ratio, 4, 0.4).

% Extraction over time
narrative_ontology:measurement(hhs__be_t0, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(hhs__be_t2, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(hhs__be_t4, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 4, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hhs_fetal_tissue_research_ban_2019, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
