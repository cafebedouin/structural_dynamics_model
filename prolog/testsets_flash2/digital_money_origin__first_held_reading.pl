% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money Origin: First Held as Value
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story defines the origin of digital money as the point
 *   when individuals began to practically hold non-physical monetary
 *   instruments as stores of value. This 'first held' reading emphasizes user
 *   adoption and practical utility over theoretical conception or formal
 *   regulatory recognition. It implies a later origin date than a 'became
 *   thinkable' reading, and a more distributed, less institutionally driven
 *   emergence than a 'regulatory recognition' reading. The constraint itself
 *   is the emergent social and technological infrastructure that made this
 *   'holding' possible and practical.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.3).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.4).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin: First Held as Value").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, 'aca45e1a-032a-4576-a256-9c832e8b4c01').
narrative_ontology:cs_kernel_codification('aca45e1a-032a-4576-a256-9c832e8b4c01', distributed).
narrative_ontology:cs_authority_grounding('aca45e1a-032a-4576-a256-9c832e8b4c01', practice).
narrative_ontology:cs_reading_relation('aca45e1a-032a-4576-a256-9c832e8b4c01', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('aca45e1a-032a-4576-a256-9c832e8b4c01', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('aca45e1a-032a-4576-a256-9c832e8b4c01', foundational, value_is_in_use).
narrative_ontology:cs_axiom_status(value_is_in_use, holdable).
narrative_ontology:cs_axiom_grounding('aca45e1a-032a-4576-a256-9c832e8b4c01', value_is_in_use, conventional).
narrative_ontology:cs_axiom('aca45e1a-032a-4576-a256-9c832e8b4c01', foundational, practical_adoption_defines_existence).
narrative_ontology:cs_axiom_status(practical_adoption_defines_existence, holdable).
narrative_ontology:cs_axiom_grounding('aca45e1a-032a-4576-a256-9c832e8b4c01', practical_adoption_defines_existence, empirically_contingent).
narrative_ontology:cs_reference_frame('aca45e1a-032a-4576-a256-9c832e8b4c01', individual_practical_utility).
narrative_ontology:cs_drift_state('aca45e1a-032a-4576-a256-9c832e8b4c01', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aca45e1a-032a-4576-a256-9c832e8b4c01', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, financial_innovators).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, technologically_excluded).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who first gained access to and utilized non-physical monetary instruments (e.g., early forms of electronic funds, digital currencies) as practical stores of value, benefiting from convenience and new financial capabilities.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Entities (e.g., tech companies, financial institutions) that developed and deployed the infrastructure and instruments enabling digital money, benefiting from market leadership and new revenue streams.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, financial_innovators, agenda_setter,
    organized, generational, arbitrage, global).

% Populations lacking access to traditional banking or digital infrastructure, who face exclusion from emerging digital economies and may bear indirect costs of a shift away from physical cash.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_populations, payer,
    powerless, biographical, trapped, local).

% Individuals or communities without the necessary technological literacy, devices, or internet access to participate in digital money systems, experiencing a widening gap in financial inclusion.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, technologically_excluded, payer,
    powerless, biographical, constrained, regional).

% Government bodies responsible for monetary policy and regulation, observing the emergence and adoption of digital money to understand its impact on financial stability and economic measurement.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding and practical use of non-physical instruments as legitimate and reliable stores of value, enabling new forms of economic interaction and transaction beyond physical cash.
% TRANSFER_FUNCTION: Facilitates the transfer of value through digital means, shifting transaction costs and risks from physical handling to digital infrastructure, from those with access to those who provide the digital services.
% ABSENT_VOICES: Those without access to digital infrastructure or financial literacy are largely absent from the discourse on digital money's origins, as their practical exclusion means their 'holding' of value remains physical.
% DISAPPEARANCE_RATIONALE: If the concept and practice of individuals holding non-physical monetary instruments as practical stores of value vanished, the global financial system would undergo a profound and immediate reorganization, reverting to physical-only transactions and value storage, disrupting all modern commerce.
% FOUNDING_PROBLEM: The limitations of physical cash for large-scale, rapid, or remote transactions, and the desire for more efficient and secure methods of storing and transferring value.
% FOUNDING_PROBLEM_CORROBORATION: Historians of finance and technology, alongside contemporary economists, corroborate the persistent challenges of physical money and the ongoing drive for digital solutions, attesting to the problem's continued relevance.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).
:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is moderate, reflecting the costs of developing and maintaining digital infrastructure, as well as the exclusion of those without access. Suppression (0.4) is also moderate, as the constraint's persistence relies on the practical barriers to entry for alternatives and the network effects of adoption, rather than overt coercion. Theater ratio is low (0.1) as the function of enabling digital value holding is genuine. The constraint is classified as a Rope because it primarily facilitates coordination for those with access, despite some emergent extraction and exclusion.
 *
 * PERSPECTIVAL GAP:
 *   The 'first held' reading emphasizes the user's perspective and the practical reality of value storage, which differs from a purely conceptual or regulatory view. This leads to a later origin date and a focus on implementation barriers and network effects as key constraints, rather than intellectual breakthroughs or legislative acts.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and financial innovators are beneficiaries, gaining new capabilities and market share. Unbanked populations and the technologically excluded are victims, facing new forms of exclusion and bearing the indirect costs of a system they cannot access. Monetary authorities are observers, analyzing the phenomenon without directly benefiting or being victimized by this specific 'first held' constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it describes an emergent historical process rather than a designed institution. Its 'mandate' is the practical utility it provides. The classification as a Rope reflects its coordination function in enabling new forms of value storage, rather than pure extraction, though it creates new forms of exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_date_ambiguity,
    'Is the true origin of digital money when it became technically conceivable, when it was first practically held, or when it received regulatory recognition?',
    'Historical consensus based on the specific criteria chosen for ''origin'' (e.g., conceptual, practical, institutional).',
    'A shift to ''became thinkable'' would imply an earlier origin and different initial constraints (intellectual, technological feasibility). A shift to ''regulatory recognition'' would imply a later origin and a focus on institutional constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(origin_date_ambiguity, conceptual, 'Ambiguity in defining the ''origin'' of digital money based on different criteria.').

omega_variable(
    exclusion_mechanism_ambiguity,
    'Is the exclusion of unbanked/technologically excluded populations a structural feature of digital money''s emergence, or a remediable side effect?',
    'Empirical studies on financial inclusion initiatives and their impact on digital money adoption among marginalized groups.',
    'If structural, the extractiveness and suppression metrics are inherent to the constraint. If remediable, the constraint could evolve into a purer Rope with lower extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_ambiguity, empirical, 'Whether exclusion is inherent or incidental to digital money''s emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1960, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_origin__first_held_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__first_held_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(digi_tr_t1980, digital_money_origin__first_held_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__first_held_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__first_held_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_origin__first_held_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__first_held_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__first_held_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__first_held_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__first_held_reading, base_extractiveness, 2000, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_origin__first_held_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__first_held_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__first_held_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__first_held_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__first_held_reading, suppression_requirement, 2000, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'digital_money_origin' kernel. This 'first_held_reading' emphasizes practical adoption, contrasting with 'became_thinkable_reading' (conceptual origin) and 'regulatory_recognition_reading' (institutional origin). Each reading defines a distinct constraint with different structural properties and temporal boundaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
