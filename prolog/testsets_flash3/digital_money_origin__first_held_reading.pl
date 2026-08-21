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
 *   human_readable: Emergence of Digital Money (First Held Reading)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story represents the 'first held' reading of the
 *   emergence of digital money, focusing on the point when individuals began
 *   to practically use non-physical monetary instruments as stores of value.
 *   This reading emphasizes the user adoption and functional utility aspects,
 *   leading to a later origin date and a constraint set that includes
 *   implementation barriers and network effects. It contrasts with readings
 *   focused on conceptualization or regulatory recognition.
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
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Emergence of Digital Money (First Held Reading)").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, '87b620c2-f57b-485f-919a-9fa041ae3641').
narrative_ontology:cs_kernel_codification('87b620c2-f57b-485f-919a-9fa041ae3641', implicit).
narrative_ontology:cs_authority_grounding('87b620c2-f57b-485f-919a-9fa041ae3641', practice).
narrative_ontology:cs_reading_relation('87b620c2-f57b-485f-919a-9fa041ae3641', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('87b620c2-f57b-485f-919a-9fa041ae3641', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('87b620c2-f57b-485f-919a-9fa041ae3641', foundational, monetary_value_is_user_adoption).
narrative_ontology:cs_axiom_status(monetary_value_is_user_adoption, holdable).
narrative_ontology:cs_axiom_grounding('87b620c2-f57b-485f-919a-9fa041ae3641', monetary_value_is_user_adoption, conventional).
narrative_ontology:cs_axiom('87b620c2-f57b-485f-919a-9fa041ae3641', foundational, practical_utility_precedes_formal_status).
narrative_ontology:cs_axiom_status(practical_utility_precedes_formal_status, holdable).
narrative_ontology:cs_axiom_grounding('87b620c2-f57b-485f-919a-9fa041ae3641', practical_utility_precedes_formal_status, empirically_contingent).
narrative_ontology:cs_reference_frame('87b620c2-f57b-485f-919a-9fa041ae3641', individual_utility_maximization).
narrative_ontology:cs_drift_state('87b620c2-f57b-485f-919a-9fa041ae3641', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('87b620c2-f57b-485f-919a-9fa041ae3641', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, financial_innovators).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, technologically_excluded).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, traditional_financial_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who first gained access to and utilized non-physical monetary instruments (e.g., early electronic payment systems, digital currencies) as practical stores of value. They benefited from convenience and new financial capabilities.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Entities (e.g., tech companies, startups) that developed and deployed the initial digital payment and value storage systems. They set the technical standards and infrastructure that enabled digital money to be 'held'.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, financial_innovators, agenda_setter,
    organized, generational, arbitrage, global).

% Populations lacking access to traditional banking infrastructure and, by extension, the early digital money systems that often required such access. They were excluded from the benefits of this new form of value storage.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_populations, excluded,
    powerless, biographical, trapped, local).

% Individuals or communities without the necessary technological infrastructure (internet access, devices) or literacy to engage with digital monetary instruments. They faced barriers to participation.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, technologically_excluded, excluded,
    powerless, biographical, trapped, regional).

% Initially faced competition and the need to adapt to new digital payment methods, incurring costs for system upgrades and strategic shifts. They eventually integrated digital money but bore early transition costs.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, traditional_financial_institutions, payer,
    institutional, generational, constrained, global).

% Observed the emergence and adoption of digital money, eventually needing to consider its implications for monetary policy, regulation, and financial stability. Their role was initially reactive to the 'first held' phenomenon.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a new, more efficient, and often more accessible means for individuals to store and transfer value without physical cash, coordinating economic activity across digital networks.
% TRANSFER_FUNCTION: Facilitated the transfer of purchasing power and wealth from physical forms to digital records, from traditional financial intermediaries to new digital platforms, and from those with access to those without.
% ABSENT_VOICES: The unbanked and technologically excluded populations, who would highlight the initial barriers to entry and the exacerbation of existing inequalities by the new digital divide. Their perspectives were not central to the early development and adoption narratives.
% DISAPPEARANCE_RATIONALE: If the concept of individuals holding non-physical monetary instruments as practical stores of value vanished, the global financial system would undergo a massive, disruptive reorganization. Digital payments, e-commerce, and modern banking would cease to function, reverting to a cash-only or barter economy, fundamentally altering daily life and commerce.
% FOUNDING_PROBLEM: The limitations of physical cash and traditional banking systems for convenience, speed, and global reach in an increasingly interconnected world.
% FOUNDING_PROBLEM_CORROBORATION: Financial historians and technology analysts corroborate that the problems of physical cash and slow traditional banking remain live, driving ongoing innovation in digital money. The continued evolution of digital payment systems and cryptocurrencies attests to this persistent need, corroborated by widespread user adoption and industry investment.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Rope because it primarily facilitates coordination (new ways to store and transfer value) with relatively low extraction. Extraction (0.3) is present due to initial access inequalities and transaction costs, but it's not the primary function. Suppression (0.4) reflects the barriers to entry for those without technology or financial access, rather than active coercion. Theater ratio is low (0.1) as the function is largely genuine. Accessibility collapse is moderate (0.6) because while digital money offered new options, it also created new forms of exclusion for some. Resistance is low (0.2) as the benefits generally outweighed the costs for those who could access it.
 *
 * PERSPECTIVAL GAP:
 *   The 'first held' reading highlights the practical, user-centric emergence, which would be experienced as a beneficial innovation by early adopters. However, for those without access, it would represent a new form of exclusion. Monetary authorities, from their analytical seat, would see a gradual, organic shift in monetary practice, distinct from a top-down regulatory imposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and financial innovators are beneficiaries, gaining new capabilities and market opportunities. Unbanked and technologically excluded populations are victims, facing new forms of exclusion and barriers to participation. Traditional financial institutions are payers, bearing the costs of adapting to a new monetary paradigm. Monetary authorities are observers, reacting to the emergent phenomenon.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_date_ambiguity,
    'Is the ''first held'' moment the most appropriate origin for digital money, or do earlier conceptual or later regulatory milestones define its true emergence?',
    'Consensus among economic historians and technology scholars on the most impactful ''point of no return'' for digital money''s societal integration.',
    'A shift to an earlier ''thinkable'' origin would lower extraction and suppression (fewer implementation barriers), while a later ''regulatory'' origin would increase them (more institutional control and enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(origin_date_ambiguity, conceptual, 'Ambiguity in defining the ''true'' origin point of digital money based on different criteria.').

omega_variable(
    inclusion_exclusion_balance,
    'Did the emergence of digital money primarily expand financial inclusion or create new forms of exclusion for vulnerable populations?',
    'Empirical studies tracking financial access metrics (e.g., account ownership, transaction costs) across different socioeconomic groups over time.',
    'If exclusion predominates, the constraint''s effective extractiveness and suppression would be higher, particularly for powerless seats. If inclusion predominates, it would reinforce the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inclusion_exclusion_balance, empirical, 'Whether digital money''s primary impact was inclusive or exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__first_held_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(digi_tr_t1995, digital_money_origin__first_held_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__first_held_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(digi_tr_t2005, digital_money_origin__first_held_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__first_held_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(digi_tr_t2015, digital_money_origin__first_held_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__first_held_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__first_held_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(digi_be_t1995, digital_money_origin__first_held_reading, base_extractiveness, 1995, 0.25).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__first_held_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(digi_be_t2005, digital_money_origin__first_held_reading, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__first_held_reading, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement(digi_be_t2015, digital_money_origin__first_held_reading, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__first_held_reading, base_extractiveness, 2020, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__first_held_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(digi_su_t1995, digital_money_origin__first_held_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__first_held_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(digi_su_t2005, digital_money_origin__first_held_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__first_held_reading, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement(digi_su_t2015, digital_money_origin__first_held_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__first_held_reading, suppression_requirement, 2020, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a family of readings on the origin of digital money. This 'first held' reading emphasizes practical adoption, influencing but not foreclosing the conceptual and regulatory perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
