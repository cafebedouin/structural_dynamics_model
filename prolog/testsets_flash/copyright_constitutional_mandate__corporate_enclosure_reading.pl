% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Maximal Corporate Property Right
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'corporate enclosure' reading of the US
 *   Constitutional Copyright Clause, which interprets 'limited times' as
 *   permitting maximal extensions short of explicit perpetuity. This reading
 *   prioritizes copyright as a property right requiring robust protection,
 *   leading to legislative efforts (e.g., the Sonny Bono Copyright Term
 *   Extension Act) and judicial interpretations that favor rights holders
 *   over public domain access and derivative creation. This is one reading of
 *   the 'copyright_constitutional_mandate' kernel, alongside
 *   'public_scaffold_reading' and 'judicial_ambiguity_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.85).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.75).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, snare).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Maximal Corporate Property Right").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, 'ab726b72-5fb3-4e09-a313-c0cccffbae43').
narrative_ontology:cs_kernel_codification('ab726b72-5fb3-4e09-a313-c0cccffbae43', fixed_text).
narrative_ontology:cs_authority_grounding('ab726b72-5fb3-4e09-a313-c0cccffbae43', extraction).
narrative_ontology:cs_interpretation_layer_present('ab726b72-5fb3-4e09-a313-c0cccffbae43').
narrative_ontology:cs_reading_relation('ab726b72-5fb3-4e09-a313-c0cccffbae43', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('ab726b72-5fb3-4e09-a313-c0cccffbae43', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('ab726b72-5fb3-4e09-a313-c0cccffbae43', foundational, copyright_as_absolute_property_right).
narrative_ontology:cs_axiom_status(copyright_as_absolute_property_right, holdable).
narrative_ontology:cs_axiom_grounding('ab726b72-5fb3-4e09-a313-c0cccffbae43', copyright_as_absolute_property_right, conventional).
narrative_ontology:cs_axiom('ab726b72-5fb3-4e09-a313-c0cccffbae43', foundational, maximal_term_extension_is_constitutional).
narrative_ontology:cs_axiom_status(maximal_term_extension_is_constitutional, holdable).
narrative_ontology:cs_axiom_grounding('ab726b72-5fb3-4e09-a313-c0cccffbae43', maximal_term_extension_is_constitutional, conventional).
narrative_ontology:cs_reference_frame('ab726b72-5fb3-4e09-a313-c0cccffbae43', maximal_corporate_control_framework).
narrative_ontology:cs_drift_state('ab726b72-5fb3-4e09-a313-c0cccffbae43', contemporary_digital_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('ab726b72-5fb3-4e09-a313-c0cccffbae43', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major media corporations (e.g., Disney, RIAA, MPAA) that actively lobby for and benefit from maximal copyright terms and strong enforcement against unauthorized use. They shape legislation and litigation strategy to extend their control over existing works.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).

% Artists, writers, musicians, and filmmakers who wish to build upon existing cultural works. They face high licensing fees, legal threats, and limited access to source material due to extended copyright terms and restrictive interpretations of fair use.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, global).

% Teachers and academics who rely on copyrighted materials for instruction and research. They navigate complex licensing requirements and face limitations on what they can share or adapt, impacting pedagogical freedom and access to knowledge.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    moderate, biographical, constrained, national).

% Institutions and individuals dedicated to preserving cultural heritage. Extended copyright terms mean works remain locked out of the public domain for decades, hindering preservation efforts and limiting access for future generations.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    moderate, generational, constrained, global).

% The body of creative works and knowledge that is free for anyone to use and build upon. Under this reading, its growth is severely curtailed, and its role as a commons for cultural production is diminished.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain).

% Congress and other national legislatures that enact copyright laws. They are influenced by lobbying from corporate incumbents, leading to repeated extensions of copyright terms and stronger enforcement mechanisms.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, legislative_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Judicial bodies that interpret copyright statutes and constitutional limits. Under this reading, they tend to favor maximal protection for rights holders, often narrowing exceptions like fair use and upholding term extensions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, courts, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for creators to control and monetize their works, theoretically incentivizing creation by preventing unauthorized copying.
% TRANSFER_FUNCTION: Transfers economic value and control over creative works from the public and derivative creators to corporate rights holders, often for extended periods far beyond the original creator's lifetime.
% ABSENT_VOICES: The 'public domain' as a concept, future creators, and the general public are largely absent from the legislative and judicial processes that shape copyright law under this reading. Their interests in a rich, accessible cultural commons are systematically underrepresented.
% DISAPPEARANCE_RATIONALE: If this maximalist interpretation of copyright vanished, the media and entertainment industries would undergo a massive restructuring. Content would flow more freely, derivative works would proliferate, and business models would shift dramatically from exclusive control to alternative monetization strategies. The public domain would expand rapidly.
% FOUNDING_PROBLEM: To incentivize the creation and dissemination of useful works by granting authors a temporary, exclusive right to their writings and discoveries.
% FOUNDING_PROBLEM_CORROBORATION: Corporate incumbents claim the problem is still live, requiring maximal protection to fund new creation. However, independent economists, legal scholars, and public interest groups (outside the benefiting parties) argue that current terms far exceed any necessary incentive, and the original problem is long solved, with the system now primarily serving to enclose existing works.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) due to the long terms and broad scope of rights, which allow corporate incumbents to control vast catalogs of works for extended periods, generating significant revenue. Suppression is also high (0.75) as this reading supports aggressive enforcement against perceived infringers, including derivative creators and archivists, often through legal threats and technological protection measures. The theater ratio is low (0.20) because the system is highly functional in achieving its goal of maximizing corporate control and revenue, with little performative maintenance for a defunct public purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of corporate incumbents, this reading is a legitimate and necessary framework for protecting intellectual property and incentivizing creation. From the perspective of derivative creators, educators, and archivists, it is an extractive snare that stifles creativity, limits access to knowledge, and encloses the public domain.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate incumbents are clear beneficiaries and agenda-setters, actively shaping and profiting from the maximalist interpretation. Derivative creators, educators, and archivists are direct payers, bearing the costs of restricted access and legal risk. The public domain is an excluded non-agent, its interests systematically undermined. Legislative bodies and courts, while powerful, are influenced by incumbent lobbying, making them de facto enforcers of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of incentivizing creation is largely 'dead' under this reading, as current terms far exceed what is necessary for incentive. The constraint persists not due to its original mandate, but due to the concentrated benefits it provides to corporate incumbents, who actively maintain and extend it. This prevents mislabeling it as a 'rope' (genuine coordination) or 'scaffold' (temporary support for a public good), instead revealing its 'snare' nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incentive_vs_enclosure,
    'At what point do copyright terms cease to incentivize new creation and instead primarily serve to enclose existing works for corporate profit?',
    'Empirical economic studies correlating term length with new creative output, and analysis of the age of works generating significant revenue for rights holders.',
    'If current terms are found to primarily serve enclosure, it would strengthen the ''snare'' classification and support policy interventions to shorten terms or expand fair use. If a strong correlation to new creation is found, it would lend credence to the ''rope'' framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_vs_enclosure, empirical, 'The functional boundary between copyright as incentive and copyright as enclosure.').

omega_variable(
    constitutional_original_intent,
    'Does the ''limited times'' clause in the US Constitution''s Copyright Clause permit indefinite extensions that effectively approach perpetuity, or does it imply a more constrained, finite duration tied to the original author''s creative life?',
    'Historical legal scholarship examining the framers'' intent, early copyright statutes, and contemporary understandings of ''limited times'' at the time of the Constitution''s drafting.',
    'An originalist interpretation favoring shorter, more finite terms would fundamentally challenge the legal basis of current copyright extensions, potentially leading to judicial invalidation of acts like the CTEA. A finding of broad legislative discretion would reinforce the current reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_original_intent, conceptual, 'The original constitutional meaning of ''limited times'' in copyright.').

omega_variable(
    public_domain_value_quantification,
    'What is the economic and cultural value lost by the public due to extended copyright terms, in terms of foregone derivative creation, educational access, and cultural preservation?',
    'Economic modeling of counterfactual scenarios with shorter copyright terms, and qualitative analysis of cultural works that could have been created or preserved if in the public domain.',
    'Quantifying this loss would provide a stronger basis for policy arguments favoring public domain access and could shift the ''preference'' for maximal protection towards a more balanced approach, potentially reclassifying the constraint towards a ''tangled_rope'' if public benefits are recognized as being suppressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_domain_value_quantification, empirical, 'The unquantified cost of copyright enclosure to the public domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.3).
narrative_ontology:measurement(copy_tr_t1985, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1998, 0.25).
narrative_ontology:measurement(copy_tr_t2010, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.65).
narrative_ontology:measurement(copy_be_t1985, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1998, 0.8).
narrative_ontology:measurement(copy_be_t2010, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(copy_su_t1985, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement(copy_su_t2010, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__corporate_enclosure_reading, 0.1).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, digital_millennium_copyright_act_enforcement).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, fair_use_doctrine_interpretation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_access_norms).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'copyright_constitutional_mandate' kernel. It represents the corporate enclosure perspective, which influences the interpretation and enforcement of related IP laws.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
