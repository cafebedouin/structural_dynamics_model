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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Corporate Enclosure (Corporate Incumbent Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'corporate enclosure' reading of the
 *   constitutional copyright mandate, where 'limited times' is interpreted as
 *   maximal extension short of perpetuity, and copyright is primarily a
 *   property right requiring maximal protection. This reading has driven
 *   legislative changes like the 1976 Copyright Act and the 1998 Sonny Bono
 *   Copyright Term Extension Act, significantly extending terms and
 *   strengthening enforcement, benefiting corporate incumbents at the expense
 *   of derivative creators and the public domain. The claimed type is 'snare'
 *   because the coordination story (incentivizing creation) is largely a
 *   cover for extraction, maintained through active lobbying and legal
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.85).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.78).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, snare).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Corporate Enclosure (Corporate Incumbent Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, 'abe87947-f4e5-4836-afba-24f552b3c435').
narrative_ontology:cs_kernel_codification('abe87947-f4e5-4836-afba-24f552b3c435', fixed_text).
narrative_ontology:cs_authority_grounding('abe87947-f4e5-4836-afba-24f552b3c435', extraction).
narrative_ontology:cs_interpretation_layer_present('abe87947-f4e5-4836-afba-24f552b3c435').
narrative_ontology:cs_reading_relation('abe87947-f4e5-4836-afba-24f552b3c435', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('abe87947-f4e5-4836-afba-24f552b3c435', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('abe87947-f4e5-4836-afba-24f552b3c435', foundational, copyright_as_perpetual_property_right).
narrative_ontology:cs_axiom_status(copyright_as_perpetual_property_right, holdable).
narrative_ontology:cs_axiom_grounding('abe87947-f4e5-4836-afba-24f552b3c435', copyright_as_perpetual_property_right, deontological).
narrative_ontology:cs_axiom('abe87947-f4e5-4836-afba-24f552b3c435', secondary, maximal_incentive_requires_maximal_term).
narrative_ontology:cs_axiom_status(maximal_incentive_requires_maximal_term, holdable).
narrative_ontology:cs_axiom_grounding('abe87947-f4e5-4836-afba-24f552b3c435', maximal_incentive_requires_maximal_term, empirically_contingent).
narrative_ontology:cs_reference_frame('abe87947-f4e5-4836-afba-24f552b3c435', maximal_corporate_property_rights).
narrative_ontology:cs_drift_state('abe87947-f4e5-4836-afba-24f552b3c435', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('abe87947-f4e5-4836-afba-24f552b3c435', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lobby for and benefit from maximal copyright term extensions, criminalization of circumvention, and restriction of fair use. They view copyright as a perpetual property right essential for their business models, actively shaping legislation and litigation to this end.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the costs of extended copyright terms and strict enforcement, limiting their ability to build upon existing works without prohibitive licensing fees or legal risk. Their creative output is constrained by the enclosure of the cultural commons.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, global).

% Face restrictions on using copyrighted materials for teaching and scholarship, often navigating complex licensing or risking infringement. The extended terms make historical and cultural works less accessible for educational purposes.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    organized, biographical, constrained, national).

% Struggle to preserve and provide access to cultural heritage due to long copyright terms and anti-circumvention laws, which complicate digital preservation and access initiatives. Many works become 'orphan works' due to inability to locate rights holders.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    moderate, generational, constrained, global).

% Argue for a robust public domain and shorter, constitutionally-aligned copyright terms. Their arguments are often marginalized in legislative processes dominated by corporate lobbying, and they bear the diffuse cost of a diminished public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_advocates, excluded,
    powerless, generational, trapped, global).

% Respond to lobbying efforts from corporate incumbents, often enacting legislation that extends copyright terms or strengthens enforcement, aligning with the 'maximal protection' interpretation. They face political pressure from well-funded industry groups.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, legislators, agenda_setter,
    institutional, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for corporate entities to monetize creative works over extended periods, coordinating their investment and exploitation strategies without immediate competition.
% TRANSFER_FUNCTION: Transfers economic value from derivative creators, educators, and the general public to corporate copyright holders through prolonged exclusive rights and enforcement mechanisms.
% ABSENT_VOICES: The public domain itself, future creators, and those who believe in a robust cultural commons are largely absent from the legislative and judicial processes that shape copyright law, their interests represented only by underfunded advocacy groups.
% DISAPPEARANCE_RATIONALE: If this reading of copyright vanished, corporate incumbents would face immediate challenges to their long-term revenue streams, leading to a rapid reorganization of their business models. The public domain would expand dramatically, fostering new waves of derivative creation and cultural access, fundamentally altering the creative economy.
% FOUNDING_PROBLEM: The original constitutional mandate for copyright was to 'promote the Progress of Science and useful Arts' by granting authors exclusive rights for 'limited Times'. This reading interprets the problem as ensuring maximal economic incentive for corporate content production.
% FOUNDING_PROBLEM_CORROBORATION: Corporate incumbents and their legal teams assert that maximal protection is still necessary to incentivize creation in a global digital economy. Derivative creators, educators, and legal scholars (outside the benefiting parties) argue that the original problem of incentivizing creation is now overshadowed by rent-seeking and enclosure, and that the 'limited times' clause has been effectively nullified.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the significant economic value transferred from the public and derivative creators to corporate rights holders through extended monopolies. Suppression (0.78) is high due to aggressive enforcement of anti-circumvention laws and the chilling effect of litigation on fair use. The theater ratio (0.20) is relatively low, as the enforcement machinery is genuinely active in protecting these extended rights, though the 'public benefit' justification often serves as a rhetorical cover for private gain. Accessibility collapse (0.65) is substantial as many works remain locked behind copyright for decades, and resistance (0.70) is high from various public interest groups and creators.
 *
 * PERSPECTIVAL GAP:
 *   Corporate incumbents perceive this as a legitimate property right and a necessary incentive for investment, thus a 'rope' or even 'mountain' of economic reality. Derivative creators and public domain advocates experience it as a 'snare' that restricts access and creativity. The engine's classification will reflect the latter due to the high extraction and suppression metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate incumbents are clear beneficiaries and agenda-setters, actively shaping the legal landscape. Derivative creators, educators, and archivists are direct payers, bearing the costs of restricted access and licensing. Public domain advocates are excluded voices, their interests systematically marginalized. Legislators, while agenda-setters, are influenced by the powerful lobbying of corporate incumbents, making them indirect beneficiaries of the system's stability.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the current copyright regime as a 'rope' (pure coordination) by highlighting the substantial, actively enforced extraction. The 'contested' status of the founding problem (incentivizing creation) combined with the 'world_rearranges' disappearance verdict suggests a system whose original mandate may be atrophied, now serving primarily extractive ends, but whose removal would still cause significant disruption due to entrenched interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_original_intent,
    'Does the current interpretation of ''limited times'' align with the original intent of the US Constitution''s Copyright Clause, or has it drifted significantly?',
    'Historical legal scholarship examining founding-era debates and early copyright statutes, and comparative constitutional analysis of similar clauses in other nations.',
    'If a significant drift from original intent is established, it would strengthen arguments for legislative reform or judicial reinterpretation, potentially reducing extractiveness and suppression. If alignment is found, it would bolster the current reading''s legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_original_intent, empirical, 'Ambiguity regarding the historical and legal interpretation of ''limited times''.').

omega_variable(
    economic_incentive_efficacy,
    'Do extended copyright terms genuinely incentivize new creation, or do they primarily serve to protect existing corporate assets?',
    'Empirical economic studies correlating term length with new creative output, and analysis of the financial structures of major copyright holders.',
    'If extended terms are shown to have negligible incentive effect, it undermines the primary justification for the corporate enclosure reading, potentially leading to policy changes that favor the public domain. If a strong correlation is found, it would support the current regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_incentive_efficacy, empirical, 'Whether copyright extensions serve their stated purpose or primarily benefit incumbents.').

omega_variable(
    public_domain_value,
    'How do the economic and cultural benefits of a robust public domain compare to the benefits of extended private copyright monopolies?',
    'Economic modeling of public domain contributions to innovation and cultural production, and qualitative studies of creative reuse and remix culture.',
    'A strong demonstration of public domain value would shift the policy preference towards shorter terms and greater access, challenging the ''maximal protection'' axiom. If private monopolies are shown to yield greater net benefit, the current reading would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_value, preference, 'The relative societal value of private monopolies versus the public domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(copy_tr_t1990, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(copy_tr_t2010, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.6).
narrative_ontology:measurement(copy_be_t1990, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1998, 0.8).
narrative_ontology:measurement(copy_be_t2010, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(copy_su_t1990, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1998, 0.75).
narrative_ontology:measurement(copy_su_t2010, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, digital_millennium_copyright_act__anti_circumvention_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, fair_use_doctrine__restrictive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'copyright_constitutional_mandate' kernel. It is linked to other constraints that represent specific legislative enactments or interpretive doctrines flowing from this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
