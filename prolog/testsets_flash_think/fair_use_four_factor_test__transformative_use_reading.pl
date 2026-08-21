% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__transformative_use_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use: Transformative Use Dominance Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint story models the 'transformative use' reading of the fair
 *   use doctrine in US copyright law. This reading emphasizes that a new
 *   work's transformative character (adding new meaning or message) is the
 *   most important factor in the four-factor balancing test, often
 *   subordinating potential market harm to the original work. It functions as
 *   a legal mechanism to enable remix culture and user-generated content, but
 *   also shifts costs and control away from original copyright holders.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.45).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.35).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use: Transformative Use Dominance Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, '32ecebea-4c06-4e03-af3f-76e36e49838b').
narrative_ontology:cs_kernel_codification('32ecebea-4c06-4e03-af3f-76e36e49838b', fixed_text).
narrative_ontology:cs_authority_grounding('32ecebea-4c06-4e03-af3f-76e36e49838b', lineage).
narrative_ontology:cs_interpretation_layer_present('32ecebea-4c06-4e03-af3f-76e36e49838b').
narrative_ontology:cs_reading_relation('32ecebea-4c06-4e03-af3f-76e36e49838b', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('32ecebea-4c06-4e03-af3f-76e36e49838b', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('32ecebea-4c06-4e03-af3f-76e36e49838b', foundational, transformative_use_is_priority).
narrative_ontology:cs_axiom_status(transformative_use_is_priority, holdable).
narrative_ontology:cs_axiom_grounding('32ecebea-4c06-4e03-af3f-76e36e49838b', transformative_use_is_priority, conventional).
narrative_ontology:cs_axiom('32ecebea-4c06-4e03-af3f-76e36e49838b', secondary, market_harm_subordinated_to_new_meaning).
narrative_ontology:cs_axiom_status(market_harm_subordinated_to_new_meaning, holdable).
narrative_ontology:cs_axiom_grounding('32ecebea-4c06-4e03-af3f-76e36e49838b', market_harm_subordinated_to_new_meaning, conventional).
narrative_ontology:cs_reference_frame('32ecebea-4c06-4e03-af3f-76e36e49838b', post_1976_copyright_act_framework).
narrative_ontology:cs_drift_state('32ecebea-4c06-4e03-af3f-76e36e49838b', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('32ecebea-4c06-4e03-af3f-76e36e49838b', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_artists).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, tech_platforms_ugc).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, public_domain_advocates).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, user_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, risk_averse_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply the four-factor fair use test, with a strong emphasis on transformativeness, shaping the legal landscape for cultural production. Their decisions set precedents that guide other actors.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, courts_judges, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the ability to create new works by building upon existing copyrighted material without needing explicit licenses, provided their use is deemed transformative. This enables their creative practice and market access.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_artists, beneficiary,
    moderate, biographical, mobile, global).

% Benefit from a legal framework that allows user-generated content (UGC) incorporating copyrighted material to flourish on their platforms, reducing their liability and content moderation burden related to copyright infringement claims. This fuels their business model.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, tech_platforms_ugc, beneficiary,
    institutional, generational, arbitrage, global).

% As everyday users who create and share content, they benefit from the flexibility to incorporate copyrighted elements into their works (e.g., memes, fan art, commentary) without constant fear of litigation, fostering online cultural participation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, user_creators, beneficiary,
    moderate, biographical, constrained, global).

% Advocate for broader public access to creative works and the expansion of the cultural commons. This reading of fair use aligns with their goals by prioritizing new creative expression over strict property rights.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, public_domain_advocates, beneficiary,
    organized, generational, analytical, global).

% Bear the cost of diminished control over their copyrighted works when transformative uses are permitted without compensation. They may see their potential licensing revenue reduced and their ability to dictate derivative markets curtailed.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders, payer,
    powerful, biographical, constrained, global).

% May be hesitant to create new works that build on existing material due to the inherent uncertainty and cost of fair use litigation, even if their use might ultimately be deemed transformative. They bear the psychological and potential financial cost of legal ambiguity.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, risk_averse_creators, payer,
    powerless, biographical, constrained, global).

% Advise clients and litigate fair use cases, influencing the development and application of the doctrine. They benefit from the complexity and contestability of fair use, which generates demand for their services.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, copyright_lawyers, agenda_setter,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, tech_platforms_ugc).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To balance the constitutional goals of copyright: incentivizing original authorship by granting exclusive rights, while also promoting the progress of science and useful arts by allowing limited, transformative uses that foster new creation and public discourse.
% TRANSFER_FUNCTION: Transfers the right to use copyrighted material without explicit permission or payment, from original copyright holders to transformative users, under judicial oversight. This effectively transfers a portion of the economic value and control from the original work to the new, transformative work.
% ABSENT_VOICES: Small-scale original creators who lack the resources to defend their rights against large platforms or well-funded transformative users. Also, individual transformative users who cannot afford to defend fair use claims, leading to self-censorship or abandonment of projects.
% DISAPPEARANCE_RATIONALE: If the transformative use doctrine vanished, cultural production would be severely stifled. Every derivative work, commentary, or parody would require explicit licensing, leading to a much more restrictive, less dynamic, and less accessible creative ecosystem. Tech platforms relying on UGC would face immense legal challenges.
% FOUNDING_PROBLEM: To provide a flexible, equitable safety valve in copyright law, preventing it from becoming an absolute monopoly that stifles creativity, free expression, and public access to knowledge and culture, while still incentivizing original authorship.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, cultural critics, and artists (both original and remix) widely attest to the ongoing tension between copyright protection and the need for new creation. Judicial opinions frequently reiterate the importance of fair use in balancing these interests, though the precise application remains contested.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).
:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (enabling new creative works and public discourse) but also involves asymmetric extraction. Original copyright holders bear the cost of diminished control and potential market harm, while transformative users and platforms benefit. Active enforcement through litigation is required to define and defend the boundaries of 'transformative use'. Extractiveness is moderate because it's a balancing act, but the subordination of market harm means original creators often lose out. Suppression is moderate as it limits original creators' control but enables new creators. Theater ratio reflects the performative aspects of legal arguments in complex fair use cases.
 *
 * PERSPECTIVAL GAP:
 *   Original copyright holders perceive this reading as an erosion of their property rights and a form of extraction, while transformative users and platforms view it as essential coordination for cultural progress and free expression. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it closer to a Rope and victims closer to a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Remix artists, user creators, tech platforms, and public domain advocates are beneficiaries, as the constraint enables their activities and reduces their legal burden. Original copyright holders and risk-averse creators are victims, as they lose control and potential revenue, or face legal uncertainty. Courts and lawyers act as agenda-setters, defining and enforcing the constraint's boundaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the transformative use doctrine as a pure Rope (ignoring the extraction from original creators) or a pure Snare (ignoring its genuine coordination function for new creation). It acknowledges the ongoing tension and the active enforcement required to maintain this specific balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_definition_ambiguity,
    'How consistently and predictably is ''transformativeness'' defined and applied across different courts and factual scenarios?',
    'Empirical analysis of judicial decisions over time, identifying patterns and divergences in the application of the transformative use factor.',
    'If highly inconsistent, the constraint''s effective suppression for both original creators (uncertainty of defense) and transformative users (uncertainty of permission) is higher than measured, pushing it closer to a Snare for both. If highly consistent, it functions more predictably as a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_definition_ambiguity, empirical, 'Ambiguity in the legal definition and application of ''transformativeness''.').

omega_variable(
    ai_generated_content_impact,
    'How will the rise of AI-generated content, often trained on copyrighted material, impact the interpretation and application of the transformative use doctrine?',
    'Future judicial rulings and legislative action specifically addressing AI''s role in fair use, and subsequent empirical analysis of their effects on creative industries.',
    'If AI training is broadly deemed transformative, it could significantly increase extraction from original creators and shift the constraint closer to a Snare. If AI output is rarely deemed transformative, it could increase suppression for AI developers and users, pushing it closer to a Rope for original creators.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_generated_content_impact, empirical, 'Uncertainty regarding fair use application to AI-generated content.').

omega_variable(
    balance_of_incentives,
    'At what point does prioritizing transformative use disincentivize original creation by eroding the economic value of copyright, rather than promoting it?',
    'Longitudinal economic studies tracking trends in original creative output, licensing revenue, and the prevalence of transformative works under different fair use interpretations.',
    'If original creation demonstrably declines, the constraint''s overall coordination function is undermined, and its extractive aspects become more salient, pushing it closer to a Snare. If original creation remains robust, the current balance is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_incentives, empirical, 'The optimal balance between incentivizing original creation and enabling transformative use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1988, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1988, 0.15).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(fair_tr_t2012, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2012, 0.23).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1976, 0.3).
narrative_ontology:measurement(fair_be_t1988, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1988, 0.35).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(fair_be_t2012, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2012, 0.43).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1976, 0.2).
narrative_ontology:measurement(fair_su_t1988, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1988, 0.25).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(fair_su_t2012, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2012, 0.33).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, copyright_licensing_regime).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, digital_content_monetization).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__user_centric_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'fair_use_four_factor_test' kernel, focusing on the dominance of transformative use. It is linked to sibling readings that emphasize creator rights or broader user access, as these interpretations are in constant dialogue and influence each other's application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
