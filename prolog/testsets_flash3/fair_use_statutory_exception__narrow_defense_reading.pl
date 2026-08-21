% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Property-Centric Reading)
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the fair use doctrine in
 *   copyright law, where fair use is construed as a narrow affirmative
 *   defense, primarily to protect the market value of copyrighted works. This
 *   reading emphasizes copyright as a property right, placing a high burden
 *   on defendants to prove their use is fair, and often prioritizing the
 *   commercial impact of a use over its transformative nature. This is one of
 *   several contested interpretations of the fair use kernel.
 *
 * KEY AGENTS:
 *   - copyright_holders: Primary beneficiary/agenda_setter (institutional/mobile)
 *   - licensing_agencies: Beneficiary (organized/constrained)
 *   - unauthorized_users: Primary payer (powerless/trapped)
 *   - transformative_creators: Payer (moderate/constrained)
 *   - public_domain_advocates: Excluded (organized/analytical)
 *   - courts: Agenda_setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.78).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.65).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Property-Centric Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '73cdd122-6194-44ff-8c77-b4e07d01d950').
narrative_ontology:cs_kernel_codification('73cdd122-6194-44ff-8c77-b4e07d01d950', fixed_text).
narrative_ontology:cs_authority_grounding('73cdd122-6194-44ff-8c77-b4e07d01d950', lineage).
narrative_ontology:cs_interpretation_layer_present('73cdd122-6194-44ff-8c77-b4e07d01d950').
narrative_ontology:cs_reading_relation('73cdd122-6194-44ff-8c77-b4e07d01d950', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('73cdd122-6194-44ff-8c77-b4e07d01d950', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('73cdd122-6194-44ff-8c77-b4e07d01d950', foundational, copyright_as_absolute_property_right).
narrative_ontology:cs_axiom_status(copyright_as_absolute_property_right, holdable).
narrative_ontology:cs_axiom_grounding('73cdd122-6194-44ff-8c77-b4e07d01d950', copyright_as_absolute_property_right, deontological).
narrative_ontology:cs_axiom('73cdd122-6194-44ff-8c77-b4e07d01d950', foundational, market_preservation_as_primary_goal).
narrative_ontology:cs_axiom_status(market_preservation_as_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('73cdd122-6194-44ff-8c77-b4e07d01d950', market_preservation_as_primary_goal, instrumental).
narrative_ontology:cs_reference_frame('73cdd122-6194-44ff-8c77-b4e07d01d950', traditional_property_rights_framework).
narrative_ontology:cs_drift_state('73cdd122-6194-44ff-8c77-b4e07d01d950', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('73cdd122-6194-44ff-8c77-b4e07d01d950', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, licensing_agencies).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, unauthorized_users).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert their exclusive rights, seeking to maximize revenue from licensing and control over derivative works. They view fair use as a limited exception that should not erode their market. This reading aligns with their economic interests.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, agenda_setter,
    institutional, generational, mobile, global).

% Profit from facilitating the licensing of copyrighted works. A narrow fair use defense expands the scope of uses requiring a license, increasing their market and revenue. They actively lobby for this interpretation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, licensing_agencies, beneficiary,
    organized, biographical, constrained, national).

% Face potential infringement lawsuits for uses that might be considered fair under broader interpretations. The burden of proving fair use is on them, and the risk of litigation often deters even potentially legitimate uses.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, unauthorized_users, payer,
    powerless, immediate, trapped, global).

% Seek to build upon existing works to create new, transformative content. This reading makes their work legally precarious, requiring them to either seek licenses (often prohibitively expensive) or risk litigation, stifling innovation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, transformative_creators, payer,
    moderate, biographical, constrained, global).

% Argue for a robust public domain and broad user rights to foster creativity and access to knowledge. This reading significantly curtails those rights, pushing more content into proprietary control. They are often excluded from direct legislative or judicial influence on this interpretation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, public_domain_advocates, excluded,
    organized, generational, analytical, global).

% Are tasked with interpreting and applying copyright law, including fair use. This reading guides their decisions towards protecting copyright holders' market interests and placing a high burden on defendants claiming fair use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for copyright holders to monetize their creative works and for users to understand the boundaries of permissible use, theoretically fostering creative production by ensuring creators are compensated.
% TRANSFER_FUNCTION: Transfers economic value from unauthorized users and transformative creators to copyright holders and licensing agencies by restricting the scope of free use and expanding the need for paid licenses.
% ABSENT_VOICES: Advocates for a robust public domain, open access, and user rights are often marginalized in the legal and lobbying processes that shape this interpretation. They would argue for a balance that prioritizes cultural production and access over strict property rights.
% DISAPPEARANCE_RATIONALE: If this narrow reading of fair use vanished, the landscape of digital content creation, distribution, and monetization would fundamentally shift. More uses would be deemed fair, reducing licensing revenue for copyright holders and potentially fostering a surge in transformative works. The entire information economy would need to re-evaluate its business models.
% FOUNDING_PROBLEM: The original copyright statute aimed to balance creator incentives with public access, but the digital age introduced new forms of copying and reuse that challenged this balance, leading to disputes over the scope of 'fair' exceptions.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and licensing agencies attest the problem is live, citing ongoing piracy and market erosion. Transformative creators and public domain advocates contest this, arguing the problem has shifted from piracy to over-enforcement and stifled innovation; legal scholars and economists outside the benefiting parties corroborate the contested nature of the problem's current manifestation.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because this reading significantly limits the scope of free use, forcing more users into licensing agreements or deterring them entirely. Suppression (0.65) is substantial due to the legal costs and risks associated with challenging copyright holders, effectively chilling many potentially fair uses. Theater ratio is low (0.15) as the enforcement is genuinely aimed at protecting market value, not merely performing a function. The trend shows increasing extractiveness and suppression as this reading has gained prominence, particularly with the rise of digital content and easier enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Copyright holders and licensing agencies perceive this as a necessary and just framework for protecting creative industries. Unauthorized users and transformative creators experience it as an extractive barrier to cultural participation and innovation. Courts, as agenda-setters, navigate these competing claims, but this reading guides them toward a property-centric outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and licensing agencies are clear beneficiaries, as this reading directly enhances their control and revenue (low d). Unauthorized users and transformative creators are targets, bearing the costs of licensing or litigation (high d). Public domain advocates are excluded, their interests not directly represented in the operational logic of this reading. Courts, while agenda-setters, are also constrained by precedent and statutory language, placing them closer to symmetric in their operational relationship to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (coordination) by highlighting the significant extraction and suppression. While it provides a framework for market coordination, the asymmetry of benefits and costs, coupled with active enforcement against alternatives, points to a Tangled Rope. The 'property' framing often masks the extractive nature by presenting it as a natural right.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_harm_definition,
    'How is ''harm to the market for copyrighted works'' defined and measured, particularly for transformative uses that may not directly compete with the original?',
    'Empirical studies on the actual market impact of various transformative uses, distinguishing between substitutive and complementary effects, and judicial clarification on the scope of ''potential market''.',
    'A broader definition of ''market harm'' (e.g., including any potential licensing opportunity) would increase extractiveness; a narrower definition (e.g., only direct substitution) would decrease it, potentially shifting the constraint towards a Rope for transformative creators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_definition, empirical, 'Ambiguity in defining market harm, central to fair use analysis.').

omega_variable(
    transformative_use_weighting,
    'What weight should ''transformativeness'' carry in the fair use analysis, relative to the ''effect on the market'' factor?',
    'Legislative amendment clarifying the relative importance of fair use factors, or a Supreme Court ruling providing a definitive weighting framework.',
    'Increased weighting for transformativeness would reduce extractiveness for creators and shift the constraint towards a Rope for them; decreased weighting would reinforce its current Tangled Rope nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_weighting, preference, 'The relative importance of transformativeness vs. market effect in fair use.').

omega_variable(
    burden_of_proof_fair_use,
    'Is the current allocation of the burden of proof for fair use (on the defendant) appropriate, or should it shift, particularly for non-commercial or highly transformative uses?',
    'Legislative reform or judicial precedent shifting the burden of proof for certain categories of use.',
    'Shifting the burden of proof away from the defendant would reduce suppression and extractiveness for users, making it easier to assert fair use and potentially moving the constraint closer to a Rope for them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_fair_use, conceptual, 'Whether the burden of proof for fair use should remain on the defendant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fair_tr_t1998, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(fair_tr_t2006, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2006, 0.14).
narrative_ontology:measurement(fair_tr_t2014, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(fair_be_t1998, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(fair_be_t2006, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2006, 0.72).
narrative_ontology:measurement(fair_be_t2014, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2014, 0.75).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(fair_su_t1998, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(fair_su_t2006, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2006, 0.6).
narrative_ontology:measurement(fair_su_t2014, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2014, 0.63).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, digital_millennium_copyright_act_anti_circumvention).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'fair_use_statutory_exception' kernel. Its property-centric interpretation influences other copyright-related constraints by reinforcing a broad scope of exclusive rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
