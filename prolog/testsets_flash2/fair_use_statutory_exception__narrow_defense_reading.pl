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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Preserving Market Value)
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the fair use doctrine in
 *   copyright law, where fair use is interpreted as a narrow affirmative
 *   defense, with a strong emphasis on preserving the market value of the
 *   copyrighted work. This reading prioritizes the property rights of
 *   copyright holders and places a significant burden on those claiming fair
 *   use, particularly when the use has any commercial aspect or could
 *   substitute for a licensed use. It is one of several contested
 *   interpretations of the fair use kernel.
 *
 * KEY AGENTS:
 *   - copyright_holders: Primary beneficiary/agenda_setter (institutional/mobile) — benefits from narrow interpretation, enforces property rights.
 *   - secondary_creators: Primary target/payer (moderate/constrained) — bears high legal risk, self-censorship.
 *   - educators: Target/payer (moderate/constrained) — faces legal complexity, limits pedagogical innovation.
 *   - researchers: Target/payer (moderate/constrained) — impeded in computational research.
 *   - public_domain_advocates: Excluded (organized/trapped) — marginalized in the interpretive process.
 *   - courts: Agenda_setter (institutional/constrained) — adjudicates claims, tends to prioritize market harm.
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
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Preserving Market Value)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '8a37fa25-a7cb-46bf-bd74-06e485f7032c').
narrative_ontology:cs_kernel_codification('8a37fa25-a7cb-46bf-bd74-06e485f7032c', fixed_text).
narrative_ontology:cs_authority_grounding('8a37fa25-a7cb-46bf-bd74-06e485f7032c', lineage).
narrative_ontology:cs_interpretation_layer_present('8a37fa25-a7cb-46bf-bd74-06e485f7032c').
narrative_ontology:cs_reading_relation('8a37fa25-a7cb-46bf-bd74-06e485f7032c', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a37fa25-a7cb-46bf-bd74-06e485f7032c', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('8a37fa25-a7cb-46bf-bd74-06e485f7032c', foundational, copyright_as_absolute_property_right).
narrative_ontology:cs_axiom_status(copyright_as_absolute_property_right, holdable).
narrative_ontology:cs_axiom_grounding('8a37fa25-a7cb-46bf-bd74-06e485f7032c', copyright_as_absolute_property_right, deontological).
narrative_ontology:cs_axiom('8a37fa25-a7cb-46bf-bd74-06e485f7032c', foundational, market_value_preservation_as_primary_goal).
narrative_ontology:cs_axiom_status(market_value_preservation_as_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('8a37fa25-a7cb-46bf-bd74-06e485f7032c', market_value_preservation_as_primary_goal, instrumental).
narrative_ontology:cs_reference_frame('8a37fa25-a7cb-46bf-bd74-06e485f7032c', traditional_property_rights_framework).
narrative_ontology:cs_drift_state('8a37fa25-a7cb-46bf-bd74-06e485f7032c', contemporary_digital_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8a37fa25-a7cb-46bf-bd74-06e485f7032c', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, secondary_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, educators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, researchers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert their property rights, seeking to maximize licensing revenue and control over derivative works. They initiate infringement lawsuits and advocate for interpretations of fair use that prioritize market preservation and minimize exceptions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, agenda_setter,
    institutional, generational, mobile, global).

% Seek to build upon existing works through commentary, parody, or new artistic expressions. Under this reading, they face high legal risk and potential liability, often leading to self-censorship or costly licensing, even for uses they believe are fair.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, secondary_creators, payer,
    moderate, biographical, constrained, global).

% Utilize copyrighted materials for teaching and non-commercial educational purposes. This reading forces them to navigate complex legal assessments, often leading to conservative choices to avoid infringement claims, limiting pedagogical innovation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, educators, payer,
    moderate, biographical, constrained, national).

% Rely on access to and analysis of existing works for scholarly inquiry. The narrow construction of fair use can impede data mining, text analysis, and other forms of computational research, requiring extensive permissions or legal risk assessment.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, researchers, payer,
    moderate, biographical, constrained, global).

% Argue for a robust public domain and broad exceptions to copyright to foster creativity and access to knowledge. Their arguments for a more expansive fair use are often marginalized in legal interpretations that prioritize property rights and market control.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, public_domain_advocates, excluded,
    organized, generational, trapped, global).

% Adjudicate fair use claims, often balancing competing interests. Under this reading, they tend to emphasize the commercial impact of the use and the potential for market substitution, placing a heavy burden on defendants to prove their use is fair.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for resolving disputes over unauthorized uses of copyrighted material, aiming to balance creator incentives with limited public access, primarily by defining the boundaries of permissible unlicensed use.
% TRANSFER_FUNCTION: Transfers potential licensing revenue and control over derivative works from secondary creators, educators, and researchers to copyright holders, by narrowly defining exceptions to exclusive rights.
% ABSENT_VOICES: Advocates for a robust public domain, open access, and transformative creativity are often excluded from the core interpretive process, as the debate is framed primarily around property rights and market harm, rather than cultural commons or innovation incentives.
% DISAPPEARANCE_RATIONALE: If this narrow reading of fair use vanished, copyright holders would face significantly increased challenges to their exclusive rights, leading to a surge in unlicensed uses, a re-evaluation of licensing models, and potentially a shift in the balance of power between creators and re-users. The information economy would reorganize around a more permissive reuse environment.
% FOUNDING_PROBLEM: The original problem was how to allow some socially beneficial uses of copyrighted works without undermining the economic incentives for creators, recognizing that absolute copyright control could stifle creativity and public access.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders attest the problem is live, citing ongoing threats to their market. Secondary creators and public domain advocates also attest the problem is live, but argue this reading exacerbates it by overly restricting beneficial uses. Legal scholars and economists outside the direct beneficiaries corroborate the ongoing tension between incentives and access.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.78) reflects the significant costs imposed on secondary creators and users through licensing fees, legal risks, and self-censorship, all stemming from the narrow scope of permissible unlicensed use. Suppression (0.65) is moderate, as the legal framework actively discourages unauthorized uses through litigation and the threat of injunctions, even if not all potential uses are actively litigated. The theater ratio is low (0.15) because the enforcement mechanism (litigation, cease-and-desist letters) is directly functional in protecting market value, rather than performative. Accessibility collapse is high (0.70) because the legal uncertainty and high cost of litigation make many otherwise beneficial uses practically inaccessible without permission. Resistance (0.45) is present but often diffuse, manifesting as advocacy for legislative reform or occasional high-profile legal challenges, rather than widespread non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   Copyright holders and courts (as adjudicators) perceive this reading as a necessary and legitimate mechanism for protecting property rights and fostering creative incentives. Secondary creators, educators, and researchers, however, experience it as an extractive barrier to cultural production, education, and research, forcing them to pay for uses they believe should be free or face prohibitive legal costs. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders are clear beneficiaries (d near 0.0) as the constraint directly secures and enhances their revenue streams and control. Secondary creators, educators, and researchers are targets (d near 1.0) as they bear the costs of licensing or legal risk. Courts, while agenda-setters, are also constrained by precedent and statutory language, placing them closer to symmetric (d near 0.5) in their role as adjudicators, though their interpretation here benefits copyright holders. Public domain advocates are excluded, their d value reflecting their structural inability to influence the constraint's operation from within.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling extraction as coordination by explicitly identifying the transfer of value from users to copyright holders as a primary outcome, rather than solely focusing on the 'incentive' coordination function. The rising extractiveness over time suggests an accumulation of rents beyond what might be strictly necessary for coordination, indicating a potential drift towards a Snare, even if claimed as a Rope or Tangled Rope by its proponents. The 'contested' status of the founding problem further highlights the potential for mandatrophy, where the original coordination problem (balancing incentives and access) may have been superseded by a focus on maximizing property value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_harm_definition,
    'How broadly should ''market harm'' be construed in fair use analysis? Does it include hypothetical markets that a copyright holder has not yet entered?',
    'Legislative clarification or Supreme Court ruling explicitly defining the scope of ''potential market'' harm, or empirical studies on the actual impact of unlicensed uses on existing and nascent markets.',
    'A broad construction of market harm (as in this reading) increases extractiveness and suppression; a narrow construction (limited to existing or clearly planned markets) would reduce both, potentially shifting the classification towards a more balanced Tangled Rope or even a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_definition, conceptual, 'Ambiguity in defining market harm, a key factor in fair use analysis.').

omega_variable(
    transformative_use_weighting,
    'What weight should ''transformativeness'' carry relative to ''market harm'' in fair use analysis? Is a highly transformative use still fair if it causes some market harm?',
    'Judicial precedent that explicitly prioritizes transformativeness over market harm in certain contexts, or legislative reform that rebalances the fair use factors.',
    'Under this reading, transformativeness is underweighted, leading to higher extractiveness. If transformativeness were given greater weight, more uses would be deemed fair, reducing extractiveness and suppression, potentially shifting the constraint''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_weighting, preference, 'The relative importance of transformativeness versus market harm in fair use.').

omega_variable(
    burden_of_proof_shift,
    'Should the burden of proving fair use remain entirely on the defendant, or should the plaintiff bear some burden to demonstrate actual market harm?',
    'Procedural rule changes or judicial guidance that reallocates the burden of proof in fair use cases.',
    'Shifting some burden to the plaintiff would reduce the legal costs and risks for secondary creators, effectively lowering suppression and extractiveness by making fair use easier to assert. Maintaining the current burden (as in this reading) reinforces the high extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_of_proof_shift, empirical, 'The allocation of the burden of proof in fair use claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fair_be_t1980, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1980, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, digital_millennium_copyright_act_anti_circumvention).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'fair_use_statutory_exception' kernel. Its high extractiveness and emphasis on market value distinguish it from the 'transformative_right_reading' and 'market_licensing_reading', which represent different structural claims about the purpose and scope of fair use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
