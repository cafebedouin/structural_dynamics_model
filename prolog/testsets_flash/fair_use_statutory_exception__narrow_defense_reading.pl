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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Market Value Preservation Reading)
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint represents the 'narrow defense' reading of the fair use
 *   statutory exception in copyright law. Under this interpretation,
 *   copyright is primarily a property right, and fair use is an affirmative
 *   defense that must be narrowly construed, with significant emphasis placed
 *   on whether the secondary use harms the market for the original work. The
 *   burden of proof rests heavily on the defendant. This reading often leads
 *   to high extractiveness for unauthorized uses, as commercial nature is a
 *   determinative factor and transformativeness is underweighted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.78).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.65).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Market Value Preservation Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, 'c0dc956f-094e-4725-ae05-6963c1accf24').
narrative_ontology:cs_kernel_codification('c0dc956f-094e-4725-ae05-6963c1accf24', fixed_text).
narrative_ontology:cs_authority_grounding('c0dc956f-094e-4725-ae05-6963c1accf24', lineage).
narrative_ontology:cs_interpretation_layer_present('c0dc956f-094e-4725-ae05-6963c1accf24').
narrative_ontology:cs_reading_relation('c0dc956f-094e-4725-ae05-6963c1accf24', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0dc956f-094e-4725-ae05-6963c1accf24', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('c0dc956f-094e-4725-ae05-6963c1accf24', foundational, copyright_as_absolute_property_right).
narrative_ontology:cs_axiom_status(copyright_as_absolute_property_right, holdable).
narrative_ontology:cs_axiom_grounding('c0dc956f-094e-4725-ae05-6963c1accf24', copyright_as_absolute_property_right, deontological).
narrative_ontology:cs_axiom('c0dc956f-094e-4725-ae05-6963c1accf24', foundational, market_harm_as_primary_fair_use_factor).
narrative_ontology:cs_axiom_status(market_harm_as_primary_fair_use_factor, holdable).
narrative_ontology:cs_axiom_grounding('c0dc956f-094e-4725-ae05-6963c1accf24', market_harm_as_primary_fair_use_factor, instrumental).
narrative_ontology:cs_reference_frame('c0dc956f-094e-4725-ae05-6963c1accf24', traditional_property_rights_framework).
narrative_ontology:cs_drift_state('c0dc956f-094e-4725-ae05-6963c1accf24', contemporary_digital_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c0dc956f-094e-4725-ae05-6963c1accf24', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, licensing_agencies).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, remix_artists).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, scholars_educators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, independent_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own the exclusive rights to copyrighted works and actively enforce them. They benefit from a narrow interpretation of fair use that maximizes their control over derivative works and licensing revenue. They initiate infringement lawsuits.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, agenda_setter,
    institutional, generational, mobile, global).

% Represent copyright holders and facilitate the licensing of copyrighted material. They benefit from a legal framework that encourages licensing for most uses, thereby increasing their transaction volume and fees.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, licensing_agencies, beneficiary,
    organized, biographical, mobile, global).

% Create new works by incorporating existing copyrighted material. Under this reading, their uses are frequently deemed infringing, requiring them to seek licenses or face legal action, which often stifles their creative output due to cost or refusal.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, remix_artists, payer,
    powerless, immediate, constrained, global).

% Utilize copyrighted materials for teaching, research, and criticism. This reading makes them cautious about incorporating materials without explicit permission, leading to self-censorship or increased administrative burden for licensing.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, scholars_educators, payer,
    moderate, biographical, constrained, national).

% Produce original content but often rely on referencing or incorporating elements of popular culture. The narrow fair use defense makes them vulnerable to infringement claims, limiting their ability to engage in commentary or parody without significant legal risk.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, independent_creators, payer,
    powerless, biographical, constrained, global).

% Interpret and apply copyright law, including the fair use doctrine. Under this reading, they prioritize the market impact of the secondary use and the property rights of the copyright holder, often placing a heavy burden on defendants to prove fair use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts_judges, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for balancing copyright holders' exclusive rights with limited public access for certain uses, aiming to incentivize creation while allowing some reuse.
% TRANSFER_FUNCTION: Transfers control and potential licensing revenue from users of copyrighted material back to copyright holders, by limiting the scope of non-compensable uses.
% ABSENT_VOICES: Advocates for robust public domain, digital archivists, and open-source communities are often marginalized in discussions that prioritize market value and property rights. They would argue for broader exceptions to facilitate knowledge sharing and cultural preservation.
% DISAPPEARANCE_RATIONALE: If the narrow fair use defense vanished, copyright holders would lose a significant tool for enforcing their exclusive rights, leading to a surge in unauthorized uses and a collapse in licensing markets. The entire information economy built on copyright would need to fundamentally reorganize.
% FOUNDING_PROBLEM: To balance the exclusive rights of authors with the public interest in promoting the progress of science and useful arts, preventing copyright from becoming an absolute monopoly that stifles creativity.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and licensing agencies argue the problem is live, citing ongoing threats to their revenue streams from unauthorized digital uses. Remix artists and scholars argue the problem has shifted, with the current interpretation stifling new creation and public access, citing academic studies on chilling effects and legal analyses from public interest groups.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).

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
 *   Extractiveness is high (0.78) because this reading prioritizes the copyright holder's market control, leading to many uses being deemed infringing and requiring licensing. Suppression (0.65) is moderate because while legal avenues exist, the high cost of litigation and the narrow scope of defense actively deter many potential fair users. Theater ratio (0.20) is low, as the enforcement mechanism (litigation) is genuinely functional in protecting market value, even if its application is contested.
 *
 * PERSPECTIVAL GAP:
 *   Copyright holders and licensing agencies perceive this as a necessary and fair mechanism to protect their investments and incentivize creation. Conversely, remix artists and independent creators experience it as a significant barrier to creative expression and a tool for rent extraction, limiting their ability to build upon existing culture. Courts, while aiming for neutrality, often lean towards market preservation in their rulings under this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and licensing agencies are clear beneficiaries, as this reading maximizes their control and revenue. Remix artists, scholars, and independent creators are victims, bearing the costs of licensing or legal risk. Courts act as agenda-setters, interpreting and enforcing the constraint in a way that aligns with this reading's emphasis on property rights and market value.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by clearly identifying the coordination function (balancing rights) and the asymmetric extraction (favoring copyright holders). It highlights how a constraint, ostensibly for coordination, can become highly extractive when interpreted through a specific lens that prioritizes one set of stakeholders' interests over others, even if the founding problem (incentivizing creation) is still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_harm_definition_ambiguity,
    'How broadly should ''market harm'' be defined in fair use analysis? Does it include potential markets that the copyright holder has not yet exploited?',
    'Legislative clarification or Supreme Court ruling providing a definitive scope for ''market harm'' that explicitly addresses unexploited or speculative markets.',
    'A broad definition of market harm would further increase extractiveness and suppression for secondary users, pushing the constraint closer to a Snare. A narrow definition would reduce extractiveness and allow more uses to qualify as fair.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_definition_ambiguity, conceptual, 'Ambiguity in the scope of market harm in fair use analysis.').

omega_variable(
    transformative_use_weighting,
    'What weight should be given to the ''transformativeness'' of a secondary use, relative to its market impact, in fair use analysis?',
    'Judicial precedent that explicitly rebalances the four fair use factors, giving greater weight to transformativeness even when a potential market for the original exists.',
    'Increased weighting for transformativeness would reduce extractiveness for creators who build upon existing works, potentially shifting the constraint towards a more balanced Tangled Rope or even a Rope. Current weighting amplifies extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_weighting, conceptual, 'The relative importance of transformativeness versus market harm in fair use.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine interpretation of the fair use statute, or a constructed reading that primarily benefits identifiable agents (copyright holders and licensing agencies)?',
    'Comparative legal analysis across jurisdictions with different fair use interpretations, assessing whether alternative readings achieve the statute''s stated goals with less extraction, or historical analysis of legislative intent versus judicial evolution.',
    'If primarily constructed for benefit, the constraint''s classification would shift more definitively towards a Snare, highlighting the gap between claimed purpose and actual function. If a genuine interpretation, it remains a Tangled Rope with high extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this narrow reading of fair use is an inherent feature of the statute or a constructed interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1976, 0.6).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1976, 0.5).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
