% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__dependency_trap_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency_trap_reading of the
 *   income_support_conditionality kernel. The reading holds that
 *   unconditional income support, however well-intentioned, creates a
 *   structural welfare trap: recipients face effective marginal tax rates
 *   that make work financially irrational, skills degrade during
 *   non-participation, and identity fuses with recipient status. The
 *   arrangement extracts from taxpayers (who fund non-productive transfers)
 *   and from low-wage employers (who lose labor supply), while the
 *   administering bureaucracy and political incumbents benefit from the
 *   program's persistence. The constraint is classified as a snare — the
 *   coordination story (poverty elimination) is real but the extraction
 *   mechanism (dependency trap) dominates and persists through active
 *   enforcement of eligibility boundaries and benefit clawbacks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.72).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'b7d5cf28-ea43-42f5-93aa-12b4c443ced7').
narrative_ontology:cs_kernel_codification('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', implicit).
narrative_ontology:cs_authority_grounding('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', extraction).
narrative_ontology:cs_interpretation_layer_present('b7d5cf28-ea43-42f5-93aa-12b4c443ced7').
narrative_ontology:cs_reading_relation('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', foundational, unconditionality_creates_moral_hazard).
narrative_ontology:cs_axiom_status(unconditionality_creates_moral_hazard, holdable).
narrative_ontology:cs_axiom_grounding('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', unconditionality_creates_moral_hazard, empirically_contingent).
narrative_ontology:cs_axiom('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', foundational, work_is_necessary_for_human_flourishing).
narrative_ontology:cs_axiom_status(work_is_necessary_for_human_flourishing, holdable).
narrative_ontology:cs_axiom_grounding('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', work_is_necessary_for_human_flourishing, deontological).
narrative_ontology:cs_reference_frame('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', postwar_welfare_settlement).
narrative_ontology:cs_drift_state('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', contemporary_ubi_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b7d5cf28-ea43-42f5-93aa-12b4c443ced7', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, social_security_administration).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, welfare_bureaucracy).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, political_incumbents).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, low_wage_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income that meets basic subsistence but creates a high effective marginal tax rate on any earned income due to benefit clawbacks and loss of ancillary supports (housing, healthcare, childcare). Skills atrophy during prolonged absence from labor market; re-entry becomes progressively harder as gaps lengthen and references fade. Identity fuses with recipient status — 'someone who doesn't work' becomes a self-concept, not just a temporary condition. Exit requires not just a job offer but a job that clears the fiscal cliff, which rarely exists at the skill level available after years of non-participation.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer).

% Fund transfers that, under this reading, do not produce commensurate social return — recipients do not transition to productive employment, and the program lacks mechanisms to incentivize or enable such transition. The tax burden is diffuse but persistent; political voice is organized through fiscal conservative coalitions but exit from the funding obligation is constrained by citizenship and territorial jurisdiction. No individual taxpayer can opt out; collective exit requires policy reversal.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    organized, generational, constrained, national).

% Face a labor supply constraint — potential workers choose idleness over low-wage, precarious employment because the unconditional income plus in-kind benefits exceeds or matches take-home pay after commuting, childcare, and benefit losses. Employers cannot easily raise wages (margin pressure) and cannot easily automate (capital constraints). Some exit by relocating, outsourcing, or closing; others persist with chronic vacancies.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, low_wage_employers, payer,
    moderate, biographical, mobile, regional).

% Administers the unconditional income program, sets eligibility rules, manages clawback schedules, and controls the bureaucracy that processes claims. Institutional survival and budget growth depend on caseload maintenance; the agency has no structural incentive to reduce dependency. It can reallocate resources across programs, lobby for budget increases, and shape the narrative of 'need' — its exit options are those of a state bureaucracy with entrenched mission.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, social_security_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Frontline caseworkers, eligibility determiners, fraud investigators, and program evaluators whose positions exist because the program exists. Their professional identity, career progression, and union representation are tied to the program's scale. They benefit from stable employment and mission clarity but do not set the agenda — they implement it. Exit means leaving public sector employment for private or non-profit roles.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, welfare_bureaucracy, beneficiary,
    organized, biographical, arbitrage, national).

% Electoral coalitions include both recipient populations (mobilized by benefit protection) and fiscal conservatives (mobilized by anti-waste rhetoric). The program as structured allows incumbents to claim compassion while the dependency dynamic persists — they benefit from the arrangement's durability without owning its failure mode. Exit from the political arrangement means losing office; the constraint serves their re-election calculus.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, political_incumbents, beneficiary,
    institutional, immediate, arbitrage, national).

% Study the empirical relationship between income support generosity, conditionality, and labor supply elasticities. Their work informs the policy debate but they do not administer, fund, or receive the transfers. Exit is analytical — they can change their model specification or theoretical frame.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a guaranteed income floor that prevents destitution and coordinates a minimal standard of living across the population without means-testing overhead or stigmatizing eligibility determinations.
% TRANSFER_FUNCTION: Moves tax revenue from the general population (disproportionately middle- and upper-income earners) to non-employed adults, with no work requirement, time limit, or reciprocity condition. The transfer is large enough to sustain subsistence but structured so that marginal earned income faces effective tax rates exceeding 80-100% due to benefit phase-outs.
% ABSENT_VOICES: Working poor who earn just above the eligibility threshold and receive no support — they bear the tax burden without the transfer. Their objections are structurally silenced by income segmentation; they are not organized as a constituency. Future generations who inherit the fiscal liabilities and the normalized dependency culture — they cannot vote, organize, or exit.
% DISAPPEARANCE_RATIONALE: If the unconditional income support vanished overnight, recipients would face immediate destitution and be forced into any available employment regardless of match quality, wages, or conditions — a massive, disorderly labor supply shock. Employers would see a sudden flood of desperate applicants, depressing wages further. The fiscal transfer would cease, creating a windfall for taxpayers but a humanitarian crisis for recipients. The social contract would be visibly broken, triggering political instability.
% FOUNDING_PROBLEM: Post-war welfare states sought to eliminate absolute poverty and provide security against labor market volatility, but means-tested systems created poverty traps (high marginal tax rates), stigma, and administrative complexity. Unconditional income was proposed as a simplification that would preserve the floor while removing the trap.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (poverty traps in means-tested systems) is attested as substantially solved by the OECD (2018) review of benefit redesigns that tapered withdrawal rates — independent of the UBI advocacy coalition. The dependency trap reading's proponents (Murray 2006, Glaeser 2018) argue the UBI solution created a worse trap; this is corroborated by negative income tax experiments (Seattle/Denver 1970s, Manitoba 1970s) showing labor supply reductions concentrated among secondary earners and youth, not primary breadwinners — but those studies are contested by UBI advocates who cite different subsets. No neutral arbiter has settled the empirical dispute.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the scale of transfers that do not generate reciprocal productive activity — the fiscal flow is large and the counterfactual productive contribution of recipients is suppressed by the constraint's own incentive structure. Suppression (0.72) is high because the constraint actively prevents exit: benefit clawbacks create fiscal cliffs, administrative barriers discourage re-entry, and the identity lock makes psychological exit costly. Theater ratio (0.22) is modest — the poverty alleviation function is real and not purely performative, but a growing share of administrative effort manages the dependency rather than solving it. Accessibility collapse (0.61) is significant: once in the trap, alternatives (employment, retraining, entrepreneurship) are structurally blocked by the incentive architecture. Resistance (0.48) is moderate — fiscal conservatives resist the cost, recipients resist the stigma and control, but neither coalition has achieved structural reform.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda setter's seat, the constraint is a rope — it coordinates a poverty floor with minimal stigma and administrative overhead. From the payer seats (recipients, taxpayers, low-wage employers), it computes as a snare — extraction without reciprocity, enforced by the very structure that claims to help. The engine will compute this divergence from the structural data; the claimed_type (snare) reflects the analytical seat's assessment that the extraction dominates.
 *
 * DIRECTIONALITY LOGIC:
 *   UBI recipients are the primary targets (d → 1.0): they bear the skill atrophy, identity lock, and fiscal cliff, while the nominal benefit is the trap's bait. Taxpayers are secondary targets (d → 0.8): they fund the extraction but their exit is constrained by citizenship. Low-wage employers are tertiary targets (d → 0.6): they lose labor supply but have mobility options. The social security administration is the agenda setter (d → 0.1): it controls the rule structure and benefits from caseload growth. Welfare bureaucracy and political incumbents are beneficiaries (d → 0.15-0.2): they collect rents (employment, votes) from the constraint's operation. Labor economists are analytical observers (d → 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (poverty traps in means-tested welfare) is dead — modern tapering designs have largely solved the marginal tax rate problem without unconditionality. But the unconditional arrangement persists and has metastasized into a dependency trap. The mandatrophy is resolved: the mandate (eliminate poverty traps) has been inverted — the constraint now creates a deeper trap. The theater ratio rise over time tracks this inversion: the coordination function atrophies while the extraction function hardens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dispute_location,
    'Which structural element of the income_support_conditionality kernel do the three readings fundamentally disagree on — the victim set, the extraction mechanism, the coordination function, or the constraint type?',
    'Map each reading''s authored base_properties (beneficiaries, victims, extractiveness, suppression, claimed_type) and identify the minimal set of divergent atoms. The disagreement locus is the structural element whose value differs across all three readings.',
    'If the readings share the same victim set but differ on constraint type, the kernel dispute is evaluative (is this extraction justified?). If they differ on victim set, the kernel dispute is ontological (who is harmed?). This determines whether the readings are commensurable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_dispute_location, conceptual, 'Location of the structural disagreement among sibling readings of the income_support_conditionality kernel.').

omega_variable(
    dependency_trap_empirical_verification,
    'Does unconditional income support at subsistence level actually produce the labor supply reductions and skill atrophy this reading predicts, or do recipients use the floor for productive risk-taking (entrepreneurship, care work, retraining)?',
    'Longitudinal RCT or natural experiment with unconditional income at subsistence level (not negative income tax with high withdrawal rates), tracking labor market transitions, skill acquisition, and subjective well-being over 5+ years. Compare to matched control under conditional welfare.',
    'If labor supply effects are negligible or positive, the dependency trap reading''s victim structure collapses — recipients are not trapped, the snare classification fails, and the constraint recomputes as rope or scaffold. If effects are strongly negative, the reading''s structural claims are verified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dependency_trap_empirical_verification, empirical, 'Whether the dependency trap mechanism operates as modeled at subsistence-level unconditionality.').

omega_variable(
    identity_lock_mechanism,
    'Is the recipient''s identity lock (self-concept as ''non-worker'') caused by the unconditional income itself, by the stigma of receipt, by the skill atrophy, or by the social isolation of non-participation — and would it persist under a different framing of the same transfer (e.g., ''citizen dividend'' vs. ''welfare'')?',
    'Qualitative longitudinal study of recipients under different rhetorical framings of identical transfer amounts, measuring identity narratives, labor market search behavior, and re-entry outcomes. Control for transfer level, vary only the cultural script.',
    'If identity lock is framing-dependent, the suppression mechanism is partially internalized and socially constructed — the constraint''s effective suppression is lower than structural measures suggest, and reframing could reduce extraction. If framing-invariant, the lock is structural to the transfer''s incentive architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether the identity_lock exit option for recipients is structurally necessary or culturally mediated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__dependency_trap_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__dependency_trap_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__dependency_trap_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(inco_tr_t32, income_support_conditionality__dependency_trap_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__dependency_trap_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__dependency_trap_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__dependency_trap_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__dependency_trap_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(inco_be_t32, income_support_conditionality__dependency_trap_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__dependency_trap_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(inco_su_t8, income_support_conditionality__dependency_trap_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(inco_su_t16, income_support_conditionality__dependency_trap_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(inco_su_t24, income_support_conditionality__dependency_trap_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(inco_su_t32, income_support_conditionality__dependency_trap_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(inco_su_t40, income_support_conditionality__dependency_trap_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, labor_market_activation_requirements).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, earned_income_tax_credit_structure).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, universal_basic_services_provision).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the income_support_conditionality kernel. The dependency_trap_reading (this file) sees victims on both recipient and taxpayer sides, classifying the kernel as a snare. The freedom_floor_reading sees UBI as decommodifying labor power (positive freedom), classifying as rope. The wage_subsidy_reading sees UBI as subsidizing low-wage employers, classifying as tangled_rope. The three readings disagree on victim set, beneficiary set, and constraint type — the kernel is contested at the structural level, not merely evaluative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, powerless, 0.95).
constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, organized, 0.75).
constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, institutional, 0.15).
constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
