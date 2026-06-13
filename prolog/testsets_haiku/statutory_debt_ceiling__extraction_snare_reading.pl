% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling as Minority Extraction Mechanism
 *   domain: constitutional/political/economic
 *
 * SUMMARY:
 *   The statutory debt ceiling is a cap on total federal borrowing, set by
 *   Congress. This constraint story instantiates the EXTRACTION SNARE
 *   READING: the ceiling is weaponized by a legislative minority faction to
 *   extract policy concessions from the majority and executive under threat
 *   of default. The minority faction — representing approximately 40% of
 *   congressional seats — withholds votes for ceiling increases unless the
 *   majority accepts policy riders, budget cuts, regulatory rollbacks, or
 *   other concessions. The threat is credible because default would destroy
 *   credit markets; the minority's votes are necessary because ceiling
 *   increases require supermajority support under current filibuster rules.
 *   The constraint persists because its founding problem (preventing
 *   executive fiscal micromanagement) is dead, but the ceiling mechanism is
 *   now exclusively useful as an extraction lever. The foundational axiom of
 *   this reading is that the ceiling is a MEANS TO MINORITY VETO, not a
 *   deliberative check; the coordinate constraint family includes readings
 *   that frame it as a coordination scaffold and readings that frame it as
 *   constitutionally void. This story presents the extraction reading in full
 *   clarity.
 *
 * KEY AGENTS:
 *   - legislative_minority_faction: holds the veto via ceiling hostage mechanism; extracts policy concessions
 *   - executive_branch: trapped between legal obligation to pay appropriated funds and legal prohibition on exceeding the ceiling; pays extraction in policy concessions
 *   - majority_congress: negotiates with minority to unlock ceiling votes; accepts policy riders as extraction cost
 *   - credit_markets: price default risk, suffer spreads and repricing uncertainty during standoffs
 *   - federal_beneficiaries: face immediate payment suspensions; absorb extraction costs in real hardship
 *   - constitutional_scholars: document unconstitutionality but are excluded from the decision structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.81).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.72).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Minority Extraction Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional/political/economic").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '93edbe34-a95b-4678-9551-0b02423a668b').
narrative_ontology:cs_kernel_codification('93edbe34-a95b-4678-9551-0b02423a668b', formalized).
narrative_ontology:cs_authority_grounding('93edbe34-a95b-4678-9551-0b02423a668b', extraction).
narrative_ontology:cs_reading_relation('93edbe34-a95b-4678-9551-0b02423a668b', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('93edbe34-a95b-4678-9551-0b02423a668b', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_axiom('93edbe34-a95b-4678-9551-0b02423a668b', foundational, debt_ceiling_minority_veto_mechanism).
narrative_ontology:cs_axiom_status(debt_ceiling_minority_veto_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('93edbe34-a95b-4678-9551-0b02423a668b', debt_ceiling_minority_veto_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('93edbe34-a95b-4678-9551-0b02423a668b', foundational, founding_problem_obsolescence).
narrative_ontology:cs_axiom_status(founding_problem_obsolescence, holdable).
narrative_ontology:cs_axiom_grounding('93edbe34-a95b-4678-9551-0b02423a668b', founding_problem_obsolescence, empirically_contingent).
narrative_ontology:cs_reference_frame('93edbe34-a95b-4678-9551-0b02423a668b', deliberative_congressional_fiscal_check).
narrative_ontology:cs_drift_state('93edbe34-a95b-4678-9551-0b02423a668b', contemporary_2024, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('93edbe34-a95b-4678-9551-0b02423a668b', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, executive_branch).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, majority_congress).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, credit_markets).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, government_beneficiaries).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) and rising because the minority faction's use of the ceiling as leverage has accelerated since 2011, with each standoff resulting in explicit policy extraction (the 2011 debt ceiling crisis extracted spending cap agreements; the 2015 crisis extracted Planned Parenthood defunding; the 2023 crisis extracted spending caps and border restrictions). The measurement series tracks the escalation from 1995 (low extractiveness, routine ceiling increases) through 2024 (high extractiveness, routine hostage dynamics). Suppression is high (0.72) because the constraint's persistence depends on active suppression of constitutional challenge, legislative reform, and alternative mechanisms: Congress does not discuss abolishing the ceiling despite documented economic costs; courts declare it a political question; the minority faction uses the hostage mechanism precisely because the suppressive silence around constitutional doubts enables their extraction. Theater is moderate (0.38) because the constraint still carries rhetorical cover (fiscal responsibility, Congressional authority) even though the machinery is now exclusively extractive. Accessibility collapse is low-moderate (0.48) because alternatives exist (constitutional amendment, legislative abolition, executive reinterpretation) but are institutionally suppressed — they are not naturally collapsed by the constraint's operation but artificially closed off by the framework that sustains the ceiling.
 *
 * PERSPECTIVAL GAP:
 *   The gap is acute because the constraint exhibits an inversion of stated function — the ceiling was claimed as a check on executive fiscal policy; it now functions as a check on the majority coalition and the executive's ability to implement majority-authorized spending. The minority's rhetorical claim (fiscal responsibility, Congressional prerogative) persists even as the structural reality (hostage extraction) becomes increasingly visible. The engine's per-seat classification should reveal this inversion: from the minority seat, the constraint may compute as rope (coordination of the delegation of fiscal authority); from the majority and executive seats, it computes as snare (enforced extraction); from the constitutional scholar's seat, it computes as a violation of a superseding constraint (14th Amendment Section 4). The authored claim (snare overall) reflects the structural dominance of extraction; the seats will diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative minority faction is the beneficiary (extracts policy concessions, wields the veto mechanism, collects in the form of forced legislative change). The executive, majority Congress, credit markets, and federal beneficiaries are all victims (pay in the forms of policy concessions, delayed legislation, economic repricing, and payment suspensions respectively). The directionality flows from beneficiary (d near 0.0) to victims (d near 1.0). The minority faction's power is 'organized' rather than 'institutional' because their leverage is contingent on their voting bloc, not on the institutional role they hold — the same institutional seats held by a different voting bloc would not wield the same leverage. Their time horizon is 'biographical' because the minority faction's members serve terms of years; the constraint persists across Congresses but the immediate actors have biographical horizons.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (executive fiscal micromanagement via repeated borrowing authorization) is dead. The ceiling persists because the minority faction has captured it as a veto lever. This is precisely the mandatrophy pattern: a constraint whose original function has atrophied but whose machinery remains useful for a beneficiary, so it is maintained theatrically and used for extraction. The theater ratio at 0.38 reflects that the constraint still carries fiscal-responsibility rhetoric even though the machinery is now purely extractive. The gap between founding problem (dead) and current function (extraction) is the mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_authority_vs_hostage_mechanism,
    'Is the debt ceiling a constitutionally authorized check on executive fiscal policy, or a constitutionally prohibited hostage mechanism violating the 14th Amendment Section 4?',
    'Supreme Court review of Section 4 enforcement, congressional passage of clarifying legislation establishing legislative intent, or federal-court precedent on the ceiling''s constitutional status. The constitutional-nullity reading would be vindicated by a ruling that the ceiling is void; this extraction reading would be vindicated by a ruling that the ceiling is valid but systematically misused as a hostage mechanism.',
    'A Section 4 ruling invalidating the ceiling would eliminate the extraction mechanism entirely and reclassify this constraint as a dead political-question artifact. A ruling sustaining the ceiling would require alternative reforms (supermajority-only exceptions, automatic increases, or legislative process change) to prevent extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_authority_vs_hostage_mechanism, conceptual, 'Whether the constitutional interpretation forecloses the ceiling or sustains it as legitimate.').

omega_variable(
    minority_faction_structural_necessity,
    'Is the minority faction''s leverage structurally necessary to the ceiling''s operation, or contingent on current legislative rules (filibuster, supermajority thresholds)?',
    'Congressional rule change (elimination of filibuster, lowering of majority requirement for ceiling increases, or automatic-ceiling-increase mechanisms). If rule change eliminates the minority''s veto, the constraint reclassifies from snare to a weaker rope or mountain (natural ceiling derived from economic constraints, not legislative hostage).',
    'If minority leverage is contingent on rules, rule change would eliminate the extraction mechanism. If structural, rule change would create political crisis and force Constitutional amendment to permanently resolve the ceiling question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_faction_structural_necessity, empirical, 'Whether minority veto is inherent to the ceiling or contingent on legislative procedure.').

omega_variable(
    economic_cost_vs_political_benefit,
    'Do the policy concessions extracted by the minority faction justify the economic costs (credit repricing, market uncertainty, payment suspensions) to the broader economy?',
    'Comparative institutional analysis: cost-benefit assessment of specific extracted concessions (budget caps, regulatory rollbacks) against measured economic impacts (GDP drag, interest-rate increases, employment disruption). No purely objective answer exists (values-dependent); the resolution mechanism would be political judgment by the majority coalition.',
    'If costs demonstrably exceed benefits, the majority coalition gains normative authority to eliminate the ceiling by reconciliation or amendment. If benefits are substantial, the minority''s extractive power becomes partially legitimate as a check on fiscal excess.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_cost_vs_political_benefit, preference, 'Whether extraction benefits justify economic costs — a values question, not empirical.').

omega_variable(
    institutional_commitment_conflict,
    'This reading frames the ceiling as one instantiation of a contested kernel — the 14th Amendment Section 4 reading would foreclose the extraction reading in any single constitutional framework. Can the US institutional system coherently hold both the ceiling and the 14th Amendment''s debt-protection clause, or does one necessarily override the other?',
    'Constitutional amendment (explicit restatement of the ceiling''s authority or explicit abolition) or Supreme Court clarification of Section 4''s supremacy over statutory ceiling. The formal resolution would establish which reading is institutionally binding.',
    'If Section 4 overrides, the ceiling is void and this extraction reading becomes historical artifact. If the ceiling is reaffirmed, Section 4 interpretation must be narrowed to accommodate statutory ceilings, and the extraction reading persists with constitutional cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_commitment_conflict, conceptual, 'Whether the ceiling and 14th Amendment Section 4 can coexist or one forecloses the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(stat_tr_t2003, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2011, 0.26).
narrative_ontology:measurement(stat_tr_t2015, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement(stat_tr_t2019, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(stat_tr_t2024, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(stat_be_t2003, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2003, 0.48).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2011, 0.62).
narrative_ontology:measurement(stat_be_t2015, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(stat_be_t2019, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2019, 0.75).
narrative_ontology:measurement(stat_be_t2024, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1995, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(stat_su_t2003, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2003, 0.51).
narrative_ontology:measurement(stat_su_t2011, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2011, 0.61).
narrative_ontology:measurement(stat_su_t2015, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(stat_su_t2019, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2019, 0.69).
narrative_ontology:measurement(stat_su_t2024, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__extraction_snare_reading, 0.1).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, credit_market_repricing_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, federal_payment_disruption_veto).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling is a contested kernel instantiated by three structurally distinct constraint stories. EXTRACTION_SNARE_READING (this story) frames the ceiling as a weaponized hostage mechanism; COORDINATION_SCAFFOLD_READING frames it as a procedural check on Treasury; CONSTITUTIONAL_NULLITY_READING frames it as unconstitutional under 14th Amendment Section 4. The three readings differ fundamentally on whether the ceiling is legitimate and what function it serves. No single reading is 'the truth' — the kernel is genuinely contested, and the three readings coexist as live positions held by different political factions. The engine's role is to measure each reading's structural characteristics (extractiveness, suppression, etc.) and compute per-seat classifications. This story measures the extraction reading; the sibling stories measure the coordinate readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statutory_debt_ceiling__extraction_snare_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
