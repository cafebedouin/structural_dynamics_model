% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling as Constitutionally Void Constraint (14th Amendment Section 4 Supersession Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story instantiates the constitutional-nullity reading of the debt
 *   ceiling kernel: the claim that the statutory debt ceiling, whatever its
 *   historical administrative purpose, is superseded whenever it conflicts
 *   with the 14th Amendment's Section 4 command that 'the validity of the
 *   public debt of the United States, authorized by law... shall not be
 *   questioned.' On this reading, once Congress has passed appropriations and
 *   revenue statutes that jointly require borrowing beyond the statutory
 *   ceiling, the ceiling is not a binding legal constraint at all — it is a
 *   piece of ceremonial theater layered atop obligations Congress already
 *   created and constitutionally guaranteed. ε is authored near zero because,
 *   from this reading's own lights, the ceiling has no real operative legal
 *   force on the underlying arrangement (Treasury's obligation to pay) even
 *   though enormous political and administrative effort is spent behaving as
 *   though it does.
 *
 * KEY AGENTS:
 *   - treasury_department: administers extraordinary measures around a constraint this reading holds is void
 *   - congress: stages a ceremonial vote over an already-settled fiscal commitment
 *   - congressional_leadership_of_both_parties: extracts political theater value from the ceremony
 *   - bondholders_and_capital_markets: structurally protected by Section 4 but exposed to brinkmanship-driven volatility
 *   - federal_beneficiaries_and_contractors: bear real anxiety and occasional disruption despite the reading's claim that no genuine legal jeopardy exists
 *   - supreme_court: has never resolved the question this reading treats as settled
 *   - constitutional_law_scholars: analytical seat articulating and defending the nullity argument
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.03).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.15).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.88).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling as Constitutionally Void Constraint (14th Amendment Section 4 Supersession Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'e020f28b-20d3-40ee-9fc0-508a5613e8eb').
narrative_ontology:cs_kernel_codification('e020f28b-20d3-40ee-9fc0-508a5613e8eb', fixed_text).
narrative_ontology:cs_authority_grounding('e020f28b-20d3-40ee-9fc0-508a5613e8eb', practice).
narrative_ontology:cs_interpretation_layer_present('e020f28b-20d3-40ee-9fc0-508a5613e8eb').
narrative_ontology:cs_reading_relation('e020f28b-20d3-40ee-9fc0-508a5613e8eb', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('e020f28b-20d3-40ee-9fc0-508a5613e8eb', statutory_debt_ceiling__extraction_snare_reading, influences).
narrative_ontology:cs_axiom('e020f28b-20d3-40ee-9fc0-508a5613e8eb', foundational, public_debt_validity_clause_self_executing).
narrative_ontology:cs_axiom_status(public_debt_validity_clause_self_executing, holdable).
narrative_ontology:cs_axiom_grounding('e020f28b-20d3-40ee-9fc0-508a5613e8eb', public_debt_validity_clause_self_executing, conventional).
narrative_ontology:cs_axiom('e020f28b-20d3-40ee-9fc0-508a5613e8eb', secondary, prior_appropriations_constitute_authorized_debt_under_amendment).
narrative_ontology:cs_axiom_status(prior_appropriations_constitute_authorized_debt_under_amendment, holdable).
narrative_ontology:cs_axiom_grounding('e020f28b-20d3-40ee-9fc0-508a5613e8eb', prior_appropriations_constitute_authorized_debt_under_amendment, conventional).
narrative_ontology:cs_reference_frame('e020f28b-20d3-40ee-9fc0-508a5613e8eb', public_debt_validity_clause_self_execution).
narrative_ontology:cs_drift_state('e020f28b-20d3-40ee-9fc0-508a5613e8eb', post_2011_2013_2023_standoff_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e020f28b-20d3-40ee-9fc0-508a5613e8eb', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership_of_both_parties).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, debt_ceiling_brinkmanship_political_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, bondholders_and_capital_markets).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, bondholders_and_capital_markets).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, federal_beneficiaries_and_contractors).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, public_debt_validity_clause_supremacy).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, self_executing_constitutional_supersession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Executes appropriations Congress has already enacted, which necessarily require borrowing to fund; under this reading the statutory ceiling has no binding legal force once it collides with the Section 4 validity guarantee, yet Treasury continues to observe the ceiling procedurally out of institutional caution and deference to Congress, spending enormous administrative effort on 'extraordinary measures' to avoid ever testing the question in court.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, agenda_setter,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, payer).

% Holds periodic votes to raise or suspend the statutory ceiling that, under this reading, are legally superfluous ceremony rather than an exercise of a real gate — the borrowing has already been authorized by the appropriations and revenue statutes Congress itself passed. Congress nonetheless retains enormous political theater value from staging the vote.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Uses the recurring ceiling vote as leverage theater and fundraising/messaging opportunity regardless of which party holds the majority; benefits from the vote's persistence as a stage for demonstrating fiscal seriousness or extracting concessions, even though under this reading the vote adjudicates nothing legally binding.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership_of_both_parties, beneficiary,
    organized, biographical, arbitrage, national).

% Rely on the absolute constitutional certainty that U.S. obligations will be honored; under this reading they are structurally shielded because Section 4 forecloses default as a legal possibility regardless of what happens to the statutory ceiling. Periodically bear brief volatility and hedging costs when brinkmanship theater makes markets doubt whether officials will act on the nullity, even though the nullity itself is not in genuine legal doubt under this reading.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, bondholders_and_capital_markets, beneficiary,
    organized, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, bondholders_and_capital_markets, payer).

% Social Security recipients, federal contractors, and government employees who face genuine anxiety and occasionally real payment disruption during ceiling standoffs, even though under this reading the disruption reflects institutional unwillingness to act on a constitutional nullity rather than any genuine legal constraint on Treasury's authority to keep paying them.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_beneficiaries_and_contractors, payer,
    powerless, immediate, trapped, national).

% Has never definitively ruled on whether the statutory ceiling is void under Section 4 (Perry v. United States dicta is the closest precedent but did not resolve a live ceiling controversy); the constitutional question this reading asserts as settled has never actually been adjudicated because no party has had standing or incentive to force a ruling before each crisis is resolved politically.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, supreme_court, excluded,
    institutional, generational, analytical, national).

% Analyze the Section 4 text ('the validity of the public debt... shall not be questioned') and its drafting history; a substantial and growing body of scholarship holds the ceiling legally void when it conflicts with prior appropriations, though this remains a minority-turned-more-prominent position rather than settled doctrine.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership_of_both_parties).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading there is no genuine coordination function left to perform — Congress already coordinates spending and revenue through the appropriations and tax statutes it enacts; the separate ceiling statute adds no additional coordination the underlying fiscal statutes have not already accomplished.
% TRANSFER_FUNCTION: The ceiling vote itself transfers political leverage and media attention to whichever congressional faction stages the brinkmanship, and transfers anxiety/administrative cost onto federal payees and Treasury staff, without moving any resource whose allocation was not already settled by the appropriations Congress passed.
% ABSENT_VOICES: The Supreme Court has never been forced to rule squarely on the constitutional question, so the nullity claim remains authoritative only among the scholars and officials who assert it — no binding judicial voice has confirmed or foreclosed it, and litigants with standing rarely emerge because crises are resolved politically before a case can mature.
% DISAPPEARANCE_RATIONALE: Under this reading, if the statutory ceiling were repealed or formally declared void tomorrow, Treasury's actual obligations and appropriations would not change at all — it would simply stop staging a vote that, in this reading, was never legally load-bearing. The only change would be the disappearance of a recurring political ritual and its associated market anxiety.
% FOUNDING_PROBLEM: The 1917 Second Liberty Bond Act consolidated piecemeal bond-by-bond congressional authorization into a single aggregate ceiling to let Treasury manage WWI financing without seeking a new statute for every debt issuance.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars (e.g., Balkinization commentary, Buchanan and Dorf's extensive Section 4 scholarship) attest from outside the congressional leadership that the original administrative-convenience problem was fully solved decades ago and that subsequent appropriations acts now render the ceiling substantively redundant with, and legally subordinate to, the debts Congress separately authorizes; no institution outside congressional leadership itself attests that the ceiling still performs a live coordination function.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because, under this reading, the constraint does not actually reallocate resources between parties beyond the ceremonial: the underlying spending was already authorized, so no party pays a real price for the ceiling's existence except the administrative overhead of the charade and periodic market anxiety. Theater ratio rises sharply from 1917 (0.1, when the ceiling had a genuine administrative function) to 2025 (0.88) as repeated near-defaults (1995-96, 2011, 2013, 2021, 2023) demonstrate that virtually all activity around the ceiling has become performative brinkmanship rather than functional constraint on spending. Accessibility collapse is authored low-moderate (0.2) because alternatives (the platinum coin, premium bonds, invoking Section 4 directly, or simply repealing the statute) are widely discussed and legally available, not foreclosed — this is precisely why the constraint reads as void rather than as a genuine mountain in the strict sense; it is authored as mountain-claimed here because the reading's core assertion is that the constraint has NO remaining discretionary content once Section 4 is applied, i.e., it is a nullity as a matter of constitutional necessity, not a policy choice. Resistance is authored moderate (0.35): the reading faces real resistance from officials unwilling to test it in court and from a legal establishment that has not endorsed it as settled law.
 *
 * PERSPECTIVAL GAP:
 *   Treasury and Congress experience the ceiling as something they must still navigate procedurally even while (under this reading) understanding it lacks binding force — this is the seat divergence the engine should register: the agenda-setting seats behave as though the constraint binds while the analytical/scholarly seat asserts it does not. Federal beneficiaries and contractors experience real anxiety and occasional harm that is incongruous with a genuinely null constraint, which is itself evidence for the sibling extraction_snare_reading rather than this one — that divergence is precisely why these are separate constraint stories rather than one story with a measurement parameter.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional leadership of both parties is coded as beneficiary because the ceremonial vote generates leverage and messaging value regardless of the constraint's legal nullity; bondholders benefit from the underlying constitutional guarantee but bear volatility costs from the brinkmanship performance built on top of the (per this reading) nonexistent legal jeopardy, hence the secondary payer role. Federal beneficiaries and contractors are pure payers: powerless, trapped, and immediate time horizon, bearing the disruption cost of a political performance staged over a constraint this reading holds has no real teeth. No victim group is declared because this reading's claimed type is mountain (constitutional necessity), and mountain claims are evaluated on the emerges_naturally/beneficiary-declaration axis rather than requiring a victim array; the FSM gate is intentionally triggered here (beneficiaries declared on a mountain) because the political class's incentive to keep staging the ceremony despite its claimed nullity is exactly the kind of constructed-vs-natural ambiguity FSM exists to flag.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (aggregate authorization convenience for WWI-era bond issuance) is coded dead: appropriations acts now perform that coordination function directly and repeatedly, decades after the ceiling's original administrative rationale disappeared. Yet the mandate — the ceremonial vote — persists entirely on inertia and political utility to congressional leadership, which is the classic mandatrophy signature: an arrangement whose stated function has been mooted by subsequent institutional developments (statutory appropriations subordinate to Section 4) but which is kept alive because disappearance would remove a valuable piece of political theater from incumbents in both parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    section_4_self_execution_and_justiciability,
    'Is the Public Debt Clause self-executing in a way that renders the statutory ceiling automatically void without judicial or executive action, or does it merely provide a constitutional argument that requires affirmative invocation (by the President or courts) to have legal effect?',
    'A live Supreme Court ruling squarely addressing a debt-ceiling standoff (rather than Perry v. United States dicta from a different factual posture) would resolve whether Section 4 operates as an automatic nullifying force or as an available-but-unexercised defense.',
    'If self-executing, this reading''s near-zero extractiveness and mountain classification are well-grounded as a matter of settled constitutional structure. If not self-executing, the reading collapses into a policy argument for why officials SHOULD ignore the ceiling, not a description of the ceiling''s actual current legal status — which would push this constraint toward the extraction_snare_reading''s territory whenever officials choose to observe it anyway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_4_self_execution_and_justiciability, conceptual, 'Whether Section 4 automatically nullifies the ceiling or merely supplies an available constitutional defense.').

omega_variable(
    constructed_beneficiary_vs_natural_necessity,
    'Is the persistence of the ceremonial ceiling-vote ritual (despite its claimed constitutional nullity) evidence that the underlying constraint is a genuine constitutional necessity dressed in political theater, or evidence that the ''nullity'' framing is itself a constructed reading that benefits incumbents by preserving a valuable leverage-and-messaging device?',
    'Track whether congressional leadership of either party takes any concrete step toward formally repealing the ceiling statute or codifying Section 4 supersession when they hold unified government and face no political cost from removing the ritual; sustained refusal to do so despite the opportunity would support the constructed-beneficiary reading.',
    'If leadership never removes the ritual when costless to do so, that is strong evidence the beneficiary declaration (congressional_leadership_of_both_parties) correctly identifies a maintained, constructed constraint rather than a pure natural-law nullity — reinforcing the FSM flag already triggered on this mountain-claimed story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_beneficiary_vs_natural_necessity, empirical, 'Whether the ceremonial vote''s persistence reflects genuine constitutional structure or a constructed political benefit.').

omega_variable(
    reading_framing_underdetermination,
    'Given that the same statutory and constitutional text supports three structurally distinct readings (nullity, scaffold, snare) with orders-of-magnitude different ε values, which framing should be treated as the default description of ''the debt ceiling'' in public and scholarly discourse absent a controlling judicial resolution?',
    'This is a conceptual framing question rather than an empirical one; it would be partially resolved by a definitive judicial ruling (see section_4_self_execution_and_justiciability) but would remain partly a matter of interpretive commitment even after such a ruling, since courts could still leave room for competing characterizations of intervening episodes.',
    'Choosing this reading as authoritative substantially understates the real disruption experienced by federal_beneficiaries_and_contractors and bondholders during standoffs (better captured by extraction_snare_reading); choosing the snare reading as authoritative would overstate the ceiling''s legal bindingness against the weight of the constitutional text this reading relies on. The three-file decomposition is the intended resolution: no single file should be read as ''the'' debt ceiling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Which of the three sibling readings should anchor public discourse absent judicial resolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(stat_tr_t1979, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1979, 0.35).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1995, 0.55).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2011, 0.75).
narrative_ontology:measurement(stat_tr_t2013, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2013, 0.8).
narrative_ontology:measurement(stat_tr_t2023, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2023, 0.85).
narrative_ontology:measurement(stat_tr_t2025, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2025, 0.88).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1917, 0.02).
narrative_ontology:measurement(stat_be_t1979, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1979, 0.02).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1995, 0.03).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2011, 0.03).
narrative_ontology:measurement(stat_be_t2013, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2013, 0.03).
narrative_ontology:measurement(stat_be_t2023, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2023, 0.03).
narrative_ontology:measurement(stat_be_t2025, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2025, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__constitutional_nullity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__constitutional_nullity_reading, 0.05).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, extraction_snare_reading).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the natural-language concept 'the debt ceiling' per the ε-invariance principle: constitutional_nullity_reading (this file, ε≈0.03, claimed mountain), coordination_scaffold_reading (moderate ε, claimed scaffold or rope), and extraction_snare_reading (high ε, claimed snare). The three share statutory text and history but diverge on legal operativeness, coordination function, and beneficiary/victim structure. Each carries its own ε and stakeholder set; the label 'debt ceiling' is disambiguated into these three structurally distinct claims rather than averaged into one story with a hidden measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
