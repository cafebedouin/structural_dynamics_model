% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: GDP Growth-Rate Legitimacy Constraint (Quantitative Growth Reading)
 *   domain: political economy/development planning/state capitalism
 *
 * SUMMARY:
 *   A state-capitalist development regime grounds its claim to office in an
 *   annually demonstrated GDP growth rate. The number is set at the center,
 *   assigned downward as binding targets through a cadre-evaluation
 *   hierarchy, financed by channeling household savings into state-directed
 *   investment, and verified by a statistical apparatus the evaluated
 *   hierarchy partly staffs. The arrangement genuinely coordinates a
 *   continent-scale planning system — it mobilized mass employment,
 *   urbanization, and poverty reduction at a speed fragmented decision-making
 *   could not have achieved — and it simultaneously transfers income from
 *   household savers and wage earners to a subsidized industrial-export
 *   complex while accumulating local debt and industrial overcapacity as
 *   tolerated costs. This file instantiates ONE reading of the
 *   performance_legitimacy kernel — the quantitative growth reading — and
 *   authors epsilon for the standing growth-target arrangement as that
 *   reading assesses it; the livelihood-security, qualitative-development,
 *   and techno-nationalist readings are separate constraints in the same
 *   family, linked via network.affects_constraints, each with its own
 *   epsilon, beneficiary structure, and classification. The regime's
 *   self-account presents the target system as proof of competent
 *   coordination; the authored metrics describe substantially extractive,
 *   actively enforced operation. Claim and metrics are independent authored
 *   facts; the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - party_state_leadership: agenda-setter and principal beneficiary (institutional/identity_locked) — sets the targets, controls credit and statistics, collects the legitimacy payoff; its self-concept is fused with delivery
 *   - industrial_export_complex: primary material beneficiary (powerful/arbitrage) — receives directed credit, administratively priced land, and demand guarantees justified by the target
 *   - gdp_measured_local_officials: enforcing beneficiary (organized/identity_locked) — administers targets locally, assembles the investment packages that produce the number, careers ride on delivery
 *   - household_savers_and_wage_earners: primary target (powerless/trapped) — finance the model through capped deposit returns and a suppressed wage share; receive employment in return; no legal or collective exit
 *   - small_private_firms: secondary target (moderate/constrained) — rationed out of credit by state-sector priority, absorb subsidized competition
 *   - independent_economists: excluded voice (moderate/constrained) — question the statistical record and the debt behind it from outside the target-setting conversation
 *   - multilateral_lending_institutions: analytical observer (institutional/analytical) — publish independent growth and debt assessments that feed the model's financing conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.66).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.58).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "GDP Growth-Rate Legitimacy Constraint (Quantitative Growth Reading)").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political economy/development planning/state capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, 'a6100979-4aa6-49b8-8892-b319dcc91806').
narrative_ontology:cs_kernel_codification('a6100979-4aa6-49b8-8892-b319dcc91806', formalized).
narrative_ontology:cs_authority_grounding('a6100979-4aa6-49b8-8892-b319dcc91806', extraction).
narrative_ontology:cs_interpretation_layer_present('a6100979-4aa6-49b8-8892-b319dcc91806').
narrative_ontology:cs_reading_relation('a6100979-4aa6-49b8-8892-b319dcc91806', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6100979-4aa6-49b8-8892-b319dcc91806', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('a6100979-4aa6-49b8-8892-b319dcc91806', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('a6100979-4aa6-49b8-8892-b319dcc91806', foundational, demonstrated_growth_performance_justifies_rule).
narrative_ontology:cs_axiom_status(demonstrated_growth_performance_justifies_rule, holdable).
narrative_ontology:cs_axiom_grounding('a6100979-4aa6-49b8-8892-b319dcc91806', demonstrated_growth_performance_justifies_rule, instrumental).
narrative_ontology:cs_axiom('a6100979-4aa6-49b8-8892-b319dcc91806', secondary, employment_elasticity_of_growth_holds).
narrative_ontology:cs_axiom_status(employment_elasticity_of_growth_holds, holdable).
narrative_ontology:cs_axiom_grounding('a6100979-4aa6-49b8-8892-b319dcc91806', employment_elasticity_of_growth_holds, empirically_contingent).
narrative_ontology:cs_reference_frame('a6100979-4aa6-49b8-8892-b319dcc91806', quantitative_target_adherence).
narrative_ontology:cs_drift_state('a6100979-4aa6-49b8-8892-b319dcc91806', post_convergence_slowdown, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a6100979-4aa6-49b8-8892-b319dcc91806', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, party_state_leadership).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, household_savers_and_wage_earners).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, small_private_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, household_savers_and_wage_earners).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, investment_led_convergence_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, growth_employment_linkage_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the annual and five-year growth targets, assigns them down the administrative hierarchy, and claims governing competence from their attainment. The growth record is the organization's public proof of performance; abandoning it would require a different account of why it holds office. Its identity and its claim to rule are fused with delivery of the number, so stepping away from the target system is not a policy option but a self-redefinition. It controls the credit system, the cadre evaluation apparatus, and the statistical hierarchy that verifies results.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, party_state_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, party_state_leadership, beneficiary).

% State-owned industrial groups, export manufacturers, and the construction complex receive directed credit at below-market rates, land allocated at administrative prices, and demand guaranteed by the investment programs the growth target justifies. Earnings and balance sheets are built on this channel. Individual firms can relocate production abroad or shift sectors, and some do, but the complex as a whole exists because the target system keeps credit and demand flowing toward it.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    powerful, biographical, arbitrage, global).

% Provincial and municipal officials receive assigned growth targets that determine promotion, standing, and career survival inside the cadre system. They assemble the local investment packages — land sales, platform-company borrowing, project approvals — that produce the number, and report the result upward through a statistical hierarchy they partly staff. A career spent inside the target system leaves few outside options: missing targets ends advancement, and leaving the system forfeits what the career accumulated.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials, agenda_setter,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials, beneficiary).

% Households supply the model's financing: deposit rates are held below market, capital controls keep savings inside the state banking channel, and the wage share of national income stays low while output is directed to investment rather than consumption. In exchange they receive the employment the growth model creates and the urban incomes that came with it. Exit is largely unavailable: savings cannot legally leave the system, and there is no electoral or collective channel through which the household share of income can be renegotiated.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, household_savers_and_wage_earners, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, household_savers_and_wage_earners, beneficiary).

% Private firms outside the state channel compete for credit that is rationed toward the target-bearing state sector, pay higher effective financing costs, and absorb competition from subsidized incumbents in their own markets. They employ a large share of the workforce, which gives them a stake in the growth model's employment promise, but they have no seat in target-setting and limited recourse when credit tightens.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, small_private_firms, payer,
    moderate, biographical, constrained, national).

% Academic and market economists who question the growth statistics, the debt accumulated behind the targets, or the model's diminishing returns publish at the margin of tolerance and are not part of the target-setting process. Some work inside official think tanks where candor has career limits; others publish abroad. Their measurement work is used by outside observers but does not enter the evaluation system that produces the official number.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, independent_economists, excluded,
    moderate, biographical, constrained, national).

% International financial institutions and rating agencies track the growth record, publish independent estimates, and assess the debt built to sustain the targets. They have no role in setting or verifying the domestic targets, but their assessments feed investor decisions and cross-border borrowing costs, giving them indirect leverage over the model's financing conditions.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, multilateral_lending_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes a continent-scale planning hierarchy — ministries, provinces, state banks, state firms — on a single quantitative objective, mobilizing household savings into investment at a scale and speed fragmented decision-making could not achieve, and delivering mass employment during the shift from agrarian to industrial urban economy.
% TRANSFER_FUNCTION: Moves financial resources from household savers (via administratively capped deposit returns and capital controls) and from labor income (via a suppressed wage share) into state-directed investment; moves promotion and political standing to officials who deliver measured growth; moves an annual public demonstration of competence to the political center.
% ABSENT_VOICES: Independent economists who question the statistical record and the debt behind it, and the households whose savings finance the model, have no seat where targets are set; the number is produced inside the hierarchy that is evaluated by it.
% DISAPPEARANCE_RATIONALE: If the target system and its legitimacy claim vanished overnight, cadre evaluation, credit allocation, land finance, and the investment pipeline would lose their coordinating objective; the industrial-export complex would lose its demand guarantee; the political center would need an entirely different account of its competence — which is precisely the contest the sibling readings occupy.
% FOUNDING_PROBLEM: After the collapse of ideological legitimacy, a poor agrarian economy with mass underemployment needed rapid industrialization, job creation, and a demonstrable, verifiable account of governing competence; the growth target was built to serve all three at once.
% FOUNDING_PROBLEM_CORROBORATION: Multilateral poverty-reduction accounting and independent demographic and productivity analyses corroborate that the founding problem has been transformed: extreme poverty fell dramatically while the investment model's measured returns declined. Independent economists outside the beneficiary set attest the statistical record's inflation and the debt overhang. No source outside the benefiting parties attests that the original rate commitment remains necessary in its original form; the beneficiary set itself attests that it does.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 because the delivery mechanism is a transfer: deposit rates held below market, capital controls that keep savings in the state channel, a wage share held down while output is directed to investment, and a rising volume of debt and overcapacity tolerated to keep the rate up. The series rises over the interval because each additional point of growth costs more transfer and more debt as returns to investment diminish. Suppression is authored at 0.58 as a raw structural property — it is NOT scaled by power or scope in this story; only extraction is scaled, in the engine's computation. It runs through credit allocation, cadre discipline, capital controls, and statistical control rather than direct coercion of households; it hardened through the 2010s as targets hardened and eased slightly at the end as the center de-emphasized the hard headline number in official reports while the credit and evaluation machinery persisted. Theater is authored at 0.45: statistical embellishment, target-satisficing projects, and performative groundbreaking are real and growing, but roughly half of measured activity remains genuine output. Accessibility collapse is 0.55 — alternatives are partly collapsed (households cannot exit the savings channel; officials cannot leave the target track) but not fully, because rival legitimacy accounts survive as live sibling readings. Resistance is 0.45: local number-gaming, quiet target-missing, attempted capital flight, and published skepticism at the margin of tolerance. All three series are authored on one shared time grid (2001, 2005, 2009, 2013, 2017, 2021, 2025) so no metric's end state is silently substituted onto earlier points.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute differently. From the industrial-export complex's seat the target system is the coordination mechanism that built its markets; from the household seat the same structure is a transfer machine with no exit. Local officials occupy both positions: the target is the ladder they climb and the pressure that disciplines them, which is why their exit is identity-locked — a career inside the target system leaves no outside self. The leadership's lock is institutional identity: the organization has become its delivery function, and abandoning the number would require re-founding its claim to office on a sibling reading. If the identity frame broke at either level — officials revalued on non-growth metrics, or the leadership re-founding legitimacy on livelihood or quality delivery — the constraint's enforcement would lose its carriers and its classification would move. Coalition potential among powerless households is structurally suppressed: capital controls prevent exit and no collective channel exists through which the household income share can be renegotiated, which is what holds their seat near the full-target end rather than enabling bargaining.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations put the leadership, the export complex, and GDP-measured officials near the beneficiary end (d well below 0.5): the arrangement subsidizes all three. The complex's arbitrage-grade exit (relocatable production, foreign earnings) places it nearest the pure-beneficiary end. Officials derive low d from their beneficiary declaration but not the minimum, because they also bear delivery pressure and identity-locked exit removes the softening that mobility would give. Household savers and small private firms are declared victims; trapped exit (capital controls, no collective channel) pushes households toward the full-target end, constrained exit leaves firms slightly short of it. The continental scope of the target system makes verification harder, which the engine reflects as a modest amplification of effective extraction at every seat; the suppression scalar itself is left unscaled by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — subsistence poverty, mass underemployment, and a post-ideological need for demonstrable competence — has been substantially transformed: extreme poverty fell, the employment structure shifted, and the economy reached middle-income status, while the investment model's measured returns declined and its debt and overcapacity costs accumulated. The arrangement persists at full enforcement. The founding-problem status is authored 'contested' rather than 'dead' because the parties genuinely dispute whether mass employment and convergence still require the rate commitment; the mismatch consumer should read that contest against the theater trajectory, which has more than doubled (0.20 to 0.45) as the gap between the delivered number and the underlying economy widened. If the rate target persists after returns are exhausted, the constraint decays toward theatrical maintenance of a number — the piton signature — with the industrial-export complex as the residual seat the transfers still reach. Mandatrophy is not declared resolved: the reading contest (see the legitimacy_substitution_feasibility and enforced_reading omegas) is exactly the open question that decides whether the mandate is outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforced_reading_of_performance_kernel,
    'Which reading of the performance_legitimacy kernel does the enforcement machinery actually instantiate — the declared quantitative growth target, or an operating blend in which strategic-industry and livelihood metrics already share cadre evaluation and credit allocation?',
    'Inspect what the evaluation system actually rewards across successive plan cycles: which targets carry career consequences, where directed credit flows when the headline target and strategic-industry goals conflict, and which numbers careers are made or lost on.',
    'If enforcement has already blended, this constraint''s machinery is being inherited by sibling readings and the quantitative reading drifts toward theatrical maintenance of a number no longer binding; if the rate still binds alone, the tangled-rope classification holds with this reading as the operative constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforced_reading_of_performance_kernel, conceptual, 'Which sibling reading the enforcement machinery actually serves.').

omega_variable(
    statistical_record_integrity,
    'How much of the reported growth record is real output, and how much is statistical embellishment produced by the evaluated hierarchy that reports it?',
    'Independent cross-checks of reported growth against physical series — nighttime luminosity, electricity consumption, rail freight, tax receipts — reconciled across provinces and over time.',
    'A large persistent overstatement raises the theater ratio above the authored 0.45 and shifts the classification toward inertial theatrical maintenance; a small one supports the tangled-rope reading with mostly real coordination underneath.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statistical_record_integrity, empirical, 'Real-versus-reported growth gap.').

omega_variable(
    financial_repression_transfer_magnitude,
    'How large is the annual transfer from household savers to the state investment channel produced by administratively suppressed deposit rates and capital controls?',
    'Estimate the spread between administered deposit rates and a market-clearing rate multiplied by household deposits, plus the wage-share gap against comparable economies at the same income level.',
    'A large transfer confirms the asymmetric-extraction half of the tangled-rope structure and raises effective extraction; a small one would move the arrangement toward a coordination-dominant reading in which households are net beneficiaries of the employment it delivered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_repression_transfer_magnitude, empirical, 'Size of the household-to-investment transfer channel.').

omega_variable(
    legitimacy_substitution_feasibility,
    'Can the political center substitute a sibling reading''s legitimacy basis (livelihood delivery, development quality, strategic capability) without dismantling the investment model the growth-target machinery enforces?',
    'Observe whether evaluation criteria, credit directives, and statistical attention actually re-weight when official rhetoric shifts toward quality or livelihood metrics, and whether the investment pipeline contracts or merely re-labels.',
    'If substitution is feasible, the target system can be renegotiated rather than decayed — it becomes transitional support for a legitimacy handover; if not, the machinery persists theatrically while the founding problem stays contested, and the decay path runs toward inertial maintenance with the export complex as residual receipt seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_substitution_feasibility, conceptual, 'Whether the legitimacy basis can be swapped without dismantling the enforcement machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 2001, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t2001, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement_basis(perf_tr_t2001, observed).
narrative_ontology:measurement(perf_tr_t2005, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement_basis(perf_tr_t2005, observed).
narrative_ontology:measurement(perf_tr_t2009, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2009, 0.32).
narrative_ontology:measurement_basis(perf_tr_t2009, observed).
narrative_ontology:measurement(perf_tr_t2013, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement_basis(perf_tr_t2013, observed).
narrative_ontology:measurement(perf_tr_t2017, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2017, 0.42).
narrative_ontology:measurement_basis(perf_tr_t2017, observed).
narrative_ontology:measurement(perf_tr_t2021, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2021, 0.44).
narrative_ontology:measurement_basis(perf_tr_t2021, observed).
narrative_ontology:measurement(perf_tr_t2025, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(perf_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(perf_be_t2001, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2001, 0.48).
narrative_ontology:measurement_basis(perf_be_t2001, observed).
narrative_ontology:measurement(perf_be_t2005, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement_basis(perf_be_t2005, observed).
narrative_ontology:measurement(perf_be_t2009, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2009, 0.56).
narrative_ontology:measurement_basis(perf_be_t2009, observed).
narrative_ontology:measurement(perf_be_t2013, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2013, 0.6).
narrative_ontology:measurement_basis(perf_be_t2013, observed).
narrative_ontology:measurement(perf_be_t2017, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2017, 0.63).
narrative_ontology:measurement_basis(perf_be_t2017, observed).
narrative_ontology:measurement(perf_be_t2021, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2021, 0.65).
narrative_ontology:measurement_basis(perf_be_t2021, observed).
narrative_ontology:measurement(perf_be_t2025, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(perf_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t2001, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement_basis(perf_su_t2001, observed).
narrative_ontology:measurement(perf_su_t2005, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2005, 0.47).
narrative_ontology:measurement_basis(perf_su_t2005, observed).
narrative_ontology:measurement(perf_su_t2009, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2009, 0.53).
narrative_ontology:measurement_basis(perf_su_t2009, observed).
narrative_ontology:measurement(perf_su_t2013, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2013, 0.58).
narrative_ontology:measurement_basis(perf_su_t2013, observed).
narrative_ontology:measurement(perf_su_t2017, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2017, 0.61).
narrative_ontology:measurement_basis(perf_su_t2017, observed).
narrative_ontology:measurement(perf_su_t2021, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement_basis(perf_su_t2021, observed).
narrative_ontology:measurement(perf_su_t2025, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(perf_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'performance legitimacy' covers four structurally distinct legitimacy arrangements sharing one kernel (rule justified by delivered performance) but instantiating different constraints with different epsilon, beneficiary sets, and enforcement machinery. This file instantiates the quantitative growth reading: the headline rate is the binding metric, the investment-export model is the delivery mechanism, and the industrial-export complex plus GDP-measured officials are the beneficiary set. The sibling files instantiate the livelihood-security, qualitative-development, and techno-nationalist readings. The family is linked because the readings compete for the same enforcement machinery (cadre evaluation, credit allocation) and the same legitimacy payoff; this reading's machinery structurally pressures the qualitative reading's resource base, which is why the influences edge runs from this file to the qualitative sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
