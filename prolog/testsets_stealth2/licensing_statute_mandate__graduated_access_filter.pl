% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Gate: Graduated Market Access Filter
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   Across dozens of occupations — from medicine and electrical work to
 *   cosmetology, auctioneering, and interior design — statute makes a
 *   state-issued credential a precondition of lawful practice. Entry requires
 *   paid clock-hours, supervised experience, examinations, and recurring
 *   fees; for many aspirants the binding obstacle is the resource bill, which
 *   can exceed a year of income for a low-wage worker, rather than any
 *   demonstrated incompetence. Enforcement boards prosecute unlicensed
 *   practice, closing the informal lane that historically absorbed learners.
 *   The result is a legally maintained tier line in labor markets: above it,
 *   a credentialed class holding protected market share and a wage premium;
 *   below it, a population sorted into debt-financed attempts, informal work
 *   under legal risk, or exit from the trade altogether — with the sorting
 *   tracking prior wealth and family resources more tightly than it tracks
 *   skill. This story authors that access-incidence arrangement as its single
 *   subject; the claimed type is stated from the generating seat and the
 *   metrics are authored independently as descriptive of observed operation.
 *
 * KEY AGENTS:
 *   - - credentialed_incumbents: Primary beneficiary (organized/arbitrage) — collects the access premium behind the statutory tier line
 *   - - accredited_training_institutions: Secondary beneficiary (institutional/mobile) — sells the statutorily mandated credential path
 *   - - licensing_board_administrators: Agenda-setter with fee-funded benefit (institutional/constrained) — writes rules, runs exams, prosecutes unlicensed practice
 *   - - state_legislatures: Agenda-setter (institutional/mobile) — enacts and amends the practice acts, lobbied asymmetrically
 *   - - uncredentialed_low_income_applicants: Primary target (powerless/constrained) — bears the full resource barrier
 *   - - indebted_training_dropouts: Target (powerless/trapped) — carries debt without the license that was to service it
 *   - - gray_market_practitioners: Target (moderate/constrained) — practices informally under cease-and-desist and fine risk
 *   - - would_be_job_learners: Excluded voice (powerless/constrained) — the apprenticeship rungs the minimums phased out; absent from every docket
 *   - - competition_policy_researchers: Analytical observer (analytical/analytical) — measures the filter's incidence on wages, supply, and entry composition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.74).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.8).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.74).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Gate: Graduated Market Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, 'c7ad4ed5-e31c-46b9-9332-24c38aeb60d0').
narrative_ontology:cs_kernel_codification('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', formalized).
narrative_ontology:cs_authority_grounding('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', expertise).
narrative_ontology:cs_interpretation_layer_present('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0').
narrative_ontology:cs_reading_relation('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', foundational, market_access_sorts_by_prior_resources).
narrative_ontology:cs_axiom_status(market_access_sorts_by_prior_resources, holdable).
narrative_ontology:cs_axiom_grounding('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', market_access_sorts_by_prior_resources, empirically_contingent).
narrative_ontology:cs_axiom('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', secondary, resource_neutral_competence_gates_required).
narrative_ontology:cs_axiom_status(resource_neutral_competence_gates_required, holdable).
narrative_ontology:cs_axiom_grounding('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', resource_neutral_competence_gates_required, deontological).
narrative_ontology:cs_reference_frame('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', tiered_statutory_market_access).
narrative_ontology:cs_drift_state('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', contemporary_reform_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('c7ad4ed5-e31c-46b9-9332-24c38aeb60d0', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_incumbents).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, accredited_training_institutions).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, uncredentialed_low_income_applicants).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, indebted_training_dropouts).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, gray_market_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the license the statute requires and staff the boards and association committees that propose hour and exam requirements. Their services face less price competition because new entrants must clear the same costly path they already cleared. Many contribute dues and campaign support to defend current thresholds; movement between states is eased by reciprocity compacts their associations negotiated.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_incumbents, beneficiary,
    organized, biographical, arbitrage, national).

% Sell the clock-hours, coursework, and exam preparation the statute makes mandatory. Enrollment is secured by law rather than by demand for their product, and program lengths tend to grow when their graduates sit on the boards that set requirements. If the statute shrank, they would need to compete for students on price and outcomes.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, accredited_training_institutions, beneficiary,
    institutional, generational, mobile, national).

% Write administrative rules, schedule and grade examinations, collect fees, and bring disciplinary actions against unlicensed practice. Board operations are funded from licensee and applicant fees, and board seats are commonly held by current licensees. Their professional standing consists in running the credentialing process.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators, beneficiary).

% Enact and amend the practice acts that define which occupations require a license and what the requirements are. They hear predominantly from incumbent professional associations, which are organized and persistent, while affected outsiders rarely appear. Revisiting thresholds usually requires a scandal, a lawsuit, or a sunset-review deadline.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, state_legislatures, agenda_setter,
    institutional, biographical, mobile, national).

% Want to earn a living in a licensed occupation but face tuition, months of unpaid supervised hours, exam fees, and preparation costs that can total more than a year of income for a low-wage worker. The options are paying the full path, practicing without a license and risking fines, switching to an unlicensed trade, or leaving the field. They hold no seat in the rulemaking process.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, uncredentialed_low_income_applicants, payer,
    powerless, immediate, constrained, regional).

% Borrowed to complete required coursework, then failed the examination or ran out of money before finishing supervised hours. They carry student debt without the license that was supposed to service it, and the sunk spending keeps them retaking exams or working in adjacent unlicensed roles rather than starting over elsewhere.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, indebted_training_dropouts, payer,
    powerless, biographical, trapped, regional).

% Serve clients informally without a license, often in neighborhoods where licensed providers are scarce or unaffordable. Income is real but precarious: boards send cease-and-desist letters, impose fines, and occasionally refer cases for prosecution. Some have organized and litigated — hair braiders and African-style natural hair care workers won statutory exemptions in several states.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, gray_market_practitioners, payer,
    moderate, immediate, constrained, local).

% Would once have entered through paid apprenticeship or junior positions and learned on the job; statutory minimums phased out most of those rungs in covered trades. They never enter the pipeline at all, so they appear in no docket, pay no fee, and are counted nowhere except as evidence of reduced labor supply.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, would_be_job_learners, excluded,
    powerless, biographical, constrained, national).

% Estimate how credential mandates affect wages, employment, prices, and who manages to enter, using census, survey, and natural-experiment data across states and decades. Their findings circulate in journals and legislative testimony but carry no vote on any board.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, competition_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, credentialed_incumbents).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes a verifiable, portable signal of documented training before market entry, addresses the consumer's inability to evaluate competence before purchase, and gives administrators a single administrable criterion for who may practice.
% TRANSFER_FUNCTION: Moves market-access rights — and the income premium attached to them — from uncredentialed entrants to current licensees, and moves tuition, exam, and renewal fees from aspirants and licensees to training institutions and board budgets.
% ABSENT_VOICES: Would-be apprentices and informal practitioners — the people the minimums exclude — are absent from rulemaking dockets, which are dominated by incumbent associations; low-income communities bearing the incidence have no organized seat. Their objections surface only indirectly, through litigation and academic studies.
% DISAPPEARANCE_RATIONALE: Overnight repeal would open covered trades to direct entry: labor supply in licensed occupations would rise, wages and prices in those trades would compress toward competitive levels, training institutions would lose captive enrollment, board revenues would collapse, and a share of informal practice would formalize — the occupational structure of entire regions would reorganize around open entry.
% FOUNDING_PROBLEM: In the trades first covered, consumers could not judge competence before purchase and demonstrable harms occurred — botched medical treatment, unsafe wiring, adulterated goods — so states mandated documented minimum training as a condition of practice.
% FOUNDING_PROBLEM_CORROBORATION: Historical public-health records attest the original harm problem for medicine and a handful of skilled trades; independent labor-economic studies and state sunset-review commissions find little or no comparable harm evidence for most currently covered occupations, while incumbent associations attest the problem remains live everywhere. Corroboration for the shifted status comes from outside the benefiting parties, and the dispute between those sources is itself the finding.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74) because the access premium transferred to licensees is large, persistent, and decoupled from marginal competence value across most covered occupations. Suppression is higher (0.80) because persistence depends on active prosecution of unlicensed practice and statutorily closed entry lanes, not on participant preference. Theater is moderate (0.34): examinations and hour rules do perform screening, but a growing share of enforcement activity defends the boundary itself rather than evaluating competence. Accessibility collapse is moderate (0.48): informal practice, adjacent occupations, and interstate variation keep alternatives partly alive, though the licensed lane is wholly closed to the uncredentialed. Resistance is substantial (0.62): litigation won exemptions for braiders and natural-hair practitioners, universal-recognition statutes spread, and a sustained research literature contests the arrangement. Coordination type is declared identity_coordination because the arrangement's primary coordination function is boundary maintenance — deciding who counts as a practitioner — which is exactly the function whose complexity the floor accommodates and whose cover-story risk the coupling test watches. The three measurement series share one seven-point grid across the sixty-unit interval; the trajectories are monotonic ratchets rather than cycles — enforcement capacity visibly hardened (rising suppression_requirement: expanded board staffing, sting operations, fine schedules) as coverage widened, so the series is authored deliberately rather than left to defaults.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent and board seats the statute reads as quality infrastructure they personally maintain, staff, and fund; from the applicant, dropout, and gray-market seats the same statute reads as a priced wall they are charged to climb or fined for circumventing. Identical rules, opposite experienced classifications — the divergence is computed per seat from power, exit, and declared position, not asserted by this story.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidized end: incumbents collect the access premium with arbitrage-grade mobility across reciprocal states, and training institutions collect enrollment guaranteed by law. Declared payers sit near the target end: applicants and dropouts bear the full barrier cost with constrained or trapped exit, which pins them at the full-target side; gray-market practitioners damp slightly below full target because informal practice is a partial, risky exit. Board administrators derive a low directional value from their fee-funded benefit; legislatures set the rules without directly collecting, landing nearer symmetric. No directionality overrides were needed — the beneficiary/victim declarations plus exit options reproduce the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as a snare keeps the genuine screening residue visible without letting it launder the whole structure: for a subset of covered occupations the competence signal is real and worth its cost, and the per-seat computation lets those trades classify differently from the long tail where the harm case is thin. The founding-problem interview marks the mandate's status contested rather than dead — the mismatch check (contested status against a world_rearranges verdict) routes the capture question to investigation instead of letting either the safety story or the access-filter story settle it by assertion. If the founding problem were ever resolved as dead while the arrangement persisted unchanged, the classification would migrate toward piton; the rising theater series is the leading indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This story instantiates the graduated_access_filter reading of the licensing_statute_mandate kernel; the public_safety_coordination and rent_seeking_suppression readings assign the same statutes different operative functions, victim sets, and authority grounds (practitioner-board expertise versus statutory lineage). Which structural element — the statute''s operative function, its victim set, or its authority grounding — carries the disagreement between readings?',
    'Classify the three sibling stories side by side and compare computed beneficiary/victim sets and per-seat types; locate the disagreement where the computed structures diverge.',
    'Under the safety reading the salient injured parties are consumers facing incompetent practitioners and the arrangement leans coordination; under the rent reading they are consumers paying inflated prices; under this reading they are excluded entrants sorted by resources. Grounding authority in lineage rather than expertise would change the interpretation-layer analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame location: one of three readings of the occupational-licensing kernel; disagreement located in operative function, victim set, and authority grounding.').

omega_variable(
    resource_vs_ability_sorting,
    'Does the observed completion and pass gradient across applicant classes track prior resource access (tuition capacity, unpaid supervised hours, exam prep) or pre-existing differences in relevant ability and preparation?',
    'Quasi-experiments from jurisdictions that removed specific barriers — felony-history bans, clock-hour reductions, universal license recognition — tracking who enters, at what cost, and with what subsequent performance.',
    'If resource-driven, the class-filter characterization is confirmed and the barrier is the operative mechanism; if ability-driven, the exclusionary attribution weakens toward legitimate screening and effective extraction falls accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_vs_ability_sorting, empirical, 'Whether the sorting variable is prior resources or competence.').

omega_variable(
    harm_evidence_coverage,
    'For what fraction of statutorily covered occupations does credible evidence exist that absence of licensure produces consumer harm?',
    'Systematic review of state sunset-review findings and causal studies of licensing abolition or deregulation episodes across occupations.',
    'Low harm-evidence coverage thins the coordination cover and strengthens the exclusion-mechanism reading; high coverage for the major trades would push the arrangement toward a hybrid with a genuine coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_evidence_coverage, empirical, 'Empirical breadth of the harm-prevention rationale across covered occupations.').

omega_variable(
    internalized_gate_deterrence,
    'Is non-entry by resource-poor aspirants driven by the structural cost of the barrier alone, or also by internalized expectations formed before any encounter with the requirement?',
    'Application surges in jurisdictions that cut fees or hours: if entry rises sharply without any change in candidate skill, part of the prior non-entry was internally enforced rather than externally blocked.',
    'An internalized component raises effective suppression above the structural measure and predicts persistence of the sorting pattern even after statutory reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_gate_deterrence, empirical, 'Structural versus internalized deterrence at the entry gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(lice_tr_t0, observed).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(lice_tr_t10, observed).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(lice_tr_t20, observed).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(lice_tr_t30, observed).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(lice_tr_t40, observed).
narrative_ontology:measurement(lice_tr_t50, licensing_statute_mandate__graduated_access_filter, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(lice_tr_t50, observed).
narrative_ontology:measurement(lice_tr_t60, licensing_statute_mandate__graduated_access_filter, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(lice_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(lice_be_t0, observed).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(lice_be_t10, observed).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(lice_be_t20, observed).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(lice_be_t30, observed).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(lice_be_t40, observed).
narrative_ontology:measurement(lice_be_t50, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 50, 0.71).
narrative_ontology:measurement_basis(lice_be_t50, observed).
narrative_ontology:measurement(lice_be_t60, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 60, 0.74).
narrative_ontology:measurement_basis(lice_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(lice_su_t0, observed).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(lice_su_t10, observed).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(lice_su_t20, observed).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(lice_su_t30, observed).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(lice_su_t40, observed).
narrative_ontology:measurement(lice_su_t50, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 50, 0.77).
narrative_ontology:measurement_basis(lice_su_t50, observed).
narrative_ontology:measurement(lice_su_t60, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 60, 0.8).
narrative_ontology:measurement_basis(lice_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, identity_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'occupational licensing' decomposes into three structurally distinct claims per the epsilon-invariance principle — a harm-prevention coordination claim, a rent-transfer claim, and this access-incidence claim — each with its own epsilon, victim set, and classification. The public_safety_coordination story is the upstream member: its harm-prevention premise is the justification routinely cited for the barrier operation this story measures, so upstream legitimacy conditions shape downstream enforcement. All three files link one another via affects_constraints; comparing their computed structures locates the kernel disagreement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
