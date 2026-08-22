% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__severity_carve_out_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Doctrine — Severity Carve-Out Reading (Critical Systems)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   Vendors shipping software into hospitals, banks, aircraft, and vehicles
 *   have long marked immature builds as beta and written disclaimers capping
 *   or eliminating their liability for failures. The severity carve-out
 *   reading holds that in life-safety, financial, and other critical domains
 *   this mechanism is categorically unavailable: harm severity overrides
 *   contractual liability allocation regardless of testing status or
 *   disclosure. The rule operates through decentralized judicial refusal to
 *   enforce such disclaimers, reinforced by sector regulators whose premarket
 *   and incident-reporting regimes presuppose vendor accountability. This
 *   file instantiates ONE reading of the beta_designation_doctrine kernel;
 *   the expansive_shield_reading and narrow_warning_reading siblings are
 *   separate constraints with their own epsilon values and victim sets,
 *   linked via network.affects_constraints. The epsilon referent is the
 *   standing arrangement under contest — the carve-out rule itself as this
 *   reading assesses it — not the shield regime the reading rejects.
 *
 * KEY AGENTS:
 *   - critical_software_vendors: primary cost-bearer (powerful/constrained) — bears restored liability exposure it cannot disclaim
 *   - injured_critical_system_users: primary protected party (powerless/trapped) — retains a compensation path closed by beta labels under rival readings
 *   - adjudicating_courts: rule administrator (institutional/generational) — refusal lines define the rule's reach case by case
 *   - sector_safety_regulators: secondary administrator (institutional/continental) — administrative teeth beyond courtroom refusal
 *   - enterprise_critical_system_deployers: dual-positioned intermediary (powerful/mobile) — gains recourse, absorbs residual operational risk
 *   - plaintiff_class_action_counsel: fee-stream collector (organized/mobile) — finances the test cases that build the refusal line
 *   - open_source_contributors: unrepresented affected party (powerless/mobile) — volunteer code inside critical systems with no seat in the doctrine's formation
 *   - liability_insurers: incidental beneficiary (institutional/global) — converts unpredictable exposure into ratable premium volume
 *   - technology_policy_academics: analytical observer — tracks the doctrine, supplies arguments to both sides
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.21).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.45).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.21).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.46).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Doctrine — Severity Carve-Out Reading (Critical Systems)").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '2f04fa66-4a23-4b03-a4fe-d26cf5a86379').
narrative_ontology:cs_kernel_codification('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', distributed).
narrative_ontology:cs_authority_grounding('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', distributed).
narrative_ontology:cs_reading_relation('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_axiom('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', foundational, harm_severity_overrides_contractual_allocation).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contractual_allocation, holdable).
narrative_ontology:cs_axiom_grounding('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', harm_severity_overrides_contractual_allocation, deontological).
narrative_ontology:cs_axiom('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', secondary, categorical_critical_domain_exclusion).
narrative_ontology:cs_axiom_status(categorical_critical_domain_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', categorical_critical_domain_exclusion, conventional).
narrative_ontology:cs_reference_frame('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', severity_conditioned_liability_allocation).
narrative_ontology:cs_drift_state('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', contemporary_ai_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f04fa66-4a23-4b03-a4fe-d26cf5a86379', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, injured_critical_system_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, diligent_software_vendors).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, enterprise_critical_system_deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, sector_safety_regulators).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, plaintiff_class_action_counsel).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, liability_insurers).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, critical_software_vendors).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, enterprise_critical_system_deployers).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, harm_severity_precedence_principle).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, public_policy_limits_on_liability_waivers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and sell software that runs hospital equipment, trading and core-banking rails, aircraft subsystems, and driver-assist stacks. During development they mark builds as beta and have historically written disclaimer language capping or eliminating their liability for failures. Under the prevailing severity rule those disclaimers do not hold in these deployments, so expected failure costs stay on their balance sheets; they respond by buying insurance, negotiating indemnities, staging rollouts, and lobbying for broader shield recognition or steering disputes into arbitration. Leaving the rule would mean abandoning critical-market revenue entirely, and no major vendor does.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_software_vendors, payer,
    powerful, biographical, constrained, global).

% Patients attached to infusion pumps, account holders behind core-banking software, occupants of vehicles running assistance stacks. They cannot inspect the software they depend on, cannot negotiate its terms, and cannot exit the hospital, the bank, or the road. When a failure injures them, the severity rule keeps a compensation path open against the vendor that a beta label would otherwise close. Individually they hold no leverage; their voice arrives aggregated through class counsel and regulators.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, injured_critical_system_users, beneficiary,
    powerless, biographical, trapped, national).

% State appellate benches and federal courts decide whether beta disclaimer language is enforceable in a given dispute, and their refusal lines define the rule's reach case by case. They cannot exit their dockets and rarely revisit settled refusal doctrine absent legislation or en banc pressure; their horizon runs to institutional legitimacy across generations of litigants.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, adjudicating_courts, agenda_setter,
    institutional, generational, constrained, national).

% Medical-device, aviation, and financial regulators write premarket-review and incident-reporting rules that presuppose vendor accountability, and their recall and penalty powers give the severity rule administrative force beyond courtroom refusal. They gain a clearer enforcement baseline from the rule's existence; their writ runs across continental regulatory blocs such as the EU and its product-liability revisions.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, sector_safety_regulators, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__severity_carve_out_reading, sector_safety_regulators, beneficiary).

% Hospital chains, banks, airlines, and manufacturers procure and integrate the software. They bargain for indemnities and service levels and benefit from retaining recourse when a supplier's immature build corrupts records or halts operations. They also absorb what the rule leaves with them: integration failures, downtime, and the operational risk of running pre-release builds they accepted for early access or price. They can switch suppliers and jurisdictions more readily than any other party.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, enterprise_critical_system_deployers, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__severity_carve_out_reading, enterprise_critical_system_deployers, payer).

% Contingency-fee firms identify failure patterns across user populations, finance the litigation that produces the refusal-line precedents, and take a percentage share of recoveries. Their income tracks the volume of actionable failures; they select forums and defendants strategically and abandon losing theories quickly.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, plaintiff_class_action_counsel, beneficiary,
    organized, biographical, mobile, national).

% Volunteer maintainers write components that end up inside medical, financial, and infrastructure systems, often without knowledge of the deployment. The severity debate proceeds as if commercial vendors were the only suppliers; nobody represents the position that unpaid contributors cannot price liability at all. Their personal exit — abandoning a project — is open, but taking it dissolves the commons they built.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, open_source_contributors, excluded,
    powerless, biographical, mobile, global).

% Underwrite the vendor and deployer side of critical-software risk. A stable refusal-line doctrine converts an unpredictable exposure into a ratable one, and premium volume grows with every sector pulled under the rule. They are diversified across jurisdictions and can reprice or withdraw from lines at will.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, liability_insurers, beneficiary,
    institutional, generational, mobile, global).

% Law professors and interdisciplinary researchers track the doctrine's spread, publish the empirical work on chilling effects and compensation gaps, and supply the arguments both sides cite. They hold no stake in outcomes and can study any jurisdiction.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, technology_policy_academics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a critical-software quality collective-action problem: by making immaturity labels legally inert where stakes are catastrophic, it keeps failure costs on the parties positioned to prevent them, preserves a compensation channel for people who cannot negotiate or inspect, and stops label-shifting suppliers from undercutting those that invest in testing.
% TRANSFER_FUNCTION: Moves expected-loss-bearing, and post-hoc damage payments, from users and bystanders of critical-system failures back to vendors and their insurers; incidentally moves fee income to plaintiff counsel and premium volume to insurers.
% ABSENT_VOICES: End-users are present only through representatives — class counsel and regulators speak for patients and depositors who never consented to anything. Open-source contributors whose code ships inside critical systems have no seat at all: the doctrine was built around commercial vendors and is silent on volunteer liability. Small vendors without compliance departments are likewise unrepresented in the amicus and rulemaking processes that large players dominate.
% DISAPPEARANCE_RATIONALE: If the carve-out vanished overnight and the expansive shield took its place, vendors would re-mark critical deployments as beta within a quarter, arbitration clauses would sweep up the remainder, compensation paths for injured users would close, insurance markets would reprice or withdraw, and quality-testing investment would fall as the price signal for internalizing failure costs disappeared — the critical-software economy would reorganize around disclaimability.
% FOUNDING_PROBLEM: Vendors shipping demonstrably immature software into hospitals, banks, and vehicles under beta labels while disclaiming all liability, leaving injured parties uncompensated and removing the price signal that funds testing.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: appellate opinions declining to enforce beta disclaimers in fatality and data-loss litigation, sector-regulator enforcement records (medical-device recalls, banking-system outage reports), and reinsurer loss data. No source outside the benefiting parties attests that the founding problem is resolved; the vendor side attests it is exaggerated, which is contestation of degree, not of existence.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.21, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).
:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is low (0.21) because no seat collects rents: damage recoveries pass through to compensation, and counsel fees and insurer premiums are market-rate returns for services rather than structural surplus — the rule reallocates expected losses instead of producing a capturable stream. The residual is litigation overhead, mild over-deterrence of beneficial staged rollouts, and the fee-share skim tracked by the corrective_vs_rent_extraction omega. Suppression (0.45) is real but bounded: the rule categorically overrides private agreement in governed domains — genuine coercion — yet leaves testing, insurance, indemnity negotiation, and staged-release alternatives intact, and its enforcement is decentralized judicial refusal rather than dedicated coercive machinery. Theater is low (0.14): refusal decisions alter vendor conduct, with only ceremonial opinion rhetoric and marketing-grade beta pages that carry no legal effect. Accessibility collapse is moderate (0.46): once the rule is understood, pure market-discipline and self-certification alternatives partially collapse, but disclosure-and-insure substitutes remain workable. Resistance is substantial (0.58): sustained industry end-runs through arbitration clauses, forum selection, and lobbying. The three measurement series share one time grid (t=0,6,12,18,24,30) so every metric is authored at every examined point; the rising suppression_requirement series tracks the enforcement-intensification narrative (ad hoc refusals maturing into codified sector regimes), not mere extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor seat the rule presents as an imposed, uncapped exposure with no visible collection point — the firm writes checks and buys premiums and cannot see who, if anyone, profits. From the injured-user seat the same rule presents as the only thing standing between them and an unanswerable 'beta' defense. Partisans of the expansive_shield_reading would author this same kernel with the valence inverted — users as victims, vendors as the coordinated class. The engine computes these per-seat divergences from the structural data; this file's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (injured users, diligent vendors, enterprise deployers) derive low directionality — the rule subsidizes their recourse and level playing field. Critical-software vendors derive high directionality from their payer role plus constrained exit: no major vendor exits critical-market revenue, so they sit near the full-target end even though the costs they bear are, on this reading, corrective rather than extracted. Enterprise deployers sit mid-range via their dual beneficiary/payer position. Plaintiff counsel is the one correction the structural derivation gets wrong: declared a beneficiary, the derivation would render them strongly subsidized, but they are better described as a compensated processor of the rule's flows — hence the directionality override to 0.45 at the organized power atom (the only organized-power seat in the story, so the override targets it uniquely). Courts and regulators derive mid administrative values; insurers derive low-mid, consistent with genuine premium-volume benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — vendors disclaiming liability for demonstrably immature software deployed where failures kill or bankrupt — is live, not dead: every new critical domain (clinical AI, real-time payment rails) revives the disclaimer temptation, and the corroboration record shows no outside source attesting resolution. With founding_problem_status=live and disappearance_verdict=world_rearranges, the mismatch consumer finds no dead-mandate flag, and mandatrophy_resolved is deliberately not declared. On this reading the rule persists because its function persists; the rope claim predicts continued functional operation rather than theatrical maintenance, and the flat-to-mildly-rising theater series is consistent with that.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the beta_designation_doctrine kernel governs — universal waiver (expansive_shield_reading), time-bounded testing disclosure (narrow_warning_reading), or categorical severity exclusion (this reading)?',
    'Appellate convergence across jurisdictions or statutory codification of the shield''s scope; until then the three readings persist as rival constraints with distinct epsilon values and distinct beneficiary/victim structures.',
    'Sibling adoption inverts the structural picture: under the expansive reading, users of critical systems become the victim class and epsilon assessed from their seat is high; under this reading vendors bear corrected costs and no victim class is declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'This constraint is one of three rival readings of the beta-designation kernel; the disagreement is located on the scope variable — the domains in which beta designation validly allocates liability.').

omega_variable(
    corrective_vs_rent_extraction,
    'Is vendor cost-bearing under the severity rule purely corrective (expected damages returned to their causer), or does the litigation system skim a rent share through contingency fees and insurance loading?',
    'Benchmark plaintiff-fee shares and insurance loadings in critical-software litigation against comparable product-liability lines; material excess over competitive returns indicates rent capture.',
    'Demonstrated rent capture would revise epsilon upward and drift the computed classification toward a hybrid coordination/extraction profile; clean benchmarks support the pure-coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corrective_vs_rent_extraction, empirical, 'Whether the rule''s cost flows contain a rent component beyond compensatory transfer.').

omega_variable(
    innovation_chill_tradeoff,
    'Does categorical unavailability deter beneficial staged deployments of safety software (shadow-mode fraud models, adaptive-system pilots) more than it deters premature release?',
    'Difference-in-differences on staged-rollout rates across severity-governed versus ordinary software domains before and after refusal-line consolidation.',
    'Net chill adds a deadweight component to epsilon and pressures the classification away from pure coordination; net deterrence supports it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_chill_tradeoff, empirical, 'Whether the categorical bar''s deterrence of recklessness outweighs its chilling of useful piloting in critical domains.').

omega_variable(
    arbitration_bifurcation,
    'Do mandatory-arbitration clauses and forum-selection practices restore beta-shield effect for sophisticated counterparties while the refusal line holds only in published consumer litigation?',
    'Compare disclaimer-enforcement rates in arbitration awards against published opinions in equivalent critical-system disputes.',
    'Confirmed bifurcation makes effective protection class-dependent, pushing the user seat''s directionality toward full target and opening a hidden extraction layer the headline metrics miss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbitration_bifurcation, empirical, 'Whether private ordering hollows the rule for enterprise counterparties while preserving it for consumers.').

omega_variable(
    critical_domain_boundary_open_texture,
    'Where does ''life-safety, financial, or other critical'' end — do foundation-model deployments, social-infrastructure outages, or agricultural control systems fall inside the categorical bar?',
    'Statutory enumeration, an adopted doctrinal test, or sector-regulator rulemaking drawing the boundary administratively.',
    'Boundary expansion pulls additional vendor populations under the rule and scales effective extraction through the scope modifier; contraction narrows the governed set and reopens shield space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(critical_domain_boundary_open_texture, conceptual, 'The governed-category boundary is open-textured; its placement determines who the rule reaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_severity_carveout_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(beta_severity_carveout_tr_t6, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement(beta_severity_carveout_tr_t12, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(beta_severity_carveout_tr_t18, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 18, 0.11).
narrative_ontology:measurement(beta_severity_carveout_tr_t24, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(beta_severity_carveout_tr_t30, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 30, 0.14).

% Extraction over time
narrative_ontology:measurement(beta_severity_carveout_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(beta_severity_carveout_be_t6, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 6, 0.16).
narrative_ontology:measurement(beta_severity_carveout_be_t12, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement(beta_severity_carveout_be_t18, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 18, 0.19).
narrative_ontology:measurement(beta_severity_carveout_be_t24, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(beta_severity_carveout_be_t30, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 30, 0.21).

% Suppression requirement over time
narrative_ontology:measurement(beta_severity_carveout_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(beta_severity_carveout_su_t6, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 6, 0.32).
narrative_ontology:measurement(beta_severity_carveout_su_t12, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(beta_severity_carveout_su_t18, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 18, 0.39).
narrative_ontology:measurement(beta_severity_carveout_su_t24, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(beta_severity_carveout_su_t30, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'beta designation doctrine' decomposes into three structurally distinct readings sharing one kernel: universal waiver, bounded testing disclosure, and categorical severity exclusion. Per the epsilon-invariance principle each is a separate file with its own epsilon and beneficiary/victim structure; this file links both siblings. The expansive reading is the historical baseline practice; this reading arises as a reactive limitation on it, so the dependency runs from the baseline practice's persistence to this reading's content.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__severity_carve_out_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
