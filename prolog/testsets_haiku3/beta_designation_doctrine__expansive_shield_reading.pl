% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__expansive_shield_reading, []).

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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Beta Designation as Comprehensive Liability Shield
 *   domain: technology/legal/consumer_protection
 *
 * SUMMARY:
 *   Software developers and platform operators use beta designation to
 *   disclaim all liability for defects, data loss, security breaches, and
 *   operational failures—regardless of how long the software has been in
 *   public use, how critical it is to user operations, or how many third
 *   parties depend on it. This constraint is the EXPANSIVE SHIELD READING of
 *   the beta-designation doctrine: beta status is treated as a comprehensive,
 *   indefinite, universal liability waiver. The narrow-warning reading treats
 *   beta as a time-bounded disclosure of immaturity; the severity-carve-out
 *   reading categorically excludes beta status from critical systems. This
 *   story instantiates only the expansive reading: beta means developers
 *   externalize all defect costs to users and dependent systems,
 *   indefinitely, without temporal or severity boundaries.
 *
 * KEY AGENTS:
 *   - software_developers: agenda_setter, institutional power, arbitrage exit — define and enforce the expansive reading through EULA clauses and litigation defense.
 *   - beta_software_users: payer, powerless power, identity_locked exit — adopt beta software for personal/organizational use and bear all defect costs despite lack of liability recourse.
 *   - platform_operators: beneficiary, institutional power, arbitrage exit — profit from hosting beta software by distributing liability waivers and defending them in disputes.
 *   - dependent_systems: payer, powerless power, trapped exit — depend on beta software as infrastructure and face cascading costs when it fails, with no recovery path.
 *   - harmed_third_parties: payer, powerless power, trapped exit — suffer harm from beta software failures they did not consent to and cannot recover because the waiver is treated as pre-empting tort liability.
 *   - consumer_protection_authorities: observer, institutional power, analytical exit — can impose temporal limits, severity carve-outs, or unconscionability doctrine to reshape the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.87).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.71).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive Liability Shield").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology/legal/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, '9828e1c1-e5f2-4953-8d50-2568814c202a').
narrative_ontology:cs_kernel_codification('9828e1c1-e5f2-4953-8d50-2568814c202a', distributed).
narrative_ontology:cs_authority_grounding('9828e1c1-e5f2-4953-8d50-2568814c202a', extraction).
narrative_ontology:cs_reading_relation('9828e1c1-e5f2-4953-8d50-2568814c202a', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_reading_relation('9828e1c1-e5f2-4953-8d50-2568814c202a', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('9828e1c1-e5f2-4953-8d50-2568814c202a', foundational, beta_status_complete_liability_waiver).
narrative_ontology:cs_axiom_status(beta_status_complete_liability_waiver, holdable).
narrative_ontology:cs_axiom_grounding('9828e1c1-e5f2-4953-8d50-2568814c202a', beta_status_complete_liability_waiver, conventional).
narrative_ontology:cs_axiom('9828e1c1-e5f2-4953-8d50-2568814c202a', foundational, indefinite_beta_designation_permissible).
narrative_ontology:cs_axiom_status(indefinite_beta_designation_permissible, holdable).
narrative_ontology:cs_axiom_grounding('9828e1c1-e5f2-4953-8d50-2568814c202a', indefinite_beta_designation_permissible, conventional).
narrative_ontology:cs_reference_frame('9828e1c1-e5f2-4953-8d50-2568814c202a', developer_autonomy_in_software_release).
narrative_ontology:cs_drift_state('9828e1c1-e5f2-4953-8d50-2568814c202a', contemporary_critical_infrastructure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9828e1c1-e5f2-4953-8d50-2568814c202a', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, platform_operators).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, beta_software_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, dependent_systems).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, harmed_third_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declare software 'beta' to shield from all liability for defects, data loss, security breaches, and system failures. The expansive reading permits indefinite beta designation on any software regardless of user population or criticality. Developers argue beta status is a transparency signal about maturity; in practice, it functions as a unilateral waiver of product liability and breach-of-warranty obligations that would otherwise apply. They enforce this reading by inserting beta disclaimers into end-user license agreements (EULAs) and enforcing them through litigation.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_developers, agenda_setter,
    institutional, generational, arbitrage, global).

% Adopt software labeled 'beta' for personal or organizational use, often unaware of the liability waiver or unable to exit because the software is critical to their workflow. They bear all defect costs: data corruption, security compromise, privacy breach, system downtime, and financial loss. Their options are limited: accept the risk or forgo the software entirely (often infeasible if competitors impose the same beta-shield condition). Professional identity (IT staff, data stewards) and organizational dependence lock them into continued use despite the waiver.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, beta_software_users, payer,
    powerless, biographical, identity_locked, global).

% Operate platforms (app stores, cloud services, operating systems) that host or depend on beta-designated software. They benefit from the liability shield by avoiding responsibility for distributing software with known or latent defects. They enforce the shield by contractually requiring developers to include comprehensive beta disclaimers and by defending the reading in disputes with affected users.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Organizations or critical infrastructure that incorporate beta software as a dependency: supply chains, hospitals, financial systems, industrial control systems. When the beta software fails, they absorb cascading costs—operational disruption, data loss, liability to their own customers—but have no recourse because the developers disclaimed all liability. They are excluded from the contract between developers and direct users; the waiver applies by default to all downstream harm.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, dependent_systems, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, dependent_systems, excluded).

% Persons or entities harmed by beta software failures they did not consent to: patients whose medical records are corrupted, investors whose trading systems crash, individuals whose privacy is breached through a beta application. They are wholly excluded from the transaction and the waiver, yet the developers' beta disclaimer often prevents users from recovering and passing liability upstream, blocking tort remedies.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, harmed_third_parties, payer,
    powerless, immediate, trapped, global).

% Regulate consumer protection, product liability, and fair contract terms. They investigate whether comprehensive beta waivers are unconscionable, whether indefinite beta designation is deceptive, and whether the waiver overrides mandatory consumer protections or liability caps. They can impose remedies—term duration limits, severity carve-outs, mandatory warnings—that would alter or collapse the constraint.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_protection_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, software_developers).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Theoretically: permits rapid iteration and user testing by reducing liability exposure during early development. Enables early-access feedback from users willing to accept defects in exchange for early availability or participation in development.
% TRANSFER_FUNCTION: Moves all defect costs, including breach-of-warranty claims, data loss, security compromise, and downstream third-party harms, from developers to users and dependent systems. Developers retain all revenue from beta software while externalizing all quality risk.
% ABSENT_VOICES: Harmed third parties and downstream dependent systems are structurally absent: they did not consent to the waiver, did not benefit from the testing, and have no contractual seat at the arrangement. Consumer protection authorities and tort doctrine advocates argue for time-bounded beta windows and severity carve-outs but are excluded from the developing reading's enforcement coalition.
% DISAPPEARANCE_RATIONALE: If comprehensive indefinite beta waivers vanished and product liability law applied to all software regardless of designation, developers would face significant liability exposure on software released to the public, pricing and release strategies would shift, many developers would shorten public-facing beta windows or eliminate them, and users would recover damages through tort or breach-of-warranty claims. The software development and distribution ecosystem would reorganize around standard product liability rather than liability exemption.
% FOUNDING_PROBLEM: Early software was novel and unstable; users who participated in testing understood the defect risk and accepted pre-release software in exchange for early access. Beta designation was intended to flag genuinely immature software in active development, time-bounded and explicit.
% FOUNDING_PROBLEM_CORROBORATION: Consumer protection advocates and regulatory authorities attest that the founding problem is substantially resolved: software distribution is now professional and scaled, beta periods are often indefinite and invisible to users, and many beta-designated applications are in production use for millions of people. Independent economic analysis and testimony from harmed users document that beta designation persists as liability exemption long after software matures. The developer and platform operator seats defend continued indefinite beta authority, citing innovation speed; that defense comes from the parties who benefit from the waiver and is contradicted by independent accounts from consumer advocates and tort scholars.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__expansive_shield_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__expansive_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.87 at interval end) because developers and platforms transfer all defect liability to users and dependent systems while retaining revenue. The extraction is growing over the interval (0.71 to 0.87) as software that was genuinely beta matures and remains indefinitely beta-designated, increasing the proportion of software revenue collected under the waiver. Suppression is substantial (0.71) because identity-locking and power asymmetry (powerless users, institutional developers) prevent users from effectively exiting or negotiating waiver terms. Theater is moderate (0.42) because the beta designation carries a genuine testing function early in the constraint's application, but that function atrophies as software matures while the waiver persists—the measurement series captures this drift. Accessibility of alternatives is low (0.68 collapse) because beta software often becomes foundational to users' workflows, and competitors impose the same beta-shield reading, leaving few genuine exits. Resistance is moderate (0.59) because consumer advocates and regulatory authorities actively contest the reading, but enforcement machinery (litigation, EULA adhesion, platform enforcement) suppresses this resistance.
 *
 * PERSPECTIVAL GAP:
 *   A developer and a user adopting beta software see structurally opposite arrangements. The developer sees a transparency mechanism and risk-sharing contract; the user (especially one whose organization depends on the software) sees an indefinite liability exemption they cannot negotiate. The developer's seat should compute the constraint as moderate coordination (risk disclosure is real); the user's seat should compute it as pure extraction (liability externalization with no genuine alternative). This gap is structural and arises from the power and exit-option asymmetry, not from measurement ambiguity. The engine computes this divergence from the authored stakeholder data.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers sit at d ≈ 0.0 (full beneficiary): they collect liability exemption revenue and externalize all quality risk. Beta software users sit at d ≈ 1.0 (full target): they pay all defect costs and are locked by identity and workflow dependence. Platform operators sit at d ≈ 0.1–0.2: they benefit from distribution liability shields but face regulatory and reputational pressure. Dependent systems and third parties also sit at d ≈ 1.0 but are wholly excluded from the contract—the waiver applies by default to their harms. The directionality is extreme: asymmetric power, asymmetric liability, asymmetric exit. The engine derives this from the declared beneficiary/victim structure and the stakeholder exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead: early software testing genuinely required users to accept defect risk because software was novel and often unstable. That problem is substantially resolved—professional software development now includes rigorous internal testing, staged rollouts, and user acceptance testing before public release. Yet the beta waiver persists indefinitely and universally, long after the founding problem that justified it has vanished. This is classic mandatrophy: the mandate (enable early testing with explicit risk disclosure) outlived its function (testing is now internal), and the arrangement persists as pure extraction (liability exemption indefinitely applied to mature software). The constraint is not holding the founding problem in equilibrium; it is defending a benefit (liability exemption) that no longer serves the founding function. The theater_ratio increase from 0.28 to 0.42 documents this: the proportion of beta-designated software that is mature (not genuinely beta) is growing, and the waiver is increasingly performing liability shield rather than testing disclosure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_vs_structural,
    'To what degree is the measured suppression (0.71) structural (external barriers: powerless users, institutional developers, EULA adhesion) versus internalized (users believe they deserve the risk or accept it as inevitable)?',
    'Post-exit suppression tracking: if users who leave the beta software and shift to alternatives report reduced constraint awareness, suppression was internalized; if they report the same waivers from competing products, suppression is structural.',
    'If suppression is largely internalized, the constraint''s real hold is psychological/identity-based, not market-structural. If suppression is structural, the constraint''s hold is maintained by power asymmetry and EULA adhesion, which are more tractable targets for regulatory intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'The composition of measured suppression: structural barriers versus internalized acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(beta_tr_t25, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 5, 0.76).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.86).
narrative_ontology:measurement(beta_be_t25, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 25, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(beta_su_t25, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__expansive_shield_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% The beta-designation doctrine decomposes into three structurally distinct constraints: (1) expansive_shield_reading (this story): beta is indefinite, universal, absolute liability waiver — high extraction, low alternatives. (2) narrow_warning_reading: beta is time-bounded testing disclosure, base product liability preserved — moderate extraction, genuine alternatives. (3) severity_carve_out_reading: beta cannot waive liability for critical systems — moderate extraction, differentiated by system type. These readings instantiate different ε values, different victim structures, and different legal doctrines. They form a constraint family linked by kernel contest: all three readings are live positions in the software law debate; no single reading has foreclosed the others despite decades of practice. The expansive reading influences the others (its precedents are cited in narrow and severity debates), but coexists with them in different jurisdictions and institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__expansive_shield_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
