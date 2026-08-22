% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Beta Designation as Comprehensive Liability Shield (Expansive Reading)
 *   domain: technology/law/consumer_protection
 *
 * SUMMARY:
 *   Under this expansive reading of the beta-designation doctrine, software
 *   publishers can indefinitely label products as 'beta,' thereby disclaiming
 *   all liability for defects—regardless of the product's actual maturity,
 *   the duration of the beta phase, or the criticality of the systems it
 *   operates in. A Gmail clone can remain labeled beta for a decade while
 *   millions depend on it daily. A financial trading platform can disclaim
 *   liability for data loss. Medical software can operate under beta immunity
 *   indefinitely. The reading extracts by transferring the cost of software
 *   defects (financial loss, privacy breach, operational disruption) entirely
 *   to users, while developers retain revenue and market presence without
 *   bearing failure costs. This constraint is authored as a SNARE because
 *   developers externalize all defect liability by fiat, the constraint
 *   persists through enforcement against users (contract terms, platform
 *   policies, norm acceptance), and users have limited alternatives once
 *   dependent on the service. The claim/metric gap is intentional: this
 *   reading CLAIMS comprehensive immunity while the metrics show high
 *   extraction, moderate theater (some quality-assurance activity is real,
 *   but increasingly serves to legitimize immunity rather than reduce defect
 *   rate), and substantial suppression. The engine will compute whether payer
 *   and agenda-setter seats perceive this differently—as the framework
 *   intends.
 *
 * KEY AGENTS:
 *   - software_developers: agenda-setters who write and publish the software; they define what qualifies as beta, determine duration, control disclosure; they avoid liability costs by fiat
 *   - venture_capital_markets: beneficiaries who profit from accelerated launch cycles and derisked development (startups can launch without full product-liability insurance); the beta doctrine subsidizes rapid iteration by externalization
 *   - beta_software_users: victims who bear the cost of defects (data loss, service unavailability, privacy breach); they depend on the software but enter into contracts that disclaim the developer's duty of care
 *   - dependent_systems_operators: secondary victims whose critical operations (hospitals, trading floors, power grids) depend on beta software and absorb cascade failures; their exit is often impossible
 *   - consumer_protection_regulators: excluded analysts who argue beta indefiniteness contradicts consumer-protection law; they are kept out of the immunity negotiation by contract-to-contract enforcement at the individual-user level
 *   - narrow_reading_advocates: opposing doctrine-holders who argue beta must be time-bounded and product-liability preserved; they remain in conceptual contest but lack institutional power to enforce their reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.81).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.72).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive Liability Shield (Expansive Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology/law/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, 'f1606e95-60ea-48d0-8e7b-7a8acaaae9f3').
narrative_ontology:cs_kernel_codification('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', distributed).
narrative_ontology:cs_authority_grounding('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', extraction).
narrative_ontology:cs_reading_relation('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', foundational, beta_designation_waives_all_liability).
narrative_ontology:cs_axiom_status(beta_designation_waives_all_liability, holdable).
narrative_ontology:cs_axiom_grounding('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', beta_designation_waives_all_liability, conventional).
narrative_ontology:cs_axiom('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', foundational, indefinite_beta_duration_permissible).
narrative_ontology:cs_axiom_status(indefinite_beta_duration_permissible, holdable).
narrative_ontology:cs_axiom_grounding('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', indefinite_beta_duration_permissible, empirically_contingent).
narrative_ontology:cs_reference_frame('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', developer_autonomy_liability_exfiltration).
narrative_ontology:cs_drift_state('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', contemporary_regulatory_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f1606e95-60ea-48d0-8e7b-7a8acaaae9f3', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, venture_capital_markets).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, beta_software_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, dependent_systems_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write, publish, and distribute software. Under this reading, they can label any product 'beta' and disclaim all liability for defects, regardless of the product's actual maturity or the duration of the beta phase. They collect revenue from users while externalizing the cost of defects. They enforce this by including liability-waiver language in terms of service and by leveraging platform policies (app stores, SaaS providers) that enforce the waiver on their behalf.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_developers, agenda_setter,
    institutional, generational, arbitrage, global).

% Fund software startups and early-stage companies. The beta shield enables rapid market entry and derisked product development: startups can launch incomplete products without full product-liability insurance, reducing capital requirements and accelerating time-to-revenue. The constraint subsidizes the VC model of fast iteration and market learning.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, venture_capital_markets, beneficiary,
    institutional, generational, arbitrage, global).

% Adopt and depend on software labeled beta. They bear the cost of defects: data loss, service unavailability, privacy breaches, operational disruption. They have no recourse through liability claims because they agreed to the waiver by using the software. Switching to alternatives is difficult because the beta software is often the market leader or the only option in its category. Many users are unaware they are using beta software or do not understand the liability implications until a defect causes loss.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, beta_software_users, payer,
    powerless, biographical, constrained, global).

% Operate critical infrastructure (hospitals, financial markets, power grids, telecommunications) that increasingly depends on software labeled beta. They bear cascade-failure risk: if a beta library or service fails, their entire system can fail, affecting thousands or millions of end-users. They cannot exit because no alternative beta software exists, and they cannot downgrade to non-beta because the ecosystem has standardized on the beta product. They are maximally trapped and maximally extracted from.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, dependent_systems_operators, payer,
    powerful, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, dependent_systems_operators, observer).

% Enforce consumer-protection law and product-liability law in their jurisdictions. They argue that beta designation cannot exempt developers from the implied warranty of merchantability, the duty of care, or the prohibition on unconscionable contract terms. They lack institutional power to enforce this reading because the waiver is negotiated individually between developer and user through take-it-or-leave-it contracts, keeping regulators out of the conversation until a catastrophic failure prompts investigation.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_protection_regulators, excluded,
    institutional, generational, analytical, national).

% Legal scholars, consumer advocates, and some courts that argue beta must be time-bounded and product liability preserved. They read the beta doctrine as a legitimate testing shield, not a permanent waiver. They produce competing jurisprudence, amicus briefs, and advocacy, but remain outside the enforcement frame because the expansive reading has institutional power in the dominant technology markets.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, narrow_reading_advocates, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, software_developers).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces product-liability insurance costs and accelerates time-to-market for experimental software. Permits startups to launch with incomplete products and iterate based on user feedback, rather than waiting for complete development. Enables rapid innovation cycles in software markets.
% TRANSFER_FUNCTION: Transfers the cost of software defects from developers (who would otherwise bear liability insurance and damages) to users (who bear the cost of lost data, downtime, privacy breach, operational disruption). Also transfers the opportunity cost of accelerated launch from QA and risk management to post-launch user discovery of defects.
% ABSENT_VOICES: Injured users who suffered loss but accepted the waiver under adhesion contracts without understanding the full liability implications. Post-failure regulators who investigate accidents but find the developer immune to liability claims. Alternate vendors who tried to compete with higher-quality standards but could not match the speed-to-market of beta-labeled competitors. Consumer-protection advocates and product-liability scholars who contest the reading but lack institutional power in private-contract negotiation.
% DISAPPEARANCE_RATIONALE: If this reading disappeared (replaced by the narrow_warning_reading or foreclosed by statutory override), software development would slow: developers would need full product-liability insurance before launch, increasing capital requirements and time-to-market. Startup funding models would shift. Users would regain liability recourse, reducing their incentive to accept incomplete software. Product quality standards would rise (developers could not externalize defect costs). The venture-capital model of rapid iteration would need restructuring.
% FOUNDING_PROBLEM: Early software markets faced uncertainty: developers could not reliably predict defects in new products, and insurance companies were reluctant to cover software products with novel architectures. Product-liability law (written for physical goods) did not map cleanly to software. Developers needed a way to release experimental products without bearing unlimited liability exposure.
% FOUNDING_PROBLEM_CORROBORATION: Venture capitalists and software industry groups attest the founding problem remains live: software complexity creates genuine uncertainty and rapid iteration is essential for learning. Consumer-protection regulators, product-liability scholars, and some courts attest the founding problem is substantially solved: modern development practices (automated testing, staged rollouts, feature flags) permit learning without indefinite immunity. The regulatory and academic communities outside the benefiting parties argue that the founding problem justifies time-bounded beta, not indefinite exemption.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extraction is measured at 0.81 (highest interval value) because developers using this reading capture the entire defect-cost externality: users pay the cost (lost data, downtime, privacy breach) while developers collect revenue without liability insurance. The constraint persists without genuine coordination: the 'testing' story is theatrical—products labeled beta are often production systems serving millions. Theater_ratio = 0.48 reflects this mix: some quality assurance is real, but the majority of enforcement activity (contractual immunity language, platform policy, user-norm acceptance) serves purely to legitimize the waiver, not to reduce defect rate. Suppression = 0.72 because the constraint requires active user acceptance (contracts, click-through waivers) AND the suppression mechanisms are partially internalized (users have absorbed the belief that 'beta means no recourse,' making the suppression persist even when exit is available). Accessibility_collapse = 0.68 because once a user depends on a beta service (Gmail for email, Slack for communication, Figma for design), alternatives exist but the switching cost is high and the new service may ALSO be in beta. Resistance = 0.54 (moderate) because users and regulators both push back—class-action litigation, regulatory investigation, and norm-contestation all meet this constraint, but the agenda-setter's (developer's) control of the platform and the diffuseness of the cost prevent any unified counter-power. The measurement series show rising extraction from t=0 (0.62) to t=25 (0.81), driven by increasing scope: more critical systems adopt beta-labeled software over time, so the cost externalized increases. Theater_ratio rises slightly (0.32 to 0.48) as developers invest in quality-assurance theater to legitimize immunity. Suppression_requirement rises (0.58 to 0.72) as resistance grows and requires more contractual and norm-work to maintain the waiver.
 *
 * PERSPECTIVAL GAP:
 *   From the developer's (agenda-setter's) seat, this reading is legitimate risk allocation: 'Users can choose not to use beta software; if they do, they accept the risk. We cannot afford full product-liability insurance on experimental releases.' From the user's seat (especially the dependent-systems operator), the reading is extractive coercion: 'The software is necessary for my operations. I have no real choice. The developer collects revenue and externalizes the cost of their engineering shortcuts to me.' From the venture-capital seat, the reading is market-enabling: 'Without the beta shield, startups would be killed by liability costs before they could prove the product works; the shield accelerates innovation.' From the regulator's seat, the reading is a consumer-protection violation: 'Beta cannot exempt developers from the duty of care or the implied warranty of merchantability; indefinite duration contradicts the testing justification.' Each seat computes a different type from the same constraint because each has different power, exit options, and the extraction falls differently on each. The engine captures this through per-seat computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers are at directionality ~1.0 (full target receiving the benefit—complete exfiltration of liability costs). Venture capital markets sit near 0.0 (full beneficiary—they fund rapid launches without the insurance cost). Beta_software_users are at directionality ~0.95 (nearly full target—they depend on the service and cannot exit without substantial switching cost; they bear all defect costs). Dependent_systems_operators are at directionality ~0.98 (even more trapped than direct users—hospital IT cannot switch EMR systems mid-quarter; power grid operators cannot replace SCADA software in the middle of a crisis; they are maximally extracted from). Consumer_protection_regulators sit at ~0.5 (symmetric but powerless—they see the wrong, have no direct stake, and lack enforcement jurisdiction inside the private-contract frame). This reading's structural claim is that developers CAN unilaterally set directionality by contract: by writing the waiver, they move users from potential beneficiary (a service you want) to victim (a service you depend on, cost paid, no recourse). The engine computes directionality from beneficiary/victim + exit_options; this reading's narrative claim is that written immunity can override that computation. It cannot—the engine's directionality derivation is structural, not contractual. The divergence is the measurement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (provide developers with testing immunity and reduce product-liability insurance costs) remains live for the beneficiaries (venture capital, startups) but is contested for users and regulators. Users argue the founding problem is solved once the product is out of testing—a genuine beta phase is time-bounded. Regulators argue the founding problem does not justify indefinite immunity on critical systems. The constraint persists because developers and VCs have institutional power to set contract terms, while the cost of fixing it (statutory carve-outs for safety-critical systems, regulatory liability floors, class-action certification) is diffuse and requires coordination among users. The classification avoids the false-coordination trap: this is NOT a rope because the coordination benefit is minimal (users would prefer liability, not immunity; they accept immunity only as a condition of service access). It is NOT a tangled rope because there is no genuine coordination mixed with extraction—the coordination story is purely window-dressing. It IS a snare because the constraint's persistence depends entirely on coercion (contractual waivers, platform enforcement, norm internalization) and on suppressing alternatives (users cannot demand liability or get recourse through litigation once they click 'agree'). The theater_ratio rising over time reflects increasing focus on legitimation (quality-review language, testing protocols) rather than actual defect-reduction—a piton-like dynamic, but the extraction is still high enough and the coercion active enough that snare is the correct type, not piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indefinite_duration_coherence,
    'Can a software product remain in ''beta'' status indefinitely while delivering critical services, or does indefinite beta duration contradict the very concept of a testing phase?',
    'Doctrinal analysis of beta-designation precedent and statutory definition of ''testing phase''; empirical studies of products launched 15+ years ago still labeled beta and their feature-completeness status.',
    'If indefinite duration is incoherent with the testing justification, the constraint''s coverage scope collapses and the reading forecloses itself — extraction claims rely on the testing story. If indefinite beta is legally defensible, this reading''s extraction model stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indefinite_duration_coherence, conceptual, 'Whether indefinite beta duration is logically consistent with a testing-phase justification.').

omega_variable(
    sibling_reading_boundary_contest,
    'How is the boundary between this expansive reading and the narrow_warning_reading determined? Is it a matter of doctrine, statutory language, consumer-protection norms, or market practice?',
    'Comparative analysis of court holdings, regulatory guidance (FTC, state AG), and industry-standard practice. Jurisdiction-level variation in which reading applies.',
    'Different jurisdictions have already adopted different readings. Harmonization or divergence determines whether this constraint''s coverage is universal or contested. Some regions may foreclose this reading via statutory override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_boundary_contest, empirical, 'The doctrinal and jurisdictional determination of which reading applies.').

omega_variable(
    critical_system_carve_out_pressure,
    'How much structural pressure does the severity_carve_out_reading exert on this expansive reading? Can medical, financial, or safety-critical systems operate under beta designation without triggering liability exceptions?',
    'Review of regulatory requirements (FDA medical software, FINRA financial systems, DOT safety-critical code); emerging litigation that attempts to carve out critical systems from beta immunity.',
    'Regulatory pressure to carve out critical systems would substantially narrow the scope of this reading in practice, even if the legal doctrine technically permits indefinite beta across all contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_system_carve_out_pressure, empirical, 'The scope and enforceability of this reading in safety-critical and regulated domains.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (users unable to exit the service because they depend on it despite the defects) or internalized (users have absorbed the belief that beta software has no liability, even where alternatives exist)?',
    'Post-exit interviews with users who abandon beta-critical services and adopt alternatives; analysis of whether users'' acceptance of beta defects persists after exit, or dissolves upon engagement with products under standard liability.',
    'If internalized, the constraint''s effective suppression is higher than measured — users carry the suppression mindset beyond the original service. If primarily structural (service dependence), fixing it requires providing alternatives; if internalized, it requires norm resetting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether measured suppression is structural (external barriers) or internalized (cognitive patterns).').

omega_variable(
    kernel_reading_identity_quest,
    'This constraint is ONE reading of the beta_designation_doctrine kernel. Is the kernel best understood as a fixed legal text (statute, common law rule), a practice-grounded norm (what the market does), or a doctrine-space where readings compete? Does the kernel have written form, or is it distributed across case law and practice?',
    'Doctrinal history: trace the earliest articulation of beta-as-liability-waiver; determine whether it appears in statute, case law, or industry practice first; assess whether any single authority text grounds all readings or whether readings are in genuinely distributed competition.',
    'If the kernel is a fixed text, the reading is an interpretation and can be overruled. If distributed/practice-grounded, the readings compete for institutional endorsement. If the kernel is implicit (just what the market does), this reading IS the current market norm, not a contestable interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_quest, conceptual, 'The kernel''s ontological status — text-bound, distributed, or practice-implicit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(beta_tr_t25, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(beta_be_t25, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 25, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(beta_su_t25, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__expansive_shield_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% The beta_designation_doctrine is decomposed into three readings, each with a distinct ε and beneficiary/victim structure. The expansive_shield_reading (this constraint) interprets beta as comprehensive indefinite immunity; the narrow_warning_reading interprets it as time-bounded disclosure with preserved product liability; the severity_carve_out_reading imposes categorical prohibition on critical systems. These are distinct constraints with different structural properties, not observable-dependent variations on one claim. They are linked by kernel identity: each reading instantiates the same kernel (software can be labeled 'beta') but derives different extraction profiles from that kernel. The network edges record the doctrinal influence: the expansive reading's power depends on defeating the narrow and severity readings' institutional adoption in specific jurisdictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
