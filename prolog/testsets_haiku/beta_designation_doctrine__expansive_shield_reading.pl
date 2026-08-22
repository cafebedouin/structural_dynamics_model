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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Beta Designation as Comprehensive Liability Shield (Expansive Reading)
 *   domain: technology/legal/consumer_protection
 *
 * SUMMARY:
 *   This constraint instantiates the EXPANSIVE SHIELD READING of the
 *   contested beta-designation kernel. The expansive reading holds that
 *   affixing the label 'beta' to software constitutes a comprehensive,
 *   indefinite, universally-scoped liability waiver — developers externalize
 *   all defect costs to users and dependent institutions, regardless of
 *   severity, duration of the 'beta' phase, or criticality of the software
 *   context. This reading directly contradicts the narrow-warning reading
 *   (beta must be time-bounded testing disclosure with preserved product
 *   liability) and influences but does not foreclose the severity-carve-out
 *   reading (beta is unavailable for life-safety/financial systems). The
 *   claim/metric gap is structural: the expansive reading is CLAIMED as snare
 *   (extraction mechanism defended by contractual language) and the metrics
 *   author high extraction (0.86 at interval end), rising suppression (0.79),
 *   and substantial theater (0.67) — all consistent with extractive
 *   persistence. The measured rise in theater over 35 years indicates that
 *   the 'testing' framing has increasingly lost descriptive force (most beta
 *   software remains beta for decades) while the liability-waiver function
 *   has hardened.
 *
 * KEY AGENTS:
 *   - software_developers: agenda-setting beneficiary (set waiver terms, externalize defect costs)
 *   - beta_software_users: powerless victims (bear all defect costs, identity-locked to ecosystems)
 *   - dependent_institutions: moderate-power victims (adopt beta software for critical operations, absorb cascade failures)
 *   - end_users_harmed_by_cascade: powerless, contractually-invisible victims (harmed through institutional failures from beta software, no standing to sue developers)
 *   - regulators_and_consumer_protection: institutional observer (investigating legality, considering temporal/severity carve-outs)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.86).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.79).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive Liability Shield (Expansive Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology/legal/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, '9ae64f59-3c36-4785-9a5c-70b28b6e4dcb').
narrative_ontology:cs_kernel_codification('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', fixed_text).
narrative_ontology:cs_authority_grounding('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', extraction).
narrative_ontology:cs_interpretation_layer_present('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb').
narrative_ontology:cs_reading_relation('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_reading_relation('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', beta_designation_doctrine__severity_carve_out_reading, influences).
narrative_ontology:cs_axiom('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', foundational, beta_label_sufficient_liability_shield).
narrative_ontology:cs_axiom_status(beta_label_sufficient_liability_shield, holdable).
narrative_ontology:cs_axiom_grounding('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', beta_label_sufficient_liability_shield, conventional).
narrative_ontology:cs_axiom('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', secondary, indefinite_testing_phase_permissible).
narrative_ontology:cs_axiom_status(indefinite_testing_phase_permissible, holdable).
narrative_ontology:cs_axiom_grounding('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', indefinite_testing_phase_permissible, instrumental).
narrative_ontology:cs_reference_frame('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', contractual_liability_freedom).
narrative_ontology:cs_drift_state('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', contemporary_ecosystem_dependency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9ae64f59-3c36-4785-9a5c-70b28b6e4dcb', '2026-06-12T14:32:17Z').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, platform_operators).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, beta_software_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, dependent_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, end_users_harmed_by_cascade).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declare software 'beta' and externalize all defect costs to users indefinitely, across any context (consumer, enterprise, critical infrastructure). They set terms unilaterally and defend the designation linguistically without temporal or severity boundaries. Collect the network effects and user data from the released software while bearing no product liability risk.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_developers, agenda_setter,
    organized, generational, arbitrage, global).

% Download and run beta-labeled software, often unaware that 'beta' means comprehensive liability waiver rather than time-bounded testing. Bear all costs of defects: data loss, system corruption, financial loss, privacy breach. Cannot exit without abandoning the platform ecosystem or functionality entirely. Identity is increasingly locked to software-centric digital identity and service access.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, beta_software_users, payer,
    powerless, immediate, identity_locked, global).

% Adopt beta-designated software for operational functions (health records, financial systems, supply chain) and discover too late that 'beta' shields developers from liability even for critical failures. They absorb defect costs (operational downtime, data loss, regulatory violations) while developers remain protected. Exit is costly due to integration lock-in and ecosystem dependency.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, dependent_institutions, payer,
    moderate, biographical, constrained, regional).

% Suffer harm indirectly through institutional use of beta software: medical errors from corrupt health data, financial fraud from compromised payment systems, emergency delays from failed communication infrastructure. They have no contractual relationship with developers and no standing to sue despite bearing the concrete harm. Trapped by institutional adoption decisions.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, end_users_harmed_by_cascade, payer,
    powerless, immediate, trapped, global).

% The common-law and statutory frameworks that would ordinarily hold manufacturers liable for defective goods. This reading (expansive shield) forecloses those frameworks via linguistic/contractual override, privileging developer language choice over structural product safety regimes.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, product_liability_doctrine_priors, observer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(beta_designation_doctrine__expansive_shield_reading, product_liability_doctrine_priors).

% Argue that 'beta' should mean time-bounded testing disclosure with preserved product liability. They are excluded from the expansive shield's authority structure — their position is treated as frivolous ('beta means beta') despite commanding significant regulatory and consumer-protection support in some jurisdictions.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, narrow_warning_advocates, excluded,
    organized, biographical, constrained, national).

% Investigate whether indefinite beta designation in critical contexts violates consumer protection or product liability law. They face legal questions about whether contractual language can prospectively waive statutory duties and whether categorical carve-outs for safety-critical systems exist or should be created.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, regulators_and_consumer_protection, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, software_developers).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces developer friction for releasing early iterations and gathering user feedback by centralizing liability risk on users. In the narrow reading this coordinates genuine testing phases; in this expansive reading it coordinates perpetual externality dumping.
% TRANSFER_FUNCTION: Transfers all defect-cost bearing from developers (internalized under product liability) to users and dependent institutions (externalized). Also transfers network effects and data benefits from beta users to developers without corresponding liability or compensation.
% ABSENT_VOICES: End users harmed through institutional cascade (medical patients, financial customers, infrastructure users) have no contractual voice and are structurally excluded from the warranty/liability waiver negotiation. They appear as zero-stake casualties in the developer-user dyad.
% DISAPPEARANCE_RATIONALE: If the expansive beta-waiver doctrine disappeared, product liability would revert to developers for released software regardless of labeling; release cycles and user-acceptance timelines would shift; critical-systems adoption of untested software would face immediate legal exposure and insurance cost; the asymmetric cost transfer would reverse, forcing developers to internalize or insure defect costs.
% FOUNDING_PROBLEM: Early software development faced high friction: testing required formal QA infrastructure, liability exposure delayed releases, and developer risk prevented rapid iteration with user feedback. The beta label emerged as linguistic shorthand to signal 'incompleteness' without formal testing commitment, reducing legal friction.
% FOUNDING_PROBLEM_CORROBORATION: Developer industry and platform operators attest the founding problem remains live and beta flexibility is necessary for rapid innovation. Regulators, insurance companies, and consumer advocates attest the founding problem (friction on developers) is outweighed by the harms from indefinite, catastrophic-context liability waiver and that the remedy is outdated. Academic research on information asymmetry in software disclaimers documents user incomprehension of waiver scope.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.86, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high and rising because the waiver's scope has expanded de facto: 'beta' now shelters decades-old software and mission-critical systems, not genuinely testing iterations. Suppression is high (0.79) because users cannot exit without abandoning functional ecosystems — identity lock is internalized. Theater is substantial and rising (0.67 at interval end) because enforcement rhetoric continues to invoke 'testing phase' and 'user feedback' rationales while actual practice perpetuates indefinite liability externality. The measurement series show monotonic increase in extraction and theater with plateau by year 30, indicating the constraint has reached its exploitative steady state. Suppression stays high throughout because identity lock to software ecosystems is structural and does not decay. The rise in theater (year 0: 0.45, year 35: 0.67) is the diagnostic signature of a snare that began with genuine coordination benefits (early testing accommodation) but whose function has atrophied while the extraction mechanism has hardened.
 *
 * PERSPECTIVAL GAP:
 *   From the developer/agenda-setter seat: the constraint is a necessary friction-reduction mechanism enabling rapid iteration and user participation in software development. From the powerless-user seat: the constraint is undisclosed liability externality with internalized suppression (they don't realize they're trapped because 'beta' sounds temporary). From the institutional-adopter seat: the constraint is latent catastrophic risk — adopting beta software for critical operations exposes the institution to cascade failure liability the institution cannot contractually reverse onto developers (the developer's waiver does not bind the institution's downstream customers). From the regulator seat: the constraint violates product liability and consumer-protection doctrine by allowing indefinite waiver of statutory duties. These divergences are NOT resolved by authored claim — they are computed from the structural data (power, exit, beneficiary/victim roles, time horizon) and emerge as per-seat classification divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers (organized, institutional, arbitrage exit) are near-pure beneficiaries (d ≈ 0.15): they set terms, externalize costs, collect network effects and data benefits. Beta-software users (powerless, identity-locked, no arbitrage) are near-pure targets (d ≈ 0.95): they bear all defect costs and cannot exit without abandoning functionality. Dependent institutions (moderate power, constrained exit) are targets (d ≈ 0.75): they absorb cascade failures and institutional liability but are locked by adoption decisions and cannot reverse the waiver onto developers. End-users harmed through cascade (powerless, trapped, no contractual relationship) have no directionality in the two-party framework but are mathematically victims (d → 1.0 if relationship were modeled). Regulators are analytical observers (d = 0.5). The structural asymmetry — beneficiary with arbitrage exit vs. victim with identity lock — ensures persistent extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('developer friction in early iterations') is DEAD: modern software is released as perpetually-beta iterations (Google Chrome versioning, continuous deployment, CI/CD pipelines). The beta label persists not to solve the founding problem but to maintain the liability waiver. Theater ratio (0.67) captures this atrophy: most enforcement activity defends waiver scope rather than enabling genuine testing. The constraint meets the diagnostic profile of mandatrophy: the function it was built for (coordinating testing phases) has disappeared, but the extraction apparatus persists due to institutional inertia and beneficiary interest. Misclassifying this as rope (genuine coordination) would hide the dead-mandate problem and the extraction that now constitutes the sole remaining function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the beta designation kernel is structurally correct: expansive shield (this reading), narrow warning, or severity carve-out?',
    'Judicial resolution (product liability case establishing whether ''beta'' can waive liability for critical systems) or legislative codification (statute establishing temporal bounds or severity carve-outs for beta designation). Different readings entail different victim sets and extraction profiles.',
    'If narrow_warning or severity_carve_out prevails, this reading forecloses and the constraint reclassifies to tangled_rope or rope. If expansive_shield prevails in law but narrow_warning dominates in practice, theater rises further and the constraint becomes piton-inflected.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the beta designation kernel prevails in authoritative interpretation.').

omega_variable(
    indefinite_duration_ambiguity,
    'Is ''beta'' status genuinely permissible indefinitely, or do common-law product liability and consumer protection doctrines impose implicit temporal bounds regardless of developer labeling?',
    'Regulatory action or judicial precedent establishing that indefinite beta status violates consumer protection (e.g., GDPR, CCPA right to security; UCC implied warranties; state consumer-fraud statutes). Post-judgment analysis of whether courts enforce temporal carve-outs despite contractual language.',
    'If temporal bounds are imposed, the constraint is weakened structurally — suppression becomes harder to maintain against legal override, and extraction falls. If indefinite duration is upheld, the constraint persists at current severity and theater rises as the ''testing'' framing becomes theatricalized for decades-old software.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_duration_ambiguity, empirical, 'Whether indefinite beta designation is legally permissible or subject to implicit temporal constraint.').

omega_variable(
    critical_systems_carve_out_necessity,
    'Do safety-critical or failure-critical systems (medical devices, financial infrastructure, aviation, emergency response) require categorical exemption from beta-designation liability waiver, or can developers indefinitely label such systems as ''beta'' and remain shielded?',
    'Regulatory action (FDA, SEC, FAA, NIST establish mandatory product liability for critical systems); demonstrated harm from beta-software cascade failure (hospital downtime from corrupted health records, financial fraud from beta payment system, emergency delay from beta infrastructure) triggering legislative response.',
    'If carve-out is enacted, the constraint splits: critical systems revert to product liability (new constraint: severity_carve_out_reading), non-critical beta remains shielded (expansive_shield persists narrower). If no carve-out emerges despite cascade failures, theater rises and resistance increases as the constraint becomes transparently an extraction mechanism rather than a testing accommodation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_systems_carve_out_necessity, empirical, 'Whether safety-critical systems are categorically exempted from beta liability waivers.').

omega_variable(
    user_comprehension_and_identity_lock,
    'How many beta software users understand that ''beta'' constitutes comprehensive liability waiver vs. time-bounded testing disclosure? To what extent does identity lock to software ecosystems (''I need this app, I have no alternative for this function'') suppress exit even if users understand the waiver?',
    'User comprehension research (surveys of beta-app users showing what ''beta'' means to them; gap analysis vs. legal waiver scope). Exit-option analysis of locked ecosystems (how many users uninstall after defect vs. repair/adapt given alternatives). Post-notice behavior (does disclosure of waiver scope change adoption rates, or does identity lock override individual choice).',
    'High user incomprehension + high identity lock = suppression is internalized, persists after barrier removal, and the constraint becomes more extractive than structural measures alone suggest. Suppression shifts from structural (can''t exit) to internalized (don''t realize they could/should). This feeds identity-lock anxiety omega and reframes the theater ratio upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_comprehension_and_identity_lock, empirical, 'Degree of user comprehension of beta waiver scope and interaction with ecosystem identity lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(beta_tr_t0, observed).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement_basis(beta_tr_t5, observed).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement_basis(beta_tr_t10, observed).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 15, 0.62).
narrative_ontology:measurement_basis(beta_tr_t15, observed).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement_basis(beta_tr_t20, observed).
narrative_ontology:measurement(beta_tr_t25, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 25, 0.66).
narrative_ontology:measurement_basis(beta_tr_t25, observed).
narrative_ontology:measurement(beta_tr_t30, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 30, 0.67).
narrative_ontology:measurement_basis(beta_tr_t30, observed).
narrative_ontology:measurement(beta_tr_t35, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 35, 0.67).
narrative_ontology:measurement_basis(beta_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(beta_be_t0, observed).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement_basis(beta_be_t5, observed).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement_basis(beta_be_t10, observed).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(beta_be_t15, observed).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement_basis(beta_be_t20, observed).
narrative_ontology:measurement(beta_be_t25, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 25, 0.85).
narrative_ontology:measurement_basis(beta_be_t25, observed).
narrative_ontology:measurement(beta_be_t30, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 30, 0.86).
narrative_ontology:measurement_basis(beta_be_t30, observed).
narrative_ontology:measurement(beta_be_t35, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 35, 0.86).
narrative_ontology:measurement_basis(beta_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(beta_su_t0, observed).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(beta_su_t5, observed).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(beta_su_t10, observed).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement_basis(beta_su_t15, observed).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(beta_su_t20, observed).
narrative_ontology:measurement(beta_su_t25, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(beta_su_t25, observed).
narrative_ontology:measurement(beta_su_t30, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(beta_su_t30, observed).
narrative_ontology:measurement(beta_su_t35, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 35, 0.79).
narrative_ontology:measurement_basis(beta_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__expansive_shield_reading, 0.25).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories instantiate three readings of the beta-designation kernel. Expansive-shield (this story) holds that 'beta' is an indefinite, universal liability waiver. Narrow-warning holds it is time-bounded testing disclosure. Severity-carve-out holds it is categorically unavailable for critical systems. Each reading has its own ε, victim set, and structural profile. The three readings are linked via network edges: expansive-shield influences both siblings (if expansive prevails, narrow becomes minimal reform and severity becomes carve-out narrowing; if narrow prevails, expansive forecloses). Decomposition follows the ε-invariance principle: different readings produce different constraint referents and victim structures; a single constraint cannot hold all three readings without conflating observable-dependent classification (DP-001 violation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
