% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__narrow_warning_reading, []).

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
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation Doctrine — Narrow Warning Reading (Time-Bounded Testing Disclosure)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This story instantiates the narrow warning reading of the
 *   beta_designation_doctrine kernel: a beta designation is legally effective
 *   only as a time-bounded disclosure of genuine testing, base product
 *   liability survives outside the window, and the window's duration must
 *   track a real testing phase rather than a marketing choice. The epsilon
 *   referent is the standing arrangement under this reading — pre-release
 *   software governed by a bounded, honest-disclosure regime — assessed by
 *   this reading's own lights; the sibling readings' arrangements
 *   (comprehensive indefinite waiver; categorical critical-domain exclusion)
 *   are different constraints in different files, linked through
 *   network.affects_constraints. The colloquial concept 'the legal effect of
 *   a beta label' decomposes into three structurally distinct claims with
 *   different epsilon values, victim sets, and classifications; this file
 *   authors only the narrow reading. The claimed type and the authored
 *   metrics are independent facts: the reading's structure is transitional by
 *   construction — the shield it permits expires with the testing phase,
 *   which is why has_sunset_clause is constitutive rather than incidental —
 *   while the metrics describe how the arrangement actually operates across
 *   the interval.
 *
 * KEY AGENTS:
 *   - beta_software_end_users: Primary protected class (organized/mobile) — informed during the window, remedies preserved after it
 *   - genuine_testing_vendors: Secondary beneficiary (moderate/mobile) — receives a defensible testing interval in exchange for honest labeling
 *   - extended_beta_vendors: Primary cost-bearer (powerful/arbitrage) — loses the indefinite-shield strategy the label once carried
 *   - trial_courts: Agenda-setter (institutional/analytical) — administers the two-part temporal-genuineness inquiry
 *   - consumer_protection_agencies: Observer with enforcement margin (institutional/national)
 *   - e_and_o_insurers: Incidental beneficiary (institutional/arbitrage) — prices the predictability the doctrine creates
 *   - patient_safety_advocates: Excluded voice (organized/trapped) — the severity-carve-out constituency with no seat in doctrine formation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.22).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.4).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Doctrine — Narrow Warning Reading (Time-Bounded Testing Disclosure)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, 'd91f046f-cbac-48b1-adae-e24254c12c51').
narrative_ontology:cs_kernel_codification('d91f046f-cbac-48b1-adae-e24254c12c51', distributed).
narrative_ontology:cs_authority_grounding('d91f046f-cbac-48b1-adae-e24254c12c51', practice).
narrative_ontology:cs_interpretation_layer_present('d91f046f-cbac-48b1-adae-e24254c12c51').
narrative_ontology:cs_reading_relation('d91f046f-cbac-48b1-adae-e24254c12c51', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('d91f046f-cbac-48b1-adae-e24254c12c51', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('d91f046f-cbac-48b1-adae-e24254c12c51', foundational, beta_shield_expires_with_genuine_testing_phase).
narrative_ontology:cs_axiom_status(beta_shield_expires_with_genuine_testing_phase, holdable).
narrative_ontology:cs_axiom_grounding('d91f046f-cbac-48b1-adae-e24254c12c51', beta_shield_expires_with_genuine_testing_phase, conventional).
narrative_ontology:cs_axiom('d91f046f-cbac-48b1-adae-e24254c12c51', foundational, base_product_liability_preserved_outside_window).
narrative_ontology:cs_axiom_status(base_product_liability_preserved_outside_window, holdable).
narrative_ontology:cs_axiom_grounding('d91f046f-cbac-48b1-adae-e24254c12c51', base_product_liability_preserved_outside_window, deontological).
narrative_ontology:cs_reference_frame('d91f046f-cbac-48b1-adae-e24254c12c51', bounded_genuine_testing_disclosure).
narrative_ontology:cs_drift_state('d91f046f-cbac-48b1-adae-e24254c12c51', contemporary_preview_labeling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d91f046f-cbac-48b1-adae-e24254c12c51', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, beta_software_end_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, genuine_testing_vendors).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, extended_beta_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, e_and_o_insurers).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, beta_software_end_users).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, informed_consent_to_defect_risk).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, good_faith_dealing_in_standard_form_contracts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Install and use software explicitly labeled as pre-release. During the designated testing window they accept crashes, data loss, and missing features in exchange for early access, often at reduced price or free. When the window closes they expect the same remedies against defects as any other purchaser. Their practical exit is declining the beta and waiting for general release, which costs them access but little else.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_software_end_users, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, beta_software_end_users, payer).

% Ship incomplete software to willing external users to discover defects at scale before general release. The disclosed testing window lets them gather field data without carrying full product-liability exposure for every early crash, provided the window ends when the product is actually ready. Their alternative paths — internal QA, closed panels, staged rollouts — cost more per defect found.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, genuine_testing_vendors, beneficiary,
    moderate, biographical, mobile, global).

% Keep shipping the same product under pre-release labels long after functional completion, or rename completed products as 'previews' and 'labs' releases, to keep disclaimer protections alive. The doctrine's insistence that the window track a genuine testing phase forecloses this practice and exposes them to the liability the label was structured to avoid. Their counter-moves are relabeling, jurisdictional arbitrage, and contract drafting.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, extended_beta_vendors, payer,
    powerful, generational, arbitrage, global).

% Hear disputes over whether a beta disclaimer bars recovery. They must answer the two questions the doctrine puts to them: whether the designation lasted only as long as a real testing phase, and whether the harm at issue falls inside or outside the disclosed risks. Their rulings set precedent for other courts; they collect nothing from the outcome either way.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, trial_courts, agenda_setter,
    institutional, generational, analytical, national).

% Investigate whether pre-release labeling deceives consumers about product readiness, publish guidance on disclosure standards, and occasionally bring enforcement actions against vendors whose 'beta' products are sold at scale to buyers who did not understand what they were getting.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, consumer_protection_agencies, observer,
    institutional, generational, analytical, national).

% Price professional-liability coverage for software firms. A predictable, time-bounded disclosure window makes post-window liability insurable at calculable rates; an indefinite or contested window forces them to exclude beta-period claims or load premiums heavily. They reprice and reallocate portfolios as the doctrine's boundaries shift.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, e_and_o_insurers, beneficiary,
    institutional, biographical, arbitrage, continental).

% Organize around harms from software deployed in hospitals, aircraft, and financial infrastructure while labeled as testing. They argue no disclosure can make defective critical systems acceptable and seek a categorical exclusion for high-severity domains. They hold no formal seat in the doctrine's formation, which proceeds through contract disputes between vendors and commercial customers.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, patient_safety_advocates, excluded,
    organized, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__narrow_warning_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__narrow_warning_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables large-scale real-world testing by creating a bounded, disclosed risk-allocation window: users know they are testing, vendors get a defensible interval to find defects in the field, and both sides know when baseline liability resumes.
% TRANSFER_FUNCTION: During the genuine testing phase, shifts part of the cost of latent defects from vendor to informed users, who accept instability in exchange for early access; when the phase ends, remedy obligations revert to the vendor. Litigation risk also moves: onto vendors who stretch the label, away from those who respect its bounds.
% ABSENT_VOICES: Safety advocates for critical-system contexts — the severity-carve-out constituency — have no seat in the warning-reading framework; they would argue that no disclosure legitimizes beta deployment in life-safety or financial-infrastructure settings. Individually injured test-window users likewise lack organized representation in doctrine formation, which proceeds through commercial contract litigation between vendors and sophisticated customers.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished overnight, the underlying question would not: courts would fall back on contract enforcement of whatever waiver language vendors drafted, the expansive-shield approach would fill the vacuum by default, perpetual-beta labeling would proliferate as the rational strategy, and post-release user remedies would erode. The consumer-software liability landscape would reorganize around vendor-drafted boilerplate.
% FOUNDING_PROBLEM: Early web-era software distribution outran product-liability categories: vendors needed real-world testing at scale, users wanted early access, unbounded 'beta' labels threatened to swallow liability entirely, and unbounded liability threatened to chill all external testing. The narrow reading was built to permit genuine testing without letting the label become a permanent escape hatch.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: published appellate opinions refusing to enforce stale beta disclaimers (courts hold no stake in vendor labeling practices), law-review documentation of perpetual-beta practices compiled independently of vendor interests, and consumer-protection agency guidance treating pre-release labeling as a recurring deception vector. No beneficiary-side attestation is relied upon.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__narrow_warning_reading_tests).
:- end_tests(beta_designation_doctrine__narrow_warning_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.22 at interval end) because the transfer is confined to an informed, compensated, time-bounded window; the slow upward drift tracks vendors probing the boundary through relabeling rather than any change in the doctrine's text. Suppression (0.40) reflects active judicial enforcement: the reading survives only because courts actually inquire into duration and genuineness, and the suppression_requirement series is authored precisely because this story tracks enforcement-capacity change — early-era tolerance hardened into systematic scrutiny as perpetual-beta practices spread. Theater is low (0.15) but creeping: a growing minority of designated 'testing' phases are performative, which is the leading indicator of decay. Accessibility collapse is moderate-low (0.38) because workable alternatives persist — closed betas, paid testing panels, staged rollouts, simply finishing the product — and resistance (0.45) is sustained industry preference for the expansive reading expressed through lobbying and contract drafting. All three series share one time grid (t=0,4,8,12,16,20,24) with every metric authored at every point. Receipt surface: no named seat captures the arrangement's gains — preserved remedies scatter across the user class, the safe harbor accrues diffusely to honest shippers — so gain_flow is authored as an affirmative 'diffuse'; fixing_cost is 'prohibitive' because closing the relabeling arbitrage requires coordinated multi-jurisdiction doctrine-building against organized industry resistance, exceeding any single seat's capacity relative to the benefit.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats compute differently from the same doctrine. Extended_beta_vendors experience the reading as confiscation of a customary practice their contracts were built around; beta_software_end_users experience it as a floor under remedies that never quite reaches them during the window; trial_courts experience an administrable two-part inquiry that lets them dispose of cases without resolving the deeper question of what 'beta' means. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   End users sit near the beneficiary end but pulled toward symmetric (roughly d≈0.35–0.45): they collect preserved remedies and honest disclosure, yet bear real defect risk inside the window — the dual beneficiary/payer declaration encodes exactly this split. Genuine_testing_vendors sit low (d≈0.2): the window subsidizes their field testing relative to a no-doctrine baseline of full exposure. Extended_beta_vendors sit near the full-target end (d≈0.85): the reading's coercive force — refusal to enforce stretched disclaimers — falls squarely on them, and their arbitrage-grade exit (relabeling, forum shopping) moderates but does not reverse that position. Courts and agencies carry no economic directionality; insurers lean mildly beneficiary through priced predictability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — permitting genuine testing without letting the label swallow liability — remains live with each new distribution wave, so the arrangement is not mandatrophy-resolved and the flag is deliberately not set. The reading's internal sunset is functioning as designed: each product's shield expires with its testing phase, and the low, slowly-rising theater_ratio indicates maintenance is substantive rather than performative. The decay vector to watch is relabeling arbitrage: if courts stop looking behind 'preview' and 'labs' labels, the reading's protective function atrophies while its language persists — the classic path from working scaffold to inertial shell. The theater_ratio series is the early-warning instrument for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_underdetermination,
    'Is the narrow warning reading the correct instantiation of the beta_designation_doctrine kernel, or do the sibling readings (expansive_shield_reading, severity_carve_out_reading) better capture the doctrine''s operative content?',
    'Track doctrinal outcomes across jurisdictions: frequency of enforcement of stale or renamed beta disclaimers, appellate treatment of duration challenges, and whether courts adopt categorical domain exclusions for critical systems.',
    'If the expansive reading prevails, the arrangement becomes a comprehensive waiver regime with substantially higher extraction and a different victim set; if the severity carve-out prevails, the arrangement splits into domain-specific regimes. This story''s classification holds only under the narrow reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_underdetermination, conceptual, 'Which reading of the beta-designation kernel governs is unresolved; classification is reading-indexed.').

omega_variable(
    genuine_testing_demarcation,
    'Where is the line between a genuine testing phase and a theatrical or renamed one, and can courts reliably detect it?',
    'Feature-completeness audits, defect-discovery rates during the designated window versus after, and comparison of change logs across the labeling boundary.',
    'If designated phases are routinely theatrical, theater_ratio rises and the reading''s protective function decays toward performance; if demarcation is reliable, the reading holds as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_demarcation, empirical, 'Detectability of genuine versus performative testing phases.').

omega_variable(
    test_window_compensation_symmetry,
    'During the genuine testing window, does early-access value adequately compensate users for the defect risk they bear, making the transfer symmetric, or do test-window users bear uncompensated injury risk?',
    'Economic analysis comparing early-access pricing and discount value against actuarial defect-cost incidence in comparable windows.',
    'If uncompensated, the user seat''s directionality shifts toward target during the window and effective extraction rises above the authored base; if compensated, the current low-extraction profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(test_window_compensation_symmetry, empirical, 'Whether the intra-window risk transfer is symmetric or extractive.').

omega_variable(
    severity_context_leakage,
    'Does the narrow reading''s disclosure defense extend de facto into life-safety and financial-criticality contexts — the gap the severity_carve_out_reading targets — despite the reading contemplating ordinary consumer-facing software?',
    'Survey of litigated beta incidents by domain criticality; whether courts apply the time-bounded defense in medical, aviation, and financial-infrastructure deployments.',
    'If leakage is real, the narrow reading produces harmed classes its own frame does not acknowledge, strengthening the sibling carve-out reading''s claim and pressuring this reading''s beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(severity_context_leakage, preference, 'Domain-criticality coverage gap between the narrow reading and the carve-out sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.13).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 4, 0.15).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 12, 0.19).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 16, 0.2).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.21).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 24, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(beta_su_t24, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 24, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, information_standard).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial notion 'legal effect of a beta designation' decomposes into three structurally distinct readings of one kernel, each with its own epsilon, victim set, and classification. This story (narrow_warning_reading) is the reference frame from which the other two define themselves: expansive_shield_reading extends the shield indefinitely and comprehensively (upstream pressure from vendor contract practice cites it as the natural continuation), while severity_carve_out_reading responds to the narrow reading's residual critical-domain gap (downstream corrective). All three files cross-link through network.affects_constraints; no single story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
