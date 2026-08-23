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
    narrative_ontology:measurement_basis/2,
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
 *   domain: technology law / software liability / consumer protection
 *
 * SUMMARY:
 *   This story instantiates the narrow warning reading of the
 *   beta-designation kernel: a beta label is a time-bounded testing
 *   disclosure, base product liability survives outside the window, and the
 *   window must be a genuine testing phase. Under this reading the
 *   arrangement is a transitional device whose entire justification is the
 *   transition from development to stable release — hence scaffold, with the
 *   sunset definitional rather than decorative. The ε referent is the
 *   standing beta-designation arrangement as this reading assesses it by its
 *   own lights: bounded, disclosed, liability-preserving operation, which
 *   leaves only residual risk-transfer extractiveness. The sibling readings
 *   (expansive_shield_reading, severity_carve_out_reading) are separate
 *   constraints in separate files; per the ε-invariance principle this file
 *   does not hedge across them. KEY AGENTS (by structural relationship): -
 *   prerelease_software_publishers: agenda-setting beneficiary
 *   (institutional/arbitrage) — drafts terms, defines the window, collects
 *   the shield - early_adopter_test_users: dual-positioned risk-bearers
 *   (moderate/constrained) — supply testing labor, bear residual defect
 *   costs, gain early access - general_availability_customers: protected
 *   beneficiaries (organized/mobile) — buy under preserved liability -
 *   consumer_protection_agencies: enforcement seat (institutional/analytical)
 *   - beta_dispute_courts: adjudicative seat defining where the sunset line
 *   sits (institutional/analytical) - platform_api_dependents: excluded
 *   cost-bearers (powerful/trapped) — bear unpriced breakage risk with no
 *   seat
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.22).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.34).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Doctrine — Narrow Warning Reading (Time-Bounded Testing Disclosure)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology law / software liability / consumer protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, 'fd3fd43d-8190-4fad-92e7-c808df923a75').
narrative_ontology:cs_kernel_codification('fd3fd43d-8190-4fad-92e7-c808df923a75', distributed).
narrative_ontology:cs_authority_grounding('fd3fd43d-8190-4fad-92e7-c808df923a75', practice).
narrative_ontology:cs_interpretation_layer_present('fd3fd43d-8190-4fad-92e7-c808df923a75').
narrative_ontology:cs_reading_relation('fd3fd43d-8190-4fad-92e7-c808df923a75', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('fd3fd43d-8190-4fad-92e7-c808df923a75', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('fd3fd43d-8190-4fad-92e7-c808df923a75', foundational, shield_expires_with_genuine_testing_phase).
narrative_ontology:cs_axiom_status(shield_expires_with_genuine_testing_phase, holdable).
narrative_ontology:cs_axiom_grounding('fd3fd43d-8190-4fad-92e7-c808df923a75', shield_expires_with_genuine_testing_phase, conventional).
narrative_ontology:cs_axiom('fd3fd43d-8190-4fad-92e7-c808df923a75', foundational, base_product_liability_preserved_outside_window).
narrative_ontology:cs_axiom_status(base_product_liability_preserved_outside_window, holdable).
narrative_ontology:cs_axiom_grounding('fd3fd43d-8190-4fad-92e7-c808df923a75', base_product_liability_preserved_outside_window, conventional).
narrative_ontology:cs_reference_frame('fd3fd43d-8190-4fad-92e7-c808df923a75', bounded_testing_disclosure_baseline).
narrative_ontology:cs_drift_state('fd3fd43d-8190-4fad-92e7-c808df923a75', contemporary_perpetual_beta_aftermath, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd3fd43d-8190-4fad-92e7-c808df923a75', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, prerelease_software_publishers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, early_adopter_test_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, general_availability_customers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, early_adopter_test_users).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, real_world_testing_necessity_thesis).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, informed_consent_risk_allocation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the beta license terms, apply the designation, and define the testing window for their pre-release builds. During the window they are shielded from full product liability for defects inherent to unfinished software; they fund the arrangement with support load, telemetry infrastructure, and reputational exposure from public bugs. When the product reaches general availability the shield lapses and ordinary warranty and tort exposure resumes. They control when the window opens and closes.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, prerelease_software_publishers, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, prerelease_software_publishers, beneficiary).

% Opt into beta programs through click-through licenses, supplying the diverse real-world usage environments that internal testing cannot replicate. They receive early access, influence over feature direction, and usually free or discounted use; they absorb residual defect costs inside the window — crashes, data loss, instability — that the designation places on their side of the ledger. Declining the beta returns them to the ordinary consumer position but forfeits the access; waiting for general availability is a real but costly alternative.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, early_adopter_test_users, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, early_adopter_test_users, beneficiary).

% Purchase software after the testing window closes, under preserved base product liability. They benefit twice over: products arrive having been exercised by the test cohort, and their own purchases carry full warranty and tort protection because the narrow reading refuses to extend the shield past the window. They can switch vendors freely and are the constituency that punishes publishers who ship GA-quality failures.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, general_availability_customers, beneficiary,
    organized, biographical, mobile, global).

% Police the disclosure half of the arrangement: whether beta terms are presented comprehensibly, whether the designation conceals material defects beyond testing risk, and whether a 'beta' label is being used to dodge obligations on a product that is functionally finished. They bring enforcement actions under consumer statutes and set the practical standard for what a genuine testing disclosure looks like.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, consumer_protection_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicate whether a particular designation was a bona fide testing phase or a liability shelter: how long the window ran, whether the defect at issue was inherent to pre-release status or an ordinary product failure, and whether the disclosure actually reached the user's understanding. Their rulings define where the sunset line sits in practice.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_dispute_courts, agenda_setter,
    institutional, generational, analytical, national).

% Businesses that build production systems on interfaces the publisher has labeled beta. They sink integration cost into the beta surface, then bear breakage, deprecation, and migration costs when the interface changes or the window closes — risks they never negotiated, because the beta license is offered on a take-it-or-leave-it basis to the downstream developer community as a whole. Leaving means rewriting against a different platform at prohibitive cost.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, platform_api_dependents, excluded,
    powerful, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__narrow_warning_reading, prerelease_software_publishers).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__narrow_warning_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the risk-allocation problem of getting pre-release software into real-world conditions: developers need heterogeneous usage environments and fault discovery at scale before general availability, and willing users want early access. The bounded designation lets both transact by capping the publisher's tail liability to a genuine testing window while leaving ordinary liability intact everywhere else.
% TRANSFER_FUNCTION: Moves residual defect risk — crashes, data loss, instability, uncompensated bug consequences — from publishers to self-selected test users during the bounded window; moves early-access utility and feature influence back to those users; and, on expiry of the window, shifts failure costs back to publishers under preserved base liability.
% ABSENT_VOICES: Platform API dependents would object: they bear unpriced breakage risk from beta surfaces they depend on but had no seat in drafting the license. Also the click-through test users for whom 'informed' consent is nominal — the reading's legitimacy rests on comprehension, and those who never understood what they accepted were not effectively in the conversation.
% DISAPPEARANCE_RATIONALE: If the bounded designation vanished overnight, publishers would split three ways: delaying release until internal testing suffices (slower iteration, fewer real-world fault discoveries), shipping at general availability with full liability (chilling small-developer pre-release distribution), or adopting blanket disclaimers (leaving users worse off than the bounded regime). The current pattern of rapid public iteration with a defined liability boundary would reorganize around whichever of those imperfect substitutes each publisher could afford.
% FOUNDING_PROBLEM: Early commercial software carried catastrophic latent-defect exposure: developers faced potentially ruinous liability for bugs that are inevitable in unfinished software, while buyers had no way to distinguish a mature product from an experimental one. The beta designation emerged to mark experimental status publicly and to bound the developer's exposure to a genuine testing period.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: UCC and Magnuson-Moss warranty legislative history and the pre-doctrine case-law record on software warranty disclaimers document the liability-chilling problem the designation answered; the software engineering literature on field-testing necessity attests the underlying technical problem independently of any party's commercial interest. Publishers obviously attest the problem as well, but the external legal-historical and engineering sources carry the corroboration.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.22 at interval end) because the narrow reading preserves base liability outside the window and confines the shield to genuine testing risk; what remains is the irreducible residuum — even a consensual, disclosed risk trade leaves the test cohort bearing uncompensated defect costs, and the publisher captures the avoided-liability value. Suppression (0.34) is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled by the engine; the level reflects take-it-or-leave-it click-through licensing offset by the real alternative of waiting for general availability. Theater ratio (0.14) is low: post-correction, the disclosure function is mostly real, with a residual band of legacy labels maintained for brand familiarity rather than signal. Accessibility collapse (0.30) is low because alternatives stay open — decline the beta, wait for GA, switch vendors. Resistance (0.32) is moderate: consumer advocates and some courts actively resist stretched designations, which is what held the correction. The measurement series share one six-point grid (t=0,6,12,18,24,30) across all three tracked metrics. The trajectory is a rise-and-correct arc, not monotonic drift: through the middle of the interval the perpetual-beta phenomenon (multi-year 'beta' labels on finished products) pushed extractiveness, theater, and the enforcement burden up together; regulatory scrutiny and label retirements then pulled all three back down. The suppression_requirement series is authored deliberately because this story tracks enforcement-capacity change — the narrow reading's core demand is duration policing, and the historical record shows enforcement surging against window abuse and relaxing as norms re-settled.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the publisher seat the arrangement is earned flexibility the publisher pays for in support load, telemetry infrastructure, and public bug exposure — a tool it administers. From the test-user seat it is a priced risk trade whose price (residual defect costs) is real but consented to. From the GA-customer seat it is nearly invisible except as delivered product quality and intact warranty rights. From the excluded API-dependent seat the same designation reads as unpriced counterparty risk imposed without negotiation. The engine derives these divergent classifications from the structural data; the authored scaffold claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers sit near the beneficiary pole (listed beneficiary, arbitrage-grade exit — they control the window's length and can restructure terms). General-availability customers sit near the beneficiary pole with mobile exit. Test users are the story's one deliberate override: the derivation chain reads their presence in the beneficiaries array and would place them near-subsidized, but they are the receiving end of the transfer function — residual defect risk lands on them — so their true position is near-symmetric. The override (moderate -> 0.45) corrects this; the moderate power atom is held uniquely by the test-user cohort in this story, so the keying is unambiguous. Regulators and courts occupy analytical seats whose extraction exposure is negligible. API dependents bear real costs but hold an excluded seat: they appear in no beneficiaries/victims array, so the derivation underweights them — recorded here as commentary-grade structural residue, not a correction-grade override.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification does double work. Against the extraction reading: it records that the beta regime solves a genuine risk-allocation problem — labeling it a snare would erase the real coordination that makes pre-release software distribution possible at all. Against the complacency reading: the sunset is definitional under this reading, so any operation in which the window detaches from genuine testing is not this constraint operating well but this constraint failing into its expansive sibling. The founding problem remains live (pre-release software still exists and still needs bounded field testing), so mandatrophy is not resolved; the live risk is forward decay — if windows systematically outrun testing need, the arrangement persists past its function and drifts piton-ward or snare-ward. Omega genuine_testing_phase_boundary carries that monitoring question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the beta_designation_doctrine kernel; which reading will the adjudicating institutions consolidate, and what does each consolidation change structurally?',
    'Track doctrinal consolidation in appellate treatment of beta-dispute cases and in consumer-statute amendments: adoption of the expansive reading would convert the shield into an indefinite comprehensive waiver; adoption of the severity carve-out would remove critical-system domains from the arrangement entirely.',
    'Expansive consolidation raises epsilon sharply and converts the scaffold into a steady-state shield (snare-flavored); carve-out consolidation shrinks the arrangement''s domain without changing its internal structure; narrow consolidation stabilizes the scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the beta-designation kernel prevails, and the structural consequences of each.').

omega_variable(
    genuine_testing_phase_boundary,
    'What counts as a genuine testing phase, and are declared windows set in good faith relative to actual testing need?',
    'Audit release cadence against declared windows: compare bug-fix throughput, crash-rate stabilization, and feature-completion signals at window close against the publisher''s own telemetry; systematic excess of window length over demonstrated testing need indicates bad-faith duration.',
    'If windows systematically outrun genuine testing need, the sunset becomes decorative, the scaffold decays toward a steady-state shield, and the arrangement drifts piton- or snare-ward with rising theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_phase_boundary, empirical, 'Whether declared testing windows track genuine testing phases in good faith.').

omega_variable(
    informed_consent_adequacy,
    'Is click-through beta consent sufficiently informed for the risk transfer to count as consensual rather than extracted?',
    'Comprehension studies of beta license terms and differential opt-in behavior when material risk terms are plainly summarized at the point of consent.',
    'If consent is nominal for a substantial fraction of test users, their effective directionality rises toward the target pole, residual extractiveness exceeds the authored 0.22, and the arrangement shifts toward tangled_rope territory despite preserved base liability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_adequacy, empirical, 'Adequacy of the informed consent on which the reading''s legitimacy rests.').

omega_variable(
    perpetual_beta_reversion_risk,
    'Will the post-correction convergence toward bounded windows persist, or revert to perpetual-beta practice under competitive pressure?',
    'Longitudinal tracking of designation durations and window-renewal behavior across major platforms; watch for renewed multi-year ''beta'' labels on functionally complete products.',
    'Reversion reproduces the mid-interval peak — rising extractiveness, theater, and enforcement burden — and dates a scaffold-degradation transition; persistence confirms the correction as a durable norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetual_beta_reversion_risk, empirical, 'Durability of the correction against perpetual-beta relapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_narrow_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(beta_narrow_tr_t0, observed).
narrative_ontology:measurement(beta_narrow_tr_t6, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement_basis(beta_narrow_tr_t6, observed).
narrative_ontology:measurement(beta_narrow_tr_t12, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(beta_narrow_tr_t12, observed).
narrative_ontology:measurement(beta_narrow_tr_t18, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement_basis(beta_narrow_tr_t18, observed).
narrative_ontology:measurement(beta_narrow_tr_t24, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement_basis(beta_narrow_tr_t24, observed).
narrative_ontology:measurement(beta_narrow_tr_t30, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(beta_narrow_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(beta_narrow_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(beta_narrow_be_t0, observed).
narrative_ontology:measurement(beta_narrow_be_t6, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement_basis(beta_narrow_be_t6, observed).
narrative_ontology:measurement(beta_narrow_be_t12, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement_basis(beta_narrow_be_t12, observed).
narrative_ontology:measurement(beta_narrow_be_t18, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 18, 0.36).
narrative_ontology:measurement_basis(beta_narrow_be_t18, observed).
narrative_ontology:measurement(beta_narrow_be_t24, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 24, 0.29).
narrative_ontology:measurement_basis(beta_narrow_be_t24, observed).
narrative_ontology:measurement(beta_narrow_be_t30, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement_basis(beta_narrow_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(beta_narrow_su_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(beta_narrow_su_t0, observed).
narrative_ontology:measurement(beta_narrow_su_t6, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement_basis(beta_narrow_su_t6, observed).
narrative_ontology:measurement(beta_narrow_su_t12, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement_basis(beta_narrow_su_t12, observed).
narrative_ontology:measurement(beta_narrow_su_t18, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 18, 0.54).
narrative_ontology:measurement_basis(beta_narrow_su_t18, observed).
narrative_ontology:measurement(beta_narrow_su_t24, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement_basis(beta_narrow_su_t24, observed).
narrative_ontology:measurement(beta_narrow_su_t30, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement_basis(beta_narrow_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, resource_allocation).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial doctrine 'a beta designation shields software developers from liability' decomposes into three structurally distinct readings with different epsilon, different victim structures, and different temporal form. This file instantiates the narrow warning reading (time-bounded disclosure, preserved base liability, genuine-phase duration — scaffold-shaped, low extraction). The expansive_shield_reading (indefinite comprehensive waiver) is its contradictory sibling: the narrow reading's foundational axioms logically exclude it within any single framework. The severity_carve_out_reading (categorical exclusion of critical systems) is domain-limiting rather than contradictory and coexists with this reading as a live position held by other parties. Each reading is authored as its own story with its own stable epsilon; the family is linked through affects_constraints per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__narrow_warning_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
