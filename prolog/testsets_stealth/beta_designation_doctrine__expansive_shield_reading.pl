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
 *   human_readable: Beta Designation as Comprehensive Liability Waiver (Expansive Shield Reading)
 *   domain: technology law/software liability/consumer protection
 *
 * SUMMARY:
 *   Under the expansive shield reading, a vendor's beta designation operates
 *   as a comprehensive liability waiver: designating software beta disclaims
 *   all defect liability — data loss, security compromise, financial harm,
 *   physical harm — for as long as the designation persists, in any context
 *   of use, with no temporal or severity boundary. The arrangement is
 *   instantiated through click-through license terms, binding arbitration
 *   clauses, and class-action waivers, and it is maintained by the vendor's
 *   unilateral control of the label: a product remains beta exactly as long
 *   as the waiver is worth more than the label's signaling cost. The
 *   coordination story — real-world testing in exchange for early access —
 *   still exists at the margin, but under this reading it has no boundary
 *   that would limit the transfer: testing phases never formally end, and the
 *   waiver follows the software into hospitals, banks, and vehicles. KEY
 *   AGENTS (by structural relationship): software_vendors_publishers — agenda
 *   setter (institutional/arbitrage), drafts and enforces the waiver,
 *   collects the avoided liability; end_users — primary target
 *   (powerless/trapped), bears defect costs under non-negotiable terms;
 *   early_adopter_testers — incidental beneficiary and payer
 *   (moderate/constrained); enterprise_licensees — secondary target
 *   (powerful/constrained); critical_system_operators — catastrophic-loss
 *   target (institutional/trapped); involuntary_third_parties — excluded
 *   non-party (powerless/trapped); consumer_protection_regulators —
 *   analytical observer (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.82).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.78).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive Liability Waiver (Expansive Shield Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology law/software liability/consumer protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, 'bb1b1825-97af-4626-a6af-aa4d52e4de21').
narrative_ontology:cs_kernel_codification('bb1b1825-97af-4626-a6af-aa4d52e4de21', formalized).
narrative_ontology:cs_authority_grounding('bb1b1825-97af-4626-a6af-aa4d52e4de21', extraction).
narrative_ontology:cs_interpretation_layer_present('bb1b1825-97af-4626-a6af-aa4d52e4de21').
narrative_ontology:cs_reading_relation('bb1b1825-97af-4626-a6af-aa4d52e4de21', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb1b1825-97af-4626-a6af-aa4d52e4de21', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('bb1b1825-97af-4626-a6af-aa4d52e4de21', foundational, beta_disclosure_constitutes_informed_risk_assumption).
narrative_ontology:cs_axiom_status(beta_disclosure_constitutes_informed_risk_assumption, holdable).
narrative_ontology:cs_axiom_grounding('bb1b1825-97af-4626-a6af-aa4d52e4de21', beta_disclosure_constitutes_informed_risk_assumption, conventional).
narrative_ontology:cs_axiom('bb1b1825-97af-4626-a6af-aa4d52e4de21', foundational, contracting_parties_may_allocate_all_defect_risk).
narrative_ontology:cs_axiom_status(contracting_parties_may_allocate_all_defect_risk, holdable).
narrative_ontology:cs_axiom_grounding('bb1b1825-97af-4626-a6af-aa4d52e4de21', contracting_parties_may_allocate_all_defect_risk, conventional).
narrative_ontology:cs_axiom('bb1b1825-97af-4626-a6af-aa4d52e4de21', secondary, comprehensive_waivers_maximize_testing_and_innovation).
narrative_ontology:cs_axiom_status(comprehensive_waivers_maximize_testing_and_innovation, holdable).
narrative_ontology:cs_axiom_grounding('bb1b1825-97af-4626-a6af-aa4d52e4de21', comprehensive_waivers_maximize_testing_and_innovation, instrumental).
narrative_ontology:cs_reference_frame('bb1b1825-97af-4626-a6af-aa4d52e4de21', contractual_risk_allocation_autonomy).
narrative_ontology:cs_drift_state('bb1b1825-97af-4626-a6af-aa4d52e4de21', contemporary_consumer_protection_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('bb1b1825-97af-4626-a6af-aa4d52e4de21', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_vendors_publishers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, early_adopter_testers).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, end_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, enterprise_licensees).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, critical_system_operators).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, involuntary_third_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, early_adopter_testers).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__expansive_shield_reading, contractual_risk_allocation_doctrine).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__expansive_shield_reading, as_is_disclaimer_enforceability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the license terms that define what the beta label disclaims, chooses which products carry the label and for how long, and enforces the waiver through click-through acceptance, arbitration clauses, and class-action waivers. Collects the avoided liability directly: every defect cost the waiver shifts is a cost off its balance sheet. Its exit is easy in the relevant sense — it can relabel a product, restructure its terms, re-incorporate, or select favorable governing law; the constraint binds its customers, not it.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_vendors_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Accepts non-negotiable license terms to obtain software they need for work, communication, or daily life, and bears data loss, security compromise, and functional failure with no recourse beyond individual arbitration. No alternative terms exist: competing products carry equivalent disclaimers, and the products with the strongest network effects are distributed only under beta or as-is terms.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, end_users, payer,
    powerless, biographical, trapped, global).

% Receives early access to new capability and a channel of influence over the product in exchange for accepting defect risk, and genuinely wants that exchange at the margin. Also bears uncompensated losses when defects destroy data or compromise accounts, and cannot negotiate the waiver's scope — the terms arrive fixed.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, early_adopter_testers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, early_adopter_testers, payer).

% Deploys beta-labeled or as-is software in production because vendor roadmaps leave no fully supported alternative; negotiates indemnification on large contracts but rarely escapes the core defect disclaimer, and absorbs operational losses, outage costs, and security incidents internally. Exit means re-platforming entire workflows — possible in principle, prohibitive in practice for embedded systems.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, enterprise_licensees, payer,
    powerful, biographical, constrained, global).

% Runs beta-labeled software in hospitals, banks, utilities, and transport under the all-contexts rule, bearing life-safety and financial-catastrophe exposure that no insurance market fully prices because the waiver makes vendor liability unenforceable. Locked into vendor ecosystems by certification and integration costs, and unable to obtain a non-beta version of systems the vendor has chosen never to graduate.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, critical_system_operators, payer,
    institutional, generational, trapped, global).

% Bears the risks of beta software without any contractual relationship at all — pedestrians near vehicles running beta autonomy stacks, patients downstream of beta clinical decision support, counterparties to financial transactions routed through beta infrastructure. Never accepted any terms, has no privity, and would object to the all-contexts rule but has no seat in the licensing conversation.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, involuntary_third_parties, excluded,
    powerless, immediate, trapped, global).

% Investigates whether comprehensive beta disclaimers are unfair terms, brings enforcement actions, and in some jurisdictions voids them. Observes the arrangement from outside and can alter its enforceability through rulemaking, but its reach is jurisdictional while the terms are engineered globally.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_protection_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, software_vendors_publishers).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels pre-release software into real-world use: vendors obtain scaled usage data and defect discovery before general release, and some users obtain early access to new capability in exchange for tolerating defects. The beta label marks which software sits inside that exchange.
% TRANSFER_FUNCTION: Moves the entire cost of software defects — data loss, security compromise, financial loss, and physical harm — from vendors to users, licensees, and non-consenting third parties, and moves the corresponding legal risk off vendor balance sheets; the consideration flowing back is early access and a feedback channel.
% ABSENT_VOICES: Involuntary third parties — pedestrians near vehicles running beta autonomy stacks, patients downstream of beta clinical decision support, counterparties to transactions routed through beta infrastructure — bear the all-contexts rule's catastrophic tail and have no seat anywhere in the licensing conversation. Consumer-side drafters are absent by construction: terms are unilaterally drafted and the agreement is a non-negotiable click. Regulators enter only after harm, jurisdiction by jurisdiction.
% DISAPPEARANCE_RATIONALE: Vendors would internalize defect costs overnight: prices and release practices would adjust, insurance markets for software defects would form, beta labeling would revert to a bounded testing disclosure or disappear, and products now parked in perpetual beta would either graduate or carry priced warranties. The transfer the arrangement performs — shifting all defect costs to users and third parties — is the arrangement.
% FOUNDING_PROBLEM: Early commercial software distribution needed a way to ship imperfect software for real-world testing: vendors feared liability for inevitable pre-release defects and users wanted early access, and the beta disclaimer emerged as the disclosure that balanced the two during a bounded testing phase.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: court opinions declining to enforce beta disclaimers on products years past any genuine testing phase (finding the label fictional as to duration), consumer-protection agency findings treating perpetual beta as a commercial practice rather than a testing phase, and software-engineering literature documenting perpetual-beta release models. No source outside the vendor set attests that an indefinite, all-contexts waiver continues to serve a live testing-disclosure function; the external corroboration uniformly attests the founding problem is dead while the arrangement persists.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.82) because under this reading the waiver is comprehensive: every class of defect cost, including catastrophic life-safety and financial loss, lands on users and third parties, and the waiver's value to the vendor scales with exactly the harms it shifts. Suppression (0.78) is authored as a raw structural property — it is not scaled by power or scope in the engine's computation; only extractiveness is. The suppression here is structural rather than internalized: terms are unilaterally drafted and non-negotiable, needed software is distributed only under as-is terms, competing products carry equivalent disclaimers, and arbitration clauses paired with class-action waivers close the judicial exit — the class waiver is specifically the mechanism that suppresses the coalition power powerless users would otherwise hold. Theater_ratio (0.55): the feedback-exchange function is real at the margin, but under indefinite duration the label outlives any testing phase — products run as de facto production systems for years while retaining the label, so a majority of the label's operational work is liability positioning rather than testing coordination. Accessibility_collapse (0.5): alternatives do not fully collapse — competing and open-source products exist — but for products with network effects or unique function there is no non-beta version to switch to, so understanding the waiver does not open an exit. Resistance (0.55): unconscionability doctrine, consumer-protection enforcement, and EU unfair-terms law actively push back, but enforcement is jurisdictional while the terms are engineered globally. The measurement series run on one shared time grid (1995/2002/2009/2016/2021/2026) with every tracked metric authored at every point; the suppression_requirement series is authored because this story specifically tracks enforcement-capacity buildup — shrink-wrap to click-wrap enforceability, then the arbitration-plus-class-waiver architecture of the 2010s — not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the vendor seat the arrangement is voluntary exchange: users accept beta terms knowingly in exchange for early access, risk allocation is priced into the product, and the label is honest disclosure. From the trapped end-user and involuntary third-party seats there is no bargaining, no alternative terms, and no forum — consent is a click, and for third parties there is not even a click. Same-level dynamics differentiate the payer seats further: enterprise licensees hold nominal market power yet remain constrained because vendor roadmaps leave no fully supported alternative and re-platforming embedded workflows is prohibitive; critical-system operators hold institutional power yet are trapped by certification and integration lock-in; end users hold neither power nor exit. The engine computes per-seat classifications from this structural data; the divergence between the vendor's coordination story and the payers' extraction experience is the quantity the corpus exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   software_vendors_publishers is the structural beneficiary with arbitrage-grade exit — it can relabel, restructure terms, re-incorporate, and select governing law — so its directionality sits near the beneficiary end and its effective extraction damps toward subsidy. end_users, critical_system_operators, and involuntary_third_parties sit near the full-target end: trapped exit (or no contractual seat at all) under the full defect-cost transfer. enterprise_licensees are powerful but constrained — negotiated indemnification damps their directionality below the trapped users' but nowhere near symmetry. early_adopter_testers are the override case: the derivation chain would read the declared beneficiary status plus constrained exit as damped directionality near 0.25, but their actual position is near-symmetric — the early access they receive is worth less than the uncompensated defect losses they bear on fixed terms — so a directionality override moves moderate to 0.48. Moderate is uniquely held by this agent in the story, so the override is precise rather than coarse.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Reading the arrangement as pure coordination — the vendor's own story, that beta is a testing disclosure users accept — would miss the asymmetric transfer: with no temporal or severity boundary, the testing function the waiver nominally serves cannot limit the extraction, because a testing phase that never ends is not a boundary. Reading it as extraction with no coordination function would miss the residual real exchange: early-access feedback is genuine at the margin and some participants net-benefit. The snare claim captures the structure the metrics describe: a coordination story functioning as cover, persistence depending on actively enforced adhesion terms rather than participant preference, and an identifiable victim set that includes parties who never contracted at all. On the genealogy: the founding problem — disclosing genuine pre-release testing — is dead under this reading, since the label persists on production systems; what is maintained is the waiver, not the disclosure. The dead founding problem combined with a world_rearranges disappearance verdict is the capture signature, and it is consistent with the computed snare path rather than a piton one, because a concentrated seat (the vendor) both captures the gains and actively maintains the enforcement machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary_contest,
    'This constraint is the expansive_shield_reading of the beta_designation_doctrine kernel; the irreducible structural question is which boundary rule governs the beta waiver — comprehensive and indefinite (this reading), time-bounded to a genuine testing phase (narrow_warning_reading), or categorically unavailable for critical systems (severity_carve_out_reading)? The disagreement between readings is located exactly at the waiver''s duration and severity boundaries.',
    'Doctrinal adjudication or legislation fixing the waiver''s duration and severity boundaries; the sibling stories model the alternative boundary rules and their divergent epsilon values.',
    'Under narrow_warning_reading the victim set regains recourse after any genuine testing phase and epsilon drops sharply; under severity_carve_out_reading life-safety and financial harm leave the transfer entirely. Both siblings shrink this constraint''s victim set and move its classification toward tangled_rope or rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary_contest, conceptual, 'Committer structure: one reading of the beta_designation_doctrine kernel; the contest lives at the waiver''s duration and severity boundaries.').

omega_variable(
    click_through_consent_validity,
    'Does click-through acceptance of a comprehensive, indefinite beta waiver constitute assent capable of bearing the full liability transfer, or is it an unconscionable adhesion term that fails reasonable-expectations review?',
    'Unconscionability and reasonable-expectations outcomes across jurisdictions; unfair-terms rulings by consumer-protection authorities.',
    'If assent fails, the waiver loses enforcement and the constraint degrades toward an inertial label (piton drift with high theater_ratio); if it holds, the snare structure is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(click_through_consent_validity, empirical, 'Whether the consent mechanism can bear the comprehensive waiver''s legal weight.').

omega_variable(
    genuine_testing_vs_production_use,
    'What fraction of beta-labeled deployments are genuine testing-phase use versus de facto production use running under a retained label?',
    'Deployment telemetry, vendor release histories, and industry surveys of beta-label duration and usage patterns.',
    'Drives theater_ratio: if most labeled use is production, the testing function is cover and theater drift accelerates; if testing is substantial, part of the measured extraction is the price of real coordination and the tangled_rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_vs_production_use, empirical, 'The functional-versus-performative split of beta-label activity.').

omega_variable(
    third_party_waiver_reach,
    'Does the all-contexts rule actually reach harms to non-consenting third parties, or do privity limits and tort duties keep them outside the waiver''s coverage?',
    'Case law on disclaimers asserted against third parties — privity limits, tort preemption of contractual waivers for physical harm, and indemnification structures that route third-party claims back to vendors.',
    'If privity blocks third-party application, involuntary_third_parties exit the victim set and epsilon falls materially; if waiver-plus-indemnification structures reach them, the catastrophic-harm transfer stands and extraction stays high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_waiver_reach, empirical, 'Whether the waiver''s scope extends to the non-consenting catastrophic-loss tail.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t1995, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(beta_tr_t2002, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(beta_tr_t2009, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2009, 0.42).
narrative_ontology:measurement(beta_tr_t2016, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2016, 0.5).
narrative_ontology:measurement(beta_tr_t2021, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2021, 0.53).
narrative_ontology:measurement(beta_tr_t2026, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2026, 0.55).

% Extraction over time
narrative_ontology:measurement(beta_be_t1995, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(beta_be_t2002, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2002, 0.55).
narrative_ontology:measurement(beta_be_t2009, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2009, 0.64).
narrative_ontology:measurement(beta_be_t2016, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2016, 0.73).
narrative_ontology:measurement(beta_be_t2021, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2021, 0.78).
narrative_ontology:measurement(beta_be_t2026, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2026, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t1995, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(beta_su_t2002, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2002, 0.52).
narrative_ontology:measurement(beta_su_t2009, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2009, 0.62).
narrative_ontology:measurement(beta_su_t2016, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2016, 0.72).
narrative_ontology:measurement(beta_su_t2021, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2021, 0.76).
narrative_ontology:measurement(beta_su_t2026, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2026, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, information_standard).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'beta disclaimer' decomposes into three structurally distinct constraints over one kernel (beta_designation_doctrine): the expansive shield reading (this file — comprehensive waiver, indefinite duration, all contexts; high epsilon; victims include users and non-consenting third parties), the narrow_warning_reading (time-bounded testing disclosure preserving base product liability; bounded epsilon), and the severity_carve_out_reading (waiver categorically unavailable for critical systems; bounded epsilon). The readings differ at the waiver's boundaries — duration and severity scope — and that is where the epsilon divergence lives. The narrow_warning_reading is the upstream family member, closest to the doctrine's original testing-disclosure function; the expansive reading is the stretched downstream form that cites the same disclosure rationale to justify the unbounded transfer. Each file links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__expansive_shield_reading, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
