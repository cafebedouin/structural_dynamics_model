% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Severity Carve-Out Reading of Beta Designation Doctrine
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   A software vendor deploys a feature labeled 'beta' inside a system that
 *   controls insulin dosing, executes trades, or manages emergency dispatch
 *   routing. When the feature fails and causes harm, the vendor invokes the
 *   beta label as grounds for reduced or waived liability, citing disclosed
 *   experimental status. This reading of the beta designation kernel holds
 *   that the categorical severity of the domain — life-safety, financial, or
 *   other critical-systems contexts — forecloses beta status as a
 *   liability-shifting mechanism entirely, regardless of how genuinely
 *   experimental the feature was or how clearly its beta status was
 *   disclosed. The rule operates as a coordination mechanism (it lets courts
 *   and regulators draw a predictable bright line without litigating
 *   disclosure adequacy case-by-case) fused to an extraction structure (it
 *   strips vendors of a risk-allocation tool they built commercial practice
 *   around, transferring the residual risk of software failure back onto them
 *   and, derivatively, onto insurers and investors who priced the beta shield
 *   into product risk).
 *
 * KEY AGENTS:
 *   - end_users_of_critical_systems: Primary beneficiary (powerless/trapped) — cannot negotiate around beta disclaimers embedded in click-through terms, benefits when courts refuse to enforce them in high-stakes domains
 *   - patients_relying_on_medical_software: Primary beneficiary (powerless/trapped) — subject to software they did not choose and cannot inspect, protected by categorical carve-out when harmed
 *   - software_vendors_deploying_beta_labeled_critical_features: Primary target (powerful/constrained) — loses a liability-shaping tool specifically in the domains where liability exposure is largest
 *   - fintech_startups_using_beta_gating: Secondary target (moderate/constrained) — smaller firms with thinner insurance reserves face disproportionate exposure when the carve-out applies
 *   - medical_device_software_developers: Secondary target (organized/constrained) — operate under FDA software-as-medical-device oversight that already narrows the space beta labeling could occupy
 *   - plaintiffs_bar_in_critical_systems_litigation: Secondary beneficiary (organized/mobile) — gains a durable, categorical argument that survives fact-intensive disclosure disputes
 *   - courts_and_regulators: Analytical observer (institutional/analytical) — administers and could in principle narrow or expand the carve-out through subsequent rulings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.28).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.35).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Severity Carve-Out Reading of Beta Designation Doctrine").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, 'a1648215-c183-4ffb-987b-3ec15aa8cd3f').
narrative_ontology:cs_kernel_codification('a1648215-c183-4ffb-987b-3ec15aa8cd3f', distributed).
narrative_ontology:cs_authority_grounding('a1648215-c183-4ffb-987b-3ec15aa8cd3f', distributed).
narrative_ontology:cs_reading_relation('a1648215-c183-4ffb-987b-3ec15aa8cd3f', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('a1648215-c183-4ffb-987b-3ec15aa8cd3f', beta_designation_doctrine__narrow_warning_reading, influences).
narrative_ontology:cs_axiom('a1648215-c183-4ffb-987b-3ec15aa8cd3f', foundational, harm_severity_categorically_overrides_contractual_disclaimer).
narrative_ontology:cs_axiom_status(harm_severity_categorically_overrides_contractual_disclaimer, holdable).
narrative_ontology:cs_axiom_grounding('a1648215-c183-4ffb-987b-3ec15aa8cd3f', harm_severity_categorically_overrides_contractual_disclaimer, deontological).
narrative_ontology:cs_axiom('a1648215-c183-4ffb-987b-3ec15aa8cd3f', secondary, domain_classification_dispositive_over_disclosure_quality).
narrative_ontology:cs_axiom_status(domain_classification_dispositive_over_disclosure_quality, holdable).
narrative_ontology:cs_axiom_grounding('a1648215-c183-4ffb-987b-3ec15aa8cd3f', domain_classification_dispositive_over_disclosure_quality, conventional).
narrative_ontology:cs_created_at('a1648215-c183-4ffb-987b-3ec15aa8cd3f', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, end_users_of_critical_systems).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, patients_relying_on_medical_software).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, banking_customers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, plaintiffs_bar_in_critical_systems_litigation).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_vendors_deploying_beta_labeled_critical_features).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, fintech_startups_using_beta_gating).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, medical_device_software_developers).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, safety_severity_overrides_contractual_disclaimer).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, categorical_domain_carve_out_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses software embedded in safety-relevant systems (vehicle control interfaces, emergency response tools, industrial controls) without visibility into which features are beta-labeled or meaningful ability to decline them. Benefits when courts refuse to let vendors invoke beta status to escape liability for harm caused by such features; has no negotiating position to secure this protection themselves and depends entirely on the doctrine being applied categorically.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, end_users_of_critical_systems, beneficiary,
    powerless, biographical, trapped, national).

% Depends on software controlling diagnostic, monitoring, or treatment-delivery functions, typically without any choice of vendor or software version. A beta-labeled dosing algorithm or diagnostic model failing causes direct physical harm; this stakeholder's only protection is a legal rule that refuses to let the beta label absorb the harm regardless of disclosure.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, patients_relying_on_medical_software, beneficiary,
    powerless, biographical, trapped, national).

% Uses financial platforms where beta-labeled features (new payment rails, automated trading tools, fraud-detection systems) can cause direct monetary loss on failure. Has some choice of financial institution but little visibility into which underlying features are beta and essentially no bargaining power over the terms; benefits from a categorical rule that does not require proving inadequate disclosure feature-by-feature.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, banking_customers, beneficiary,
    powerless, biographical, constrained, national).

% Builds beta labeling into commercial practice as a way to ship features faster while managing liability exposure, and had priced the beta shield's risk-transfer value into deployment decisions in high-stakes domains. Under this reading, that shield categorically fails once a court or regulator classifies the deployment domain as critical, regardless of how well the vendor tested the feature or disclosed its status — the vendor cannot cure this by improving disclosure, only by not deploying beta labels in critical domains at all or by accepting the liability.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_vendors_deploying_beta_labeled_critical_features, payer,
    powerful, generational, constrained, national).

% Smaller financial technology firms use staged beta rollouts to manage risk while iterating quickly, often with thinner capital reserves and insurance coverage than incumbent banks. When the severity carve-out applies to a beta-labeled payment or trading feature that fails, the resulting liability exposure is proportionally more threatening to their solvency than to a large incumbent, and they cannot simply exit financial services to avoid the rule.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, fintech_startups_using_beta_gating, payer,
    moderate, biographical, constrained, national).

% Operates under FDA software-as-medical-device regulatory oversight in parallel with general product liability exposure. Organized through industry associations that lobby on regulatory classification questions, but cannot negotiate away the categorical unavailability of beta shielding once a feature is classified as safety-critical; exit would mean withdrawing the feature or accepting full liability, not relocating the doctrine's reach.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, medical_device_software_developers, payer,
    organized, generational, constrained, national).

% Litigates on behalf of harmed end users, patients, and banking customers. Gains a durable, categorical legal theory that survives without needing to litigate disclosure adequacy case-by-case — arguing severity of domain rather than quality of disclosure is a lower-cost, more predictable litigation posture. Can select which cases to bring and is not bound to any particular vendor relationship.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, plaintiffs_bar_in_critical_systems_litigation, beneficiary,
    organized, biographical, mobile, national).

% Adjudicates whether beta designation shields vendors from liability, articulating and refining the categorical carve-out through case law and, in some domains, regulatory guidance (e.g., FDA software-as-medical-device rules, financial services regulation). Administers the doctrine's boundaries and could, in principle, narrow it back toward the narrow_warning_reading or expand it toward covering more domains as critical.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, bright-line rule that spares courts and litigants the cost of relitigating disclosure adequacy in every case involving a failed beta-labeled feature in a high-stakes domain — once a domain is classified as critical, the liability question resolves on severity grounds rather than requiring fact-intensive inquiry into how genuinely experimental the feature was or how clearly it was disclosed.
% TRANSFER_FUNCTION: Moves the residual risk of critical-system software failure from end users, patients, and banking customers (who would otherwise absorb harm with no recourse against a beta-shielded vendor) back onto vendors, their insurers, and ultimately their investors, in the specific domains classified as life-safety, financial, or otherwise critical.
% ABSENT_VOICES: Insurers who priced beta-shield liability transfer into vendor risk models are not direct parties to the litigation that establishes this doctrine and have limited voice in how courts draw the critical-systems boundary, despite bearing much of the reallocated risk indirectly through claims exposure. Smaller vendors without organized industry representation (unlike medical device developers) are similarly underrepresented relative to well-resourced incumbents in shaping how courts apply the carve-out.
% DISAPPEARANCE_RATIONALE: If this categorical carve-out disappeared, vendors deploying beta-labeled features in medical, financial, and safety-critical systems would resume treating beta status as a viable liability-shaping tool in those domains, insurers would reprice vendor liability coverage downward to reflect the restored shield, and plaintiffs would lose a categorical theory, falling back to case-by-case disclosure-adequacy litigation that is more expensive and less predictable to win. Commercial deployment practices in critical domains would shift toward more aggressive beta labeling of exactly the features currently deterred from being labeled beta.
% FOUNDING_PROBLEM: Software failures in domains where failure causes irreversible physical or financial harm — malfunctioning dosing algorithms, failed trade execution, faulty safety interlocks — were being defended against liability by vendors invoking disclosed 'beta' or experimental status, even where the harm was severe and the affected parties had no meaningful ability to avoid the risk or negotiate around the disclaimer.
% FOUNDING_PROBLEM_CORROBORATION: Patient safety advocacy organizations and financial consumer protection groups, which are not parties benefiting from vendor liability and are structurally adverse to the vendors this doctrine constrains, continue to document ongoing incidents of software failure in medical and financial critical systems, corroborating that the underlying harm pattern the doctrine addresses has not disappeared. No corroborating source has been identified suggesting the founding problem is resolved; vendors themselves are the primary voices arguing the doctrine's continued application is unnecessary, which is not independent corroboration.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored moderate (0.28) rather than high because the carve-out, where it applies, redistributes risk that was arguably always latent in the vendor's product-liability exposure — it does not manufacture a new payment stream so much as block an attempted exit from an existing one. Suppression is moderate (0.35) and rising over the measured interval: as courts and regulators articulate the carve-out more explicitly across cases, the doctrine's coercive force against vendor liability-shifting attempts hardens (reflected in the suppression_requirement series moving from 0.20 to 0.35), even though the underlying extraction level moves only modestly. Theater ratio stays low throughout (0.08 to 0.15) because this is a substantive doctrinal rule with real enforcement teeth, not a performative labeling exercise — courts that apply it actually void the liability shield, they do not merely gesture at scrutiny. Accessibility collapse (0.6) is elevated because, once a court adopts the categorical severity reading, vendors have essentially no alternative labeling strategy within that domain to recover the disclaimer's function; resistance (0.55) is correspondingly real because vendors and their insurers actively litigate against the doctrine's application, arguing for the narrower testing-disclosure reading instead.
 *
 * DIRECTIONALITY LOGIC:
 *   End users, patients, and banking customers are structural beneficiaries with essentially no exit from the software they are subject to (trapped/powerless) — the carve-out is the primary mechanism protecting them, so directionality sits near the full-beneficiary end for them despite their low power, because the constraint's operation runs in their favor. Vendors deploying beta-labeled critical features are the structural targets: they are powerful in the market sense but constrained in their ability to route around the doctrine once a domain is classified as critical — they cannot simply relabel their way out once courts apply the categorical rule, which is precisely what distinguishes this reading from the narrow_warning_reading where genuine disclosure could still matter. Medical device developers sit at organized power but constrained exit because they already operate under parallel regulatory regimes (FDA oversight) that interact with, rather than substitute for, this doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — consumers and downstream parties bearing the risk of undisclosed or under-tested software failures in domains where failure causes irreversible harm — remains fully live: software failures in medical devices, payment infrastructure, and safety-critical control systems continue to occur and continue to cause the harms the doctrine addresses. This is not a mandatrophied constraint; the severity-based carve-out is not vestigial or captured by the very vendors it constrains. The tangled-rope structure is genuine: it coordinates a predictable, litigable boundary (reducing disclosure-adequacy litigation costs for both sides) while extracting the liability-shifting value vendors had built into their beta-labeling commercial practice specifically in high-stakes domains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'The beta designation doctrine kernel supports at least three structurally distinct readings — an expansive shield (any beta label waives liability indefinitely, any context), a narrow warning (beta is time-bounded testing disclosure, base liability survives), and this severity carve-out (beta is categorically unavailable as a liability mechanism in life-safety/financial/critical domains regardless of testing status or disclosure quality). Which reading a court or regulator adopts is not determined by the kernel text alone.',
    'Track appellate decisions and regulatory guidance (FDA software-as-medical-device rules, financial services regulators, product liability case law) for explicit domain-based categorical exclusions versus duration-based or waiver-scope-based holdings.',
    'If courts converge on the severity carve-out reading, beta designation becomes structurally unavailable as risk allocation in an entire class of high-stakes domains, independent of how well-tested or disclosed the beta status is — a categorical rule rather than a case-by-case disclosure analysis. If courts instead converge on the expansive shield or narrow warning readings, this constraint does not exist as a live legal rule and the analysis collapses into the sibling stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of three kernel readings of beta designation doctrine actually governs, and where the disagreement is located (domain-categorical vs. duration-based vs. waiver-scope-based).').

omega_variable(
    critical_system_boundary_definition,
    'What counts as a ''life-safety, financial, or other critical system'' for purposes of the carve-out? Is the boundary drawn by function (does the software control a safety-relevant physical process or financial transaction) or by sector label (healthcare, finance, transportation)?',
    'Examine how the carve-out is actually litigated: whether courts draw the line functionally (any software whose failure causes physical harm or financial loss, regardless of industry) or categorically by regulated sector.',
    'A functional boundary sweeps in far more software (e.g., a beta feature in a general consumer app that happens to control a smart-lock or payment flow) than a sector boundary, changing which vendors are structurally exposed and how large the victim class is.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_system_boundary_definition, conceptual, 'Ambiguity in what qualifies as a critical system triggering the categorical carve-out.').

omega_variable(
    genuine_testing_vs_permanent_beta,
    'Even within this reading, does the carve-out apply only where the vendor is genuinely using beta status for testing purposes, or does it apply even to features that would otherwise qualify as legitimate testing disclosure, purely because of domain severity?',
    'Compare case outcomes where vendors present strong evidence of active testing/iteration versus outcomes where vendors present weak evidence, holding the critical-system classification constant.',
    'If the carve-out is truly categorical (testing quality irrelevant), the doctrine imposes a bright-line domain rule. If testing quality still matters at the margins, this reading partially collapses back toward the narrow_warning_reading and the two are less structurally distinct than declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_vs_permanent_beta, empirical, 'Whether the severity carve-out is truly indifferent to testing genuineness, as the reading''s title claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 8, 0.23).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 16, 0.33).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings decomposed from the single natural-language label 'beta designation doctrine,' per the epsilon-invariance principle. The expansive_shield_reading (comprehensive waiver, any duration, any context) and narrow_warning_reading (time-bounded disclosure, base liability preserved if testing is genuine) are separate constraint files with their own epsilon values and stakeholder structures. This severity_carve_out_reading is structurally distinct: its dispositive variable is domain criticality, not waiver scope or testing duration. All three are linked bidirectionally via affects_constraints because a shift in which reading dominant case law adopts directly changes the legal environment the other two readings operate in — e.g., broader adoption of this severity carve-out narrows the practical domain in which the expansive_shield_reading could ever prevail, since critical-systems cases would be resolved on severity grounds before waiver-scope arguments are reached.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
