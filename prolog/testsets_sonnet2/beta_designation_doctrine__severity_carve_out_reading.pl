% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   human_readable: Beta Designation Severity Carve-Out (Life-Safety/Financial/Critical Systems Exclusion)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This story instantiates the severity carve-out reading of the beta
 *   designation kernel: the claim that regardless of how genuinely a product
 *   is in testing, how clearly the beta status is disclosed, or how narrow
 *   its duration, the beta label is categorically unavailable as a
 *   liability-limiting mechanism once the software touches life-safety,
 *   financial, or other critical functions. This is not a claim about
 *   disclosure adequacy (narrow_warning_reading) or about the scope of what
 *   beta can waive in general (expansive_shield_reading) — it is a
 *   domain-based exclusion that operates upstream of both, ruling certain
 *   subject-matter categories out of the beta-liability bargain entirely.
 *   Courts and regulators applying this reading do not ask whether the
 *   testing was real or the warning was clear; they ask only whether the
 *   harm-severity classification of the domain triggers the carve-out.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.58).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.42).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Severity Carve-Out (Life-Safety/Financial/Critical Systems Exclusion)").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '75f50d2c-8e5c-4375-89a4-8e766a854720').
narrative_ontology:cs_kernel_codification('75f50d2c-8e5c-4375-89a4-8e766a854720', distributed).
narrative_ontology:cs_authority_grounding('75f50d2c-8e5c-4375-89a4-8e766a854720', distributed).
narrative_ontology:cs_reading_relation('75f50d2c-8e5c-4375-89a4-8e766a854720', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('75f50d2c-8e5c-4375-89a4-8e766a854720', beta_designation_doctrine__narrow_warning_reading, influences).
narrative_ontology:cs_axiom('75f50d2c-8e5c-4375-89a4-8e766a854720', foundational, harm_severity_categorically_overrides_disclaimer).
narrative_ontology:cs_axiom_status(harm_severity_categorically_overrides_disclaimer, holdable).
narrative_ontology:cs_axiom_grounding('75f50d2c-8e5c-4375-89a4-8e766a854720', harm_severity_categorically_overrides_disclaimer, deontological).
narrative_ontology:cs_axiom('75f50d2c-8e5c-4375-89a4-8e766a854720', secondary, domain_classification_is_dispositive_not_conduct).
narrative_ontology:cs_axiom_status(domain_classification_is_dispositive_not_conduct, holdable).
narrative_ontology:cs_axiom_grounding('75f50d2c-8e5c-4375-89a4-8e766a854720', domain_classification_is_dispositive_not_conduct, conventional).
narrative_ontology:cs_reference_frame('75f50d2c-8e5c-4375-89a4-8e766a854720', consumer_software_beta_norm).
narrative_ontology:cs_drift_state('75f50d2c-8e5c-4375-89a4-8e766a854720', post_medtech_fintech_beta_litigation_wave, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('75f50d2c-8e5c-4375-89a4-8e766a854720', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_system_end_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, patients_and_care_recipients).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, consumers_of_financial_software).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, plaintiffs_bar_specializing_in_product_liability).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, safety_regulators).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_vendors_in_regulated_verticals).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, startups_building_medtech_and_fintech).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, enterprise_procurement_teams_relying_on_beta_carveout).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, harm_severity_overrides_contractual_disclaimer).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, categorical_domain_exclusion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend on life-safety, medical, or financial software without visibility into its testing status or internal risk assessment. Under this reading, they are protected because vendors cannot use a 'beta' label to disclaim liability for harms in these domains regardless of what the license agreement says or how conspicuously the beta status was disclosed.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_system_end_users, beneficiary,
    powerless, immediate, trapped, national).

% Rely on medical device software, diagnostic algorithms, and care-coordination systems that may be run under an early-release label. They have no practical ability to evaluate the software's maturity and cannot decline treatment mediated by it without cost; the carve-out is what stands between them and unrecoverable harm shielded by a disclaimer.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, patients_and_care_recipients, beneficiary,
    powerless, immediate, trapped, national).

% Use trading platforms, robo-advisors, or payment infrastructure that vendors sometimes market as beta to limit exposure for calculation errors or fund-handling defects. Under this reading their losses cannot be waived away by the beta label; they retain standard product liability and consumer protection remedies.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, consumers_of_financial_software, beneficiary,
    moderate, biographical, constrained, national).

% Litigate harms in these verticals and press courts to adopt the categorical carve-out as doctrine. They benefit professionally and financially from a bright-line rule that removes the beta defense entirely in high-severity domains, and they actively litigate test cases to entrench the reading.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, plaintiffs_bar_specializing_in_product_liability, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__severity_carve_out_reading, plaintiffs_bar_specializing_in_product_liability, agenda_setter).

% Agencies overseeing medical devices, financial services, and critical infrastructure software. They administer and can expand or narrow the severity carve-out through rulemaking and enforcement guidance, treating the beta label as irrelevant to their statutory mandate to prevent harm in these sectors.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Built products with genuine beta/testing phases and disclosed this clearly to customers, expecting the label to modulate liability exposure the way it does in ordinary consumer software. Under this reading that protection is categorically unavailable in their vertical no matter how rigorous or transparent their testing disclosure was; they bear full product liability exposure they priced their release strategy around avoiding.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_vendors_in_regulated_verticals, payer,
    powerful, biographical, constrained, national).

% Smaller entrants without the balance sheet of incumbents rely on iterative beta releases to gather real-world data cheaply before full liability exposure. The carve-out removes that on-ramp entirely for their domain, forcing either full pre-release liability insurance costs they may not afford, or exit from the regulated vertical altogether. They cannot relocate the harm-severity classification of their own product.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, startups_building_medtech_and_fintech, payer,
    moderate, biographical, trapped, national).

% Negotiated contracts assuming beta-labeled components carried reduced vendor liability, and priced their own downstream obligations accordingly. The carve-out unwinds that risk allocation retroactively for any component touching life-safety or financial-critical functions, exposing them to renegotiation and potential contractual gaps they did not budget for.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, enterprise_procurement_teams_relying_on_beta_carveout, payer,
    powerful, biographical, constrained, national).

% Adjudicate whether a given beta disclaimer is enforceable given the severity of the domain. They hear testimony from vendors, injured parties, and regulators, and their rulings either entrench or narrow this categorical exclusion over time.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, courts_and_appellate_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line rule that harm severity in life-safety, financial, and other critical domains categorically overrides any contractual or labeling attempt to reallocate liability via a 'beta' designation — coordinating expectations across vendors, regulators, and courts about which domains cannot be experimented on with reduced liability regardless of disclosure quality.
% TRANSFER_FUNCTION: Moves liability exposure from end users and consumers back onto vendors and their downstream commercial customers in regulated verticals, reversing what a beta label would otherwise have shifted onto users who accepted the designation.
% ABSENT_VOICES: Vendors who conducted genuinely rigorous, well-disclosed beta programs in borderline-critical domains (e.g., wellness apps adjacent to but not strictly medical) are not distinguished from reckless mislabeling; their voice — that severity-based categorical exclusion punishes good-faith incremental testing indiscriminately — is not part of the doctrine's own reasoning, which treats domain classification as dispositive rather than testing conduct.
% DISAPPEARANCE_RATIONALE: If this carve-out vanished, vendors in medtech, fintech, and critical infrastructure could resume using beta labels to limit liability the way general consumer software does; insurance pricing, procurement contracts, and litigation strategy in these verticals would shift substantially, and some harms currently compensable would instead be barred or reduced by disclaimer.
% FOUNDING_PROBLEM: Software vendors in high-stakes domains were using 'beta' or 'early access' labels — sometimes alongside real testing, sometimes as pure liability theater — to disclaim responsibility for harms (misdiagnoses, trading losses, infrastructure failures) that ordinary users could not meaningfully consent to given the severity and irreversibility of the potential harm.
% FOUNDING_PROBLEM_CORROBORATION: Safety regulators and courts outside the plaintiffs' bar corroborate the founding problem through documented enforcement actions and published opinions describing beta-labeled medical and financial software causing harm the label was invoked to excuse; independent academic product-liability scholarship on 'move fast' culture colliding with regulated domains supports the same account from outside both the plaintiff and vendor interest groups.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that vendors in regulated verticals who built compliant, well-disclosed beta programs still lose the liability-limiting benefit entirely — the cost lands on them regardless of good-faith conduct, which is a real transfer, not merely risk-pricing. Suppression (0.42) is moderate: the rule is enforced through litigation and regulatory guidance rather than direct coercion, but it does foreclose an entire commercial practice (regulated-vertical beta releases) as a category, which is a meaningful accessibility collapse for that specific use. Accessibility_collapse (0.35) is lower than a pure mountain because vendors retain other risk-mitigation tools (staged rollout, informed consent frameworks, insurance) — beta labeling specifically is foreclosed, not risk management generally. Resistance (0.55) is substantial: vendor associations and startups actively litigate and lobby against the categorical bar, arguing case-by-case testing-conduct review would be fairer than a domain-based bright line.
 *
 * DIRECTIONALITY LOGIC:
 *   End users, patients, and financial consumers are beneficiaries with low derived directionality — the carve-out exists specifically to prevent liability from being shifted onto them via label. Vendors and enterprise procurement in regulated verticals are targets: the rule strips a risk-allocation tool they had reasonably priced into their release strategy, and their exit options are constrained (they cannot simply leave the regulated vertical without abandoning the market or relocating jurisdictionally, which is itself costly). Regulators and the plaintiffs' bar are both beneficiaries and agenda-setters — they gain from the doctrine's existence (mandate fulfillment, litigation opportunity) and actively work to entrench it, which is the coordination/extraction hybrid this tangled_rope reading captures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — vendors weaponizing beta labels to disclaim harm in domains users cannot meaningfully consent to given irreversibility — remains live wherever critical software continues to ship under early-access framing. The doctrine has not drifted into pure theater: enforcement and litigation activity are increasing (suppression_requirement climbing from 0.22 to 0.42), not merely performative. However, the categorical (rather than conduct-based) nature of the carve-out means it extracts even from vendors whose testing and disclosure practices were genuinely responsible — this is the tangled_rope signature: real coordination function (protecting non-consenting parties from catastrophic, hard-to-reverse harm) bundled with asymmetric extraction (good-faith vendors pay the same categorical price as bad-faith ones).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_line_drawing,
    'Where exactly does ''life-safety, financial, or other critical systems'' stop? Is a wellness app that tracks but does not diagnose covered? Is a budgeting app that does not custody funds covered?',
    'Accumulated case law and regulatory guidance defining the boundary categories; a taxonomy of covered domains would need to be built through litigation or rulemaking rather than existing ex ante.',
    'A narrowly drawn boundary limits the carve-out''s extraction to genuinely high-stakes vendors; an expansively drawn boundary sweeps in borderline products and increases extraction on startups without a corresponding increase in the protected harm class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_line_drawing, conceptual, 'Uncertainty about which domains fall within the categorical carve-out.').

omega_variable(
    conduct_versus_domain_gating,
    'Should the doctrine gate on domain classification alone (this reading) or on testing conduct plus domain (a hybrid the sibling readings partially anticipate)? Is the categorical exclusion the right instrument, or would a conduct-sensitive standard achieve the same protective function with less collateral extraction from good-faith vendors?',
    'Comparative outcome study: jurisdictions or time periods applying conduct-based review versus categorical domain exclusion, measured against harm rates and innovation/entry rates in regulated verticals.',
    'If conduct-based review achieves comparable harm reduction with less extraction from responsible actors, the categorical form of this reading would be shown to be doing more coercive work than its coordination function requires — pushing it further toward the snare end of tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conduct_versus_domain_gating, preference, 'Whether categorical domain exclusion is the right instrument versus conduct-sensitive review.').

omega_variable(
    kernel_framing_under_determination,
    'Is ''the beta designation doctrine'' genuinely one kernel with three readings, or are these three functionally independent legal rules that happen to share vocabulary (the word ''beta'') but do not actually compete for the same adjudicative space?',
    'Examine whether any single case could plausibly be argued under more than one reading simultaneously (suggesting shared kernel) versus whether courts treat these as entirely separate doctrinal questions triggered by different fact patterns (suggesting three unrelated rules mislabeled as one kernel).',
    'If the readings never actually compete in the same case, the kernel framing is a labeling artifact and this story should be treated as a standalone constraint rather than linked via reading_relations; if they do compete, the reading_relations correctly capture live doctrinal tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the three declared readings genuinely share one contested kernel or are independently-triggered rules sharing only vocabulary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 4, 0.27).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(beta_su_t24, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the beta_designation_doctrine kernel. expansive_shield_reading treats beta as a comprehensive, indefinite liability waiver across all software contexts (ε low from the vendor-protective seat). narrow_warning_reading treats beta as time-bounded disclosure preserving base liability, gated on disclosure adequacy and genuine testing duration (ε moderate, contested on conduct). This severity_carve_out_reading is orthogonal to both: it imposes a categorical domain-based gate that operates regardless of how the disclosure-adequacy or waiver-scope questions are resolved. A single fact pattern (e.g., a medical-diagnostic app labeled beta with clear disclosure and a defined testing window) could be argued to satisfy narrow_warning_reading's disclosure standard while still being barred entirely under this reading's domain exclusion — the readings are not mutually exclusive outcomes but stack as sequential gates a vendor must clear.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
