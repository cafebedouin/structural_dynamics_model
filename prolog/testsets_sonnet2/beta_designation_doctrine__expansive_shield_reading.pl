% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Beta Designation as Comprehensive, Indefinite, Universal Liability Shield
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This story instantiates the expansive reading of the beta designation
 *   kernel: a single click-through 'beta' or 'preview' label, once attached,
 *   functions as a comprehensive and indefinite liability waiver applicable
 *   across all software contexts, including life-safety, financial, and
 *   infrastructure systems, with no requirement that the label correspond to
 *   a genuine, time-bounded testing phase. This is distinct from the
 *   narrow_warning_reading (which preserves base liability and requires a
 *   genuine bounded testing window) and the severity_carve_out_reading (which
 *   would categorically exclude critical systems from beta-shielding
 *   regardless of disclosure). Those are separate constraints with their own
 *   ε values; this file does not average across them or describe their
 *   contest internally — the committer structure lives in the omegas below.
 *
 * KEY AGENTS:
 *   - software_publishers: primary beneficiary and agenda-setter — drafts and administers the label
 *   - platform_operators: secondary beneficiary — distributes beta software at full commission with no defect exposure
 *   - product_liability_insurers: beneficiary — prices risk pools assuming the shield holds
 *   - consumer_beta_users: primary target — bears defect costs with no bargaining power and trapped exit
 *   - small_business_beta_adopters: target — constrained exit, real business losses
 *   - downstream_integrators: target — absorbs liability passed through from beta-labeled dependencies
 *   - consumer_advocacy_groups: excluded voice — not present at terms drafting
 *   - courts_and_regulators: analytical observer — inconsistent enforcement against this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.81).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.72).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive, Indefinite, Universal Liability Shield").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, '1b09d8cc-a85d-4aab-9b34-e1bc92c42c41').
narrative_ontology:cs_kernel_codification('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', formalized).
narrative_ontology:cs_authority_grounding('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', extraction).
narrative_ontology:cs_interpretation_layer_present('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41').
narrative_ontology:cs_reading_relation('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_reading_relation('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', beta_designation_doctrine__severity_carve_out_reading, forecloses).
narrative_ontology:cs_axiom('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', foundational, unilateral_label_dispositive_of_liability).
narrative_ontology:cs_axiom_status(unilateral_label_dispositive_of_liability, holdable).
narrative_ontology:cs_axiom_grounding('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', unilateral_label_dispositive_of_liability, conventional).
narrative_ontology:cs_axiom('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', foundational, no_temporal_or_severity_boundary_required).
narrative_ontology:cs_axiom_status(no_temporal_or_severity_boundary_required, holdable).
narrative_ontology:cs_axiom_grounding('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', no_temporal_or_severity_boundary_required, instrumental).
narrative_ontology:cs_reference_frame('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', unilateral_label_as_dispositive_waiver).
narrative_ontology:cs_drift_state('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', contemporary_saas_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1b09d8cc-a85d-4aab-9b34-e1bc92c42c41', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_publishers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, platform_operators).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, product_liability_insurers).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, consumer_beta_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, small_business_beta_adopters).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, downstream_integrators).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__expansive_shield_reading, contractual_freedom_of_terms_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the click-through terms that attach 'beta' or 'preview' labels to products, define the label's legal meaning as a total liability release, and can keep a product in beta status for years or permanently. They face no meaningful cost from mislabeling because the label itself extinguishes claims regardless of how long it is worn or how critical the software's function turns out to be.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_publishers, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, software_publishers, beneficiary).

% Host and distribute beta-labeled software through app stores and marketplaces, incorporating the same liability-waiver language into their own distribution terms. They collect distribution fees from beta products at the same rate as finished ones while bearing none of the defect risk, which the label routes entirely to the publisher-user relationship.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, platform_operators, beneficiary,
    institutional, civilizational, arbitrage, global).

% Underwrite technology companies at lower premiums when the beta label is available as a categorical defense, since it removes an entire class of claims from the loss pool without requiring the insurer to evaluate the software's actual maturity or risk profile.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, product_liability_insurers, beneficiary,
    institutional, generational, arbitrage, global).

% Install beta-labeled apps and services that are often functionally indistinguishable from finished releases, sometimes running years past any genuine testing phase. When a defect causes data loss, financial harm, or device damage, the beta label is asserted as a complete bar to recovery regardless of how long the software has carried the label or how severe the harm. They rarely read the terms and have no bargaining position to negotiate them.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_beta_users, payer,
    powerless, immediate, trapped, national).

% Adopt beta-labeled business software (payment processing, inventory, scheduling) because it is often the only or cheapest available option in a category, or because a larger partner requires it. A defect can cause real business losses, but the waiver forecloses recovery; switching costs and the absence of finished alternatives leave them functionally locked into beta products they did not choose for their beta status.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, small_business_beta_adopters, payer,
    moderate, biographical, constrained, national).

% Build products and services on top of beta-labeled APIs and infrastructure that publishers never intend to graduate out of beta status, since indefinite beta status carries no legal cost to the publisher. When the underlying beta component fails, the integrator absorbs the liability to its own downstream customers while having no recourse against the labeled component's publisher.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, downstream_integrators, payer,
    moderate, biographical, constrained, global).

% Would argue that a liability waiver conditioned on a single click-through label, unbounded in time and unbounded in the severity or criticality of the software's function, converts a testing disclosure into a permanent immunity grant. They are not party to the terms-of-service drafting process and their input is not solicited by publishers or legislatures until litigation or regulatory review is already underway.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_advocacy_groups, excluded,
    organized, generational, analytical, national).

% Adjudicate disputes where the beta label is raised as a defense, and can either enforce the waiver as written, narrow its scope, or reject it as unconscionable. Their rulings determine whether this expansive reading of the doctrine survives contact with litigation, but enforcement is inconsistent and reactive.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, software_publishers).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its legitimate form, a beta label lets developers solicit real-world testing feedback on unfinished software by disclosing immaturity to users who then assume informed, bounded risk — a genuine coordination problem (how do you test software with real users without full production-grade liability) that this reading claims to solve.
% TRANSFER_FUNCTION: Moves the cost of software defects — data loss, financial harm, downstream business failure, integration breakage — from the publisher who wrote and profits from the software to the user who installed it, via a label the publisher can attach and never remove, regardless of the software's actual maturity, revenue status, or the severity of harm it can cause.
% ABSENT_VOICES: Consumer advocacy groups and unrepresented individual users are not present when platform terms of service are drafted; small business adopters typically discover the waiver's scope only after a defect has already caused loss, not at the point of adoption.
% DISAPPEARANCE_RATIONALE: If this expansive reading disappeared, publishers would face ordinary product liability exposure for software currently held in indefinite or de facto permanent beta status; some publishers would either genuinely time-bound their beta phases and graduate products, price in insurance against defect liability, or exit categories where defect risk is high — the shield's absence would force the coordination function (informed real-world testing) to actually operate as disclosure rather than as blanket cover.
% FOUNDING_PROBLEM: Software could not be adequately tested for all real-world conditions before release, and developers needed a way to solicit genuine user testing without assuming full production liability for known-incomplete software during a genuine, bounded testing window.
% FOUNDING_PROBLEM_CORROBORATION: Publishers and platform operators attest the beta framework remains necessary for iterative development at scale. Courts in several unconscionability rulings and consumer protection regulators in enforcement actions have found that indefinite, unbounded beta labeling no longer resembles a testing disclosure and instead functions as a standing liability shield — corroboration from outside the benefiting parties that the founding problem, as applied under this reading, has been substantially superseded by the shield function.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is high (0.81) because the reading permits the waiver to apply without regard to duration or severity — the two boundaries that would otherwise cap how much cost can be externalized. Suppression (0.72) reflects the structural reality that virtually no user negotiates or can negotiate around click-through terms; exit is only nominally available (don't use the software) and often not available at all for software embedded in a larger product or workflow. Theater ratio is elevated and rising (0.58 by interval end) because an increasing share of 'beta' labeling activity is legal-defensive rather than genuine testing solicitation — the label persists on products with millions of active users and years of stable operation, which is the signature of a captured designation rather than an active testing phase.
 *
 * PERSPECTIVAL GAP:
 *   From the software_publishers seat, the arrangement reads as efficient risk allocation enabling continuous iterative deployment. From the consumer_beta_users and downstream_integrators seats, the same structure operates as a standing liability shield triggered by unilateral labeling with no correspondence to actual product risk or maturity. The engine should compute these as structurally different experiences of the same constraint given the divergent power, exit, and directionality inputs — this divergence is not resolved by the claimed_type, which states the analytical judgment that the coordination story is cover for the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers, platform operators, and insurers are declared beneficiaries with institutional power and arbitrage-grade exit (they can restructure terms, relabel, or exit unfavorable jurisdictions) — d sits near the full-beneficiary end. Consumer beta users are declared victims with powerless standing and trapped exit (no bargaining position, often no viable alternative product) — d sits near the full-target end. Small business adopters and downstream integrators are victims with moderate power and constrained exit — meaningfully harmed but with somewhat more capacity to seek alternatives than individual consumers, hence moderate rather than maximal d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling real-world testing without full production liability during a genuine bounded window — was real and narrow. This reading's classification as snare (rather than a legitimate scaffold) turns on the absence of both boundaries the founding problem actually required: duration and severity. Without a sunset, the 'testing phase' framing becomes permanently available cover; without a severity carve-out, the shield extends to contexts (financial systems, safety-critical infrastructure) where the coordination rationale (soliciting user feedback on unfinished features) does not plausibly justify externalizing catastrophic harm. The mismatch between founding_problem_status (contested, with courts and regulators corroborating obsolescence) and disappearance_verdict (world_rearranges) is the mandatrophy signal: the arrangement persists past its founding justification because publishers, platforms, and insurers actively benefit from its indefinite, unbounded form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Which reading of the beta_designation_doctrine kernel governs a given dispute — expansive_shield, narrow_warning, or severity_carve_out — and what determines which reading a court or legislature adopts?',
    'Track jurisdictional divergence in unconscionability rulings on beta-labeled software liability waivers; a reading gains ground as courts cite it as precedent or as legislatures codify duration/severity limits into consumer protection statutes.',
    'If courts converge on the narrow_warning_reading or severity_carve_out_reading, this expansive_shield_reading''s practical extraction collapses toward the levels described in those sibling stories; if courts continue enforcing the label as written regardless of duration or context, this reading''s high-extraction profile persists and hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which sibling reading of the beta designation kernel actually governs in practice, and how that is determined.').

omega_variable(
    genuine_testing_vs_permanent_label,
    'For any given beta-labeled product, is the label attached to a genuine, currently-active testing process, or has it become a permanent status maintained for its legal effect independent of actual product maturity?',
    'Compare label duration, user base size and stability, revenue generation, and update cadence against the publisher''s own internal characterization of the product''s development status (if discoverable in litigation).',
    'If most beta-labeled products in a domain show mature, stable, revenue-generating characteristics inconsistent with active testing, this substantially strengthens the case that the label functions as this reading claims — a comprehensive shield decoupled from its disclosed purpose — rather than as legitimate testing disclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_vs_permanent_label, empirical, 'Whether beta status in practice tracks genuine testing activity or has decoupled into a standing legal designation.').

omega_variable(
    critical_system_beta_prevalence,
    'How often is beta or preview labeling actually applied to life-safety, financial, or infrastructure-critical software, and what harm has resulted where the label was asserted as a defense?',
    'Incident and litigation database review across sectors (medical device software, financial trading platforms, industrial control systems) for beta-labeled deployments and subsequent defect litigation outcomes.',
    'High prevalence with successful shield assertions would validate the severity_carve_out_reading''s core objection and substantially undermine this reading''s structural legitimacy even where courts currently enforce it; low prevalence would narrow this reading''s practical extraction to non-critical contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_system_beta_prevalence, empirical, 'Empirical prevalence and consequences of beta-labeling in critical-system contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 16, 0.54).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 12, 0.71).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 16, 0.77).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__expansive_shield_reading, 0.1).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the beta_designation_doctrine kernel. expansive_shield_reading (this file) claims comprehensive, indefinite, all-context liability waiver from a single label — high ε, victim set includes ordinary consumers, snare classification. narrow_warning_reading claims the label is legitimate only as a bounded testing disclosure with base liability preserved — expected low-to-moderate ε, closer to scaffold or rope. severity_carve_out_reading claims the label is categorically unavailable for critical systems regardless of disclosure or testing status — a boundary-drawing constraint whose ε concerns enforcement of the carve-out itself. Each story carries its own beneficiary/victim structure and stable ε per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__expansive_shield_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
