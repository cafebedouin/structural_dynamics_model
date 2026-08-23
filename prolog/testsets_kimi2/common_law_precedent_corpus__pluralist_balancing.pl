% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Pluralist Balancing of Common Law Precedent
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   This constraint instantiates the pluralist_balancing reading of the
 *   common_law_precedent_corpus kernel. Under this reading, the weight of
 *   precedent is not uniform; it varies by subject-matter domain,
 *   hierarchical level, and social context, with courts balancing the need
 *   for stability against the need for adaptation on a case-by-case basis.
 *   The arrangement serves a genuine coordination functionâlegal stability
 *   and inter-judicial consistencyâwhile simultaneously generating
 *   asymmetric extraction: resource-constrained litigants face unpredictable
 *   domain-switching costs, trial courts face hierarchical compliance
 *   burdens, and novel claimants encounter high variance in whether their
 *   claims will be treated as governed by fixed rules or open balancing.
 *   Sibling readings include strict_stare_decisis (universal backward
 *   binding) and evolutionary_framework (normative adaptation permitting
 *   reinterpretation).
 *
 * KEY AGENTS:
 *   - Appellate judiciary (institutional/agenda-setter): Controls the dial on precedent weight and derives institutional authority from calibrating it.
 *   - Repeat-player corporations (powerful/beneficiary): Capture strategic value from precedent variance and stability differentials.
 *   - Legal profession (organized/beneficiary): Monetizes the interpretive complexity of domain-dependent balancing.
 *   - Resource-constrained litigants (powerless/payer): Bear the unpredictability costs without resources to map them.
 *   - Novel claimants (moderate/payer): Face high-variance treatment where doctrinal categories are unsettled.
 *   - Trial judiciary (institutional/payer): Administers precedent but is captured by appellate hierarchy extraction.
 *   - Legal scholars (analytical/observer): External critics who analyze the system's variance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.62).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.48).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.62).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Pluralist Balancing of Common Law Precedent").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, 'ef04a118-d250-484b-8a91-272466c9f208').
narrative_ontology:cs_kernel_codification('ef04a118-d250-484b-8a91-272466c9f208', fixed_text).
narrative_ontology:cs_authority_grounding('ef04a118-d250-484b-8a91-272466c9f208', lineage).
narrative_ontology:cs_interpretation_layer_present('ef04a118-d250-484b-8a91-272466c9f208').
narrative_ontology:cs_reading_relation('ef04a118-d250-484b-8a91-272466c9f208', common_law_precedent_corpus__strict_stare_decisis, forecloses).
narrative_ontology:cs_reading_relation('ef04a118-d250-484b-8a91-272466c9f208', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('ef04a118-d250-484b-8a91-272466c9f208', foundational, precedent_weight_domain_dependent).
narrative_ontology:cs_axiom_status(precedent_weight_domain_dependent, holdable).
narrative_ontology:cs_axiom_grounding('ef04a118-d250-484b-8a91-272466c9f208', precedent_weight_domain_dependent, conventional).
narrative_ontology:cs_axiom('ef04a118-d250-484b-8a91-272466c9f208', foundational, balancing_test_legitimate_method).
narrative_ontology:cs_axiom_status(balancing_test_legitimate_method, holdable).
narrative_ontology:cs_axiom_grounding('ef04a118-d250-484b-8a91-272466c9f208', balancing_test_legitimate_method, conventional).
narrative_ontology:cs_reference_frame('ef04a118-d250-484b-8a91-272466c9f208', context_sensitive_precedent_regime).
narrative_ontology:cs_drift_state('ef04a118-d250-484b-8a91-272466c9f208', contemporary_legal_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef04a118-d250-484b-8a91-272466c9f208', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, repeat_player_corporations).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, legal_profession).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, resource_constrained_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, novel_claimants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, trial_judiciary).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, common_law_method_legitimacy).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, judicial_discretion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the calibration of precedent weight across domains and levels of the judicial hierarchy; declares which precedents bind tightly, which are distinguishable, and which domains permit adaptive balancing. Derives institutional authority and career legitimacy from the ability to manage this hierarchy.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, beneficiary).

% Litigate frequently and can afford the legal analysis to map variance in precedent weight across circuits or subject-matter domains; use precedent strategically to freeze favorable rules or distinguish unfavorable ones, capturing stability benefits while shifting adaptation costs to opponents.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, repeat_player_corporations, beneficiary,
    powerful, biographical, mobile, national).

% Derives revenue, professional status, and market position from the complexity of balancing tests, domain-specific precedent hierarchies, and the interpretive labor required to predict which weight a court will assign.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Cannot predict which precedent regime or balancing test will apply to their case; lack resources to commission the legal analysis that maps domain variance; often forced into unfavorable settlement or procedural default by uncertainty about which precedents bind.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, resource_constrained_litigants, payer,
    powerless, immediate, trapped, national).

% Bring claims that do not fit cleanly into existing doctrinal categories; face high variance in whether courts treat their domain as governed by strict stability or open adaptation; precedent offers little guidance and substantial hazard because the balancing calculus is unsettled for their issue area.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, novel_claimants, payer,
    moderate, biographical, constrained, national).

% Bound by appellate precedent hierarchies but must apply them to messy fact patterns that do not map cleanly to the domains where weight was calibrated; exposed to reversal if their balancing is deemed an abuse of discretion, yet given little concrete guidance on how to balance.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, trial_judiciary, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, trial_judiciary, agenda_setter).

% Analyze and critique the variance in precedent weight across domains; some argue for formalist constraint while others defend greater adaptivity, but none are bound by the constraint they study.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__pluralist_balancing, diffuse).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__pluralist_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a distributed framework for legal stability and predictability by linking present decisions to past ones, allowing actors to plan around declared rules and preventing every dispute from being re-litigated de novo.
% TRANSFER_FUNCTION: Moves interpretive control, litigation advantage, and systemic compliance costs from resource-constrained litigants, novel claimants, and trial courts to appellate courts, repeat-player corporations, and the legal profession via unpredictable domain-switching costs and hierarchical precedent control.
% ABSENT_VOICES: Lay litigants proceeding pro se, civil-law comparatists, and substantive communities whose norms are not captured by case-law categories are excluded from the precedent-design conversation; they would argue for simpler, more transparent rule structures or codified alternatives.
% DISAPPEARANCE_RATIONALE: If precedent-based reasoning vanished overnight, the doctrinal architecture of property, contract, tort, and due process would destabilize; courts and litigants would face radical uncertainty; planning and enforcement would require immediate statutory or constitutional codification to avoid chaos.
% FOUNDING_PROBLEM: How to settle disputes consistently across a distributed judiciary without requiring a legislature to prospectively code every possible controversy, while permitting law to adapt incrementally to social change.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative jurists outside the common-law bar attest that the case-by-case dispute-resolution problem is partially solved but has mutated; legislative codification movements and civil-law scholars corroborate that alternative methods exist, while common-law jurists self-assert the necessity of the arrangement.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is medium-high because the context-dependent variance in precedent weight generates real, asymmetric litigation costs and strategic advantages; suppression (0.48) is moderate because alternatives such as statutory override, constitutional amendment, or distinguishing arguments exist but are institutionally costly; theater_ratio (0.45) reflects significant performative maintenanceâopinions present balancing as disciplined legal reasoning while exercising substantial discretion; accessibility_collapse (0.65) is moderately high because, within the common-law system, alternatives to precedent-based reasoning are largely collapsed; resistance (0.45) captures routine scholarly and litigant contestation of precedent application without systemic rejection of the method itself. The temporal series show a gradual rise in both extraction and theater as the pluralist-balancing regime matured and refined its multi-tier hierarchy.
 *
 * PERSPECTIVAL GAP:
 *   From the appellate seat, the arrangement appears as principled, domain-sensitive craftsmanship that prevents both rigid formalism and raw judicial will. From the resource-constrained litigant seat, the same arrangement appears as an opaque, expensive lottery where the rules change based on which domain label a court selects. The trial judiciary occupies an intermediate perspectival position: they experience the constraint as a compliance burden imposed from above, even as they impose it on parties below. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate judiciary, repeat-player corporations, and the legal profession are structural beneficiaries: they collect authority, strategic advantage, and revenue from the complexity and variance of the precedent regime. Resource-constrained litigants, novel claimants, and trial judiciary are structural targets: they bear the compliance costs, unpredictability penalties, and hierarchical control that the regime distributes asymmetrically. Device-users or the general public are not modeled as primary seats because their interaction with this constraint is mediated by the litigant and professional layers.
 *
 * MANDATROPHY ANALYSIS:
 *   The genuine coordination functionâstability, predictability, and inter-judicial consistencyâprevents misclassification as a pure snare. Without that function, the extraction of domain-switching costs would dominate. Conversely, the identifiable victim set and multi-tier extractiveness prevent misclassification as a pure rope. The tangled_rope classification captures that both coordination and extraction are structurally present and operate through the same mechanism: the precedent hierarchy itself coordinates courts while extracting from those lower in the hierarchy and from parties who cannot navigate its variance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_extraction_quantification,
    'Can the extractive effect of domain-switching costs and precedent-variance on resource-constrained litigants be measured empirically?',
    'Large-N empirical study of case outcomes, time-to-resolution, and settlement rates across subject-matter domains, controlling for case complexity and litigant resources.',
    'A robust empirical extraction signal would support regulatory or procedural interventions (e.g., simplified rules for pro se litigants); absence of signal would weaken the victim-ascription in this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_extraction_quantification, empirical, 'Whether precedent variance creates measurable extraction.').

omega_variable(
    pluralist_stability_boundary,
    'Does the pluralist balancing reading collapse into disguised judicial discretion, or does it maintain a genuinely constraining intermediate position between strict stare decisis and pure policy?',
    'Longitudinal doctrinal mapping: measure the predictability of appellate outcomes given case features under the pluralist regime versus formalist and evolutionary regimes.',
    'If outcomes are no more predictable under pluralist balancing than under pure policy, the coordination function is largely theatrical and the constraint drifts toward snare; if predictability is meaningfully higher, the coordination function is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pluralist_stability_boundary, conceptual, 'Whether pluralist balancing is genuinely constraining.').

omega_variable(
    kernel_reading_sibling_stability,
    'Does the pluralist balancing reading functionally oscillate between strict stare decisis in some domains and evolutionary reinterpretation in others, rather than maintaining a stable intermediate position?',
    'Domain-level classification of precedent treatment across constitutional, statutory, and common-law fields; measure rigidity variance.',
    'If the reading is functionally bimodal, it may be unstable as a distinct constraint and better understood as a composite of its siblings; if variance is continuous, the reading is structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_stability, conceptual, 'Whether pluralist balancing is a stable reading or a composite.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 10, 0.35).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 20, 0.4).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 30, 0.42).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 40, 0.44).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 50, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(common_law_precedent_corpus__pluralist_balancing, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
