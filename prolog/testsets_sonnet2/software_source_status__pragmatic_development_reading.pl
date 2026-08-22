% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open Source as Superior Development Methodology (Pragmatic/Instrumental Reading)
 *   domain: software_engineering/political_economy_of_technology
 *
 * SUMMARY:
 *   This story instantiates the pragmatic development reading of the
 *   software_source_status kernel: open source is valued instrumentally, as
 *   an engineering methodology that produces better software through
 *   distributed peer review, faster bug detection, and higher innovation
 *   velocity — not because proprietary software is unjust
 *   (freedom_imperative_reading) or because creators lack property rights
 *   (property_rights_reading rejects that premise entirely) or because
 *   licensing choice should be optimized case-by-case for aggregate welfare
 *   (utilitarian_hybrid_reading). Under this reading, permissive licensing is
 *   fine, corporate use of open code is fine, and the methodology's
 *   superiority is treated as an empirical engineering claim rather than a
 *   moral one. The measured extraction here is real but modest and rising
 *   over time: it comes from the accumulating gap between the
 *   volunteer/underpaid labor that produces the peer-review benefit and the
 *   commercial capture of that benefit by well-resourced sponsors and
 *   integrators who owe nothing back under this reading's own
 *   permissive-licensing logic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.28).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.18).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Superior Development Methodology (Pragmatic/Instrumental Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_of_technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, 'f4ed838a-0a9c-4072-8d1b-da157f2c0544').
narrative_ontology:cs_kernel_codification('f4ed838a-0a9c-4072-8d1b-da157f2c0544', distributed).
narrative_ontology:cs_authority_grounding('f4ed838a-0a9c-4072-8d1b-da157f2c0544', practice).
narrative_ontology:cs_interpretation_layer_present('f4ed838a-0a9c-4072-8d1b-da157f2c0544').
narrative_ontology:cs_reading_relation('f4ed838a-0a9c-4072-8d1b-da157f2c0544', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4ed838a-0a9c-4072-8d1b-da157f2c0544', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4ed838a-0a9c-4072-8d1b-da157f2c0544', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('f4ed838a-0a9c-4072-8d1b-da157f2c0544', foundational, openness_is_empirically_superior_engineering_practice).
narrative_ontology:cs_axiom_status(openness_is_empirically_superior_engineering_practice, holdable).
narrative_ontology:cs_axiom_grounding('f4ed838a-0a9c-4072-8d1b-da157f2c0544', openness_is_empirically_superior_engineering_practice, empirically_contingent).
narrative_ontology:cs_axiom('f4ed838a-0a9c-4072-8d1b-da157f2c0544', foundational, licensing_choice_is_morally_neutral_business_decision).
narrative_ontology:cs_axiom_status(licensing_choice_is_morally_neutral_business_decision, holdable).
narrative_ontology:cs_axiom_grounding('f4ed838a-0a9c-4072-8d1b-da157f2c0544', licensing_choice_is_morally_neutral_business_decision, instrumental).
narrative_ontology:cs_reference_frame('f4ed838a-0a9c-4072-8d1b-da157f2c0544', distributed_peer_review_engineering_norm).
narrative_ontology:cs_drift_state('f4ed838a-0a9c-4072-8d1b-da157f2c0544', contemporary_platform_capitalism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f4ed838a-0a9c-4072-8d1b-da157f2c0544', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_source_maintainers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, downstream_commercial_integrators).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, corporate_platform_sponsors).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, software_users_relying_on_peer_review).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, unpaid_contributor_labor_pool).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, open_source_maintainers).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, many_eyes_bug_detection_hypothesis).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, permissive_licensing_compatibility_with_quality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set project direction, review policy, and licensing terms for widely-used codebases. They administer the methodology's actual mechanics (code review, CI, issue triage) and collect reputational capital and career leverage from it, but frequently perform substantial unpaid or underpaid labor sustaining infrastructure that commercial actors depend on without contributing proportionally back.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_maintainers, agenda_setter,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, open_source_maintainers, payer).

% Adopt permissively-licensed open source components into commercial products, capturing the velocity and quality benefits of peer review and community bug detection at effectively zero marginal licensing cost. Can fork, redistribute proprietary derivatives, or withdraw sponsorship at will; bear none of the maintenance obligations that fall on maintainers.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, corporate_platform_sponsors, beneficiary,
    institutional, generational, arbitrage, global).

% Build products atop open codebases, benefiting from the quality and security improvements the open development process produces. Free to choose among competing open projects or maintain internal forks; not locked into any single project's governance.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, downstream_commercial_integrators, beneficiary,
    powerful, biographical, mobile, global).

% Volunteer developers, often early-career or hobbyist, whose contributions get absorbed into projects that generate substantial commercial value elsewhere. Their labor is the substrate the 'many eyes' quality claim depends on; burnout and uncompensated maintenance burden are common, and exit means abandoning a project others depend on or watching it decay.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, unpaid_contributor_labor_pool, payer,
    powerless, biographical, constrained, global).

% End users and enterprises who benefit from the security auditability and rapid bug-fixing that open development enables, without needing to participate in or understand the underlying labor structure.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_users_relying_on_peer_review, beneficiary,
    moderate, biographical, mobile, global).

% Compete for the same developer mindshare and enterprise contracts but are structurally disadvantaged in framing debates when 'open is simply better engineering' becomes the default industry narrative; their argument that closed development can match or exceed quality under different incentive structures rarely gets a hearing in this reading's own terms.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_vendors, excluded,
    powerful, biographical, mobile, global).

% Study empirical claims about defect density, time-to-patch, and innovation velocity across open and closed codebases, producing evidence that can support or undercut the pragmatic reading's central methodological claim.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_engineering_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__pragmatic_development_reading, corporate_platform_sponsors).
narrative_ontology:fixing_cost_class(software_source_status__pragmatic_development_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes code review, testing, and bug detection across a large, self-selected pool of contributors, lowering the marginal cost of catching defects and accelerating iteration relative to a closed development process that must fund all review internally.
% TRANSFER_FUNCTION: Moves unpaid or underpaid development, review, and maintenance labor from a distributed volunteer and semi-volunteer contributor base to commercial integrators and platform sponsors who capture the resulting quality and velocity gains in shipped products, largely without proportional compensation flowing back.
% ABSENT_VOICES: Proprietary vendors whose closed-development quality claims are structurally disadvantaged once 'open is better engineering' becomes the default framing; burned-out former maintainers who exited the labor pool are rarely surveyed in the empirical literature this reading cites.
% DISAPPEARANCE_RATIONALE: If the pragmatic-superiority framing vanished overnight, most existing open source projects would continue operating (the practice predates and exceeds the theory), but the rhetorical justification corporate sponsors use to rely on unpaid labor without compensation would lose cover — the underlying labor imbalance would become harder to frame as a natural feature of superior methodology and might attract funding-model reform. Maintainers dispute whether the framing helps them (career signal) or exploits them (justification for non-payment); this is precisely why the verdict is contested rather than settled.
% FOUNDING_PROBLEM: Closed, single-vendor development in the 1980s-90s often meant slow bug-fix cycles, unauditable security flaws, and vendor lock-in; open development was proposed as a methodology, independent of ethical claims about freedom, for producing more reliable and faster-iterating software by distributing review and testing widely.
% FOUNDING_PROBLEM_CORROBORATION: Empirical software engineering research (defect-density studies, CVE patch-time comparisons) partially corroborates the methodological claim independent of any beneficiary's interest. However, labor economists studying open source maintenance burden and several former maintainers writing publicly about burnout attest that the 'superior methodology' framing is now also used by well-resourced corporate beneficiaries to justify continued non-payment for labor whose founding coordination problem (distributed peer review) remains genuinely live but whose compensation problem was never solved and is arguably worsening.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, contested).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).
:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.12) and rises modestly to 0.28 over the interval, reflecting a real but not severe transfer: as more commercial value concentrates around open codebases, the compensation gap for maintainers widens, but nothing in this reading requires suppression or exit-blocking — contributors can leave, projects can be forked, and no one is coerced into contributing. Suppression is low (0.18) because this reading, unlike the freedom_imperative reading, makes no claim that closed alternatives are illegitimate — proprietary software coexists freely. Theater ratio is low-moderate and slowly rising (0.08 to 0.22) reflecting some drift toward performative 'open-source-friendly' corporate sponsorship programs that provide visibility without proportional funding.
 *
 * DIRECTIONALITY LOGIC:
 *   Maintainers sit near the payer end structurally (low power, constrained exit, labor absorbed by others) despite nominally being agenda-setters over their own projects — their agenda-setting power is real but does not translate into capture of the downstream commercial value their labor generates. Corporate sponsors and downstream integrators sit at the beneficiary end: arbitrage-grade exit, institutional power, and they capture quality/velocity gains without bearing the maintenance cost. This is the central asymmetry this reading's own metrics register even though the reading itself does not treat this asymmetry as illegitimate (that judgment belongs to the freedom_imperative or utilitarian_hybrid readings, not this one).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (slow, unauditable, vendor-locked closed development) remains partially live per corroborating empirical software-engineering research — this prevents the constraint from being classified as pure zombie mandate. But the compensation dimension of the coordination problem was never solved and the framing is now also doing cover-story work for uncompensated labor extraction, which is why founding_problem_status is authored as contested rather than simply live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodology_claim_vs_labor_extraction_boundary,
    'Is the empirical ''many eyes'' quality claim genuinely separable from the labor-compensation problem, or does the pragmatic-methodology framing function partly as a rhetorical device that naturalizes uncompensated labor by redescribing it as a superior process rather than an economic relationship?',
    'Comparative analysis of defect rates and patch velocity in fully-funded open projects (foundation-backed, paid maintainers) versus volunteer-dependent projects; if quality benefits persist under paid models, the methodology claim is separable from the extraction question and the extraction is a contingent funding failure, not intrinsic to the methodology.',
    'If separable, this reading''s low ε is well-supported and the extraction is a fixable funding gap. If inseparable — if the methodology''s cost-effectiveness specifically depends on labor being uncompensated — the pragmatic reading is quietly doing extraction-legitimizing work it does not acknowledge, and ε would need to be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_claim_vs_labor_extraction_boundary, empirical, 'Whether the instrumental quality claim depends structurally on uncompensated labor.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the four kernel readings (freedom_imperative, pragmatic_development, property_rights, utilitarian_hybrid) disagree, structurally?',
    'This is not resolvable by data — it is the committer structure itself. The freedom_imperative_reading treats proprietary restriction as intrinsically unjust regardless of engineering outcomes; this pragmatic reading treats restriction as a legitimate business choice that merely tends to underperform openness on certain quality metrics. The property_rights_reading treats the entire quality-comparison framing as secondary to a prior claim about creator entitlement that this reading does not contest but also does not need. The utilitarian_hybrid_reading would fold this reading''s empirical claim into a context-dependent welfare calculus this reading declines to perform, preferring a general methodological verdict.',
    'A reader who conflates this reading with freedom_imperative_reading would wrongly import a moral-illegitimacy claim about proprietary software that this reading explicitly does not make; a reader who conflates it with property_rights_reading would wrongly assume this reading is indifferent to engineering outcomes, when the whole point is an empirical superiority claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the structural disagreement among sibling kernel readings to prevent conflation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__pragmatic_development_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(soft_tr_t5, software_source_status__pragmatic_development_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(soft_tr_t10, software_source_status__pragmatic_development_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(soft_tr_t15, software_source_status__pragmatic_development_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(soft_tr_t20, software_source_status__pragmatic_development_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(soft_tr_t25, software_source_status__pragmatic_development_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__pragmatic_development_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(soft_be_t5, software_source_status__pragmatic_development_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(soft_be_t10, software_source_status__pragmatic_development_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(soft_be_t15, software_source_status__pragmatic_development_reading, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(soft_be_t20, software_source_status__pragmatic_development_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(soft_be_t25, software_source_status__pragmatic_development_reading, base_extractiveness, 25, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_source_status__pragmatic_development_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__pragmatic_development_reading, 0.12).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the software_source_status kernel, each authored as an independent constraint with its own ε, beneficiary/victim structure, and stakeholders. The pragmatic_development_reading (this file) authors the lowest ε among the four because it makes no moral claim against proprietary software and treats the labor-compensation gap as a fixable funding problem rather than an injustice or a property violation. freedom_imperative_reading would author higher suppression/extractiveness on the same underlying practice because it treats proprietary restriction itself as the extraction. property_rights_reading inverts the beneficiary/victim structure, treating open-source normative pressure on proprietary vendors as the extraction. utilitarian_hybrid_reading would author a context-dependent ε that varies by deployment context rather than a single scalar.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
