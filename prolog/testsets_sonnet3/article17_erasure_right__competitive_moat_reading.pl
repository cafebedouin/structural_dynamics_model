% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__competitive_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__competitive_moat_reading, []).

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
 *   constraint_id: article17_erasure_right__competitive_moat_reading
 *   human_readable: GDPR Article 17 (Right to Erasure) as Incumbent-Protecting Compliance Moat
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   This story instantiates the competitive_moat_reading of the Article 17
 *   kernel: the erasure right is read not primarily as a censorship vector or
 *   a privacy entitlement, but as a compliance-cost structure whose
 *   enforcement architecture (cross-system propagation, backup deletion,
 *   third-party notification) imposes fixed costs that are proportionally far
 *   heavier on new entrants than on incumbents who already amortized the
 *   required infrastructure. Under this reading, the same legal text that
 *   empowers individual data subjects also functions as a de facto barrier to
 *   entry in data-intensive markets, entrenching the market position of the
 *   controllers best equipped to absorb compliance overhead. This is a
 *   distinct constraint from the privacy_fundamental_reading (which treats
 *   the individual entitlement as the referent and finds low extraction) and
 *   from the censorship_mechanism_reading (which treats strategic erasure
 *   requests against speech as the referent). All three share the kernel text
 *   but diverge sharply in ε, beneficiary/victim structure, and
 *   classification, per the ε-invariance principle — they are linked via
 *   network.affects_constraints rather than merged into one story.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, 0.66).
domain_priors:suppression_score(article17_erasure_right__competitive_moat_reading, 0.58).
domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__competitive_moat_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__competitive_moat_reading, "GDPR Article 17 (Right to Erasure) as Incumbent-Protecting Compliance Moat").
narrative_ontology:topic_domain(article17_erasure_right__competitive_moat_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__competitive_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__competitive_moat_reading, '6c2eae6f-9461-463a-a87b-a5bde5ae244f').
narrative_ontology:cs_kernel_codification('6c2eae6f-9461-463a-a87b-a5bde5ae244f', formalized).
narrative_ontology:cs_authority_grounding('6c2eae6f-9461-463a-a87b-a5bde5ae244f', lineage).
narrative_ontology:cs_interpretation_layer_present('6c2eae6f-9461-463a-a87b-a5bde5ae244f').
narrative_ontology:cs_reading_relation('6c2eae6f-9461-463a-a87b-a5bde5ae244f', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c2eae6f-9461-463a-a87b-a5bde5ae244f', article17_erasure_right__censorship_mechanism_reading, influences).
narrative_ontology:cs_axiom('6c2eae6f-9461-463a-a87b-a5bde5ae244f', foundational, compliance_burden_proportionality_required).
narrative_ontology:cs_axiom_status(compliance_burden_proportionality_required, holdable).
narrative_ontology:cs_axiom_grounding('6c2eae6f-9461-463a-a87b-a5bde5ae244f', compliance_burden_proportionality_required, empirically_contingent).
narrative_ontology:cs_axiom('6c2eae6f-9461-463a-a87b-a5bde5ae244f', secondary, uniform_obligation_produces_differential_market_effect).
narrative_ontology:cs_axiom_status(uniform_obligation_produces_differential_market_effect, holdable).
narrative_ontology:cs_axiom_grounding('6c2eae6f-9461-463a-a87b-a5bde5ae244f', uniform_obligation_produces_differential_market_effect, empirically_contingent).
narrative_ontology:cs_reference_frame('6c2eae6f-9461-463a-a87b-a5bde5ae244f', harmonized_data_protection_baseline).
narrative_ontology:cs_drift_state('6c2eae6f-9461-463a-a87b-a5bde5ae244f', post_schrems_enforcement_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6c2eae6f-9461-463a-a87b-a5bde5ae244f', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__competitive_moat_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, large_platform_incumbents).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, compliance_technology_vendors).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, established_data_controllers).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, startup_data_processors).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, small_ad_tech_challengers).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, open_source_data_projects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, individual_data_subjects).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, established_data_controllers).
narrative_ontology:constraint_vindicates(article17_erasure_right__competitive_moat_reading, data_subject_erasure_entitlement_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate mature, already-amortized data infrastructure with dedicated erasure-propagation pipelines, legal teams, and automated deletion tooling across distributed systems, backups, and third-party data-sharing agreements. The fixed cost of building erasure-compliant architecture was absorbed years ago and is now a small marginal cost against enormous revenue. Compliance itself becomes a market signal ('we take privacy seriously') that reinforces trust and lock-in with regulators and users alike.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, large_platform_incumbents, beneficiary,
    institutional, generational, arbitrage, continental).

% Sell erasure-orchestration software, data-mapping audits, and 'right to be forgotten' pipelines to companies that cannot build in-house tooling. Revenue scales directly with the complexity and universality of the erasure mandate; they have no incentive to see the compliance burden simplified.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, compliance_technology_vendors, beneficiary,
    organized, biographical, mobile, continental).

% Mid-to-large firms that have already built erasure-compliant data architectures. They bear ongoing compliance costs but those costs are now a sunk, amortized part of operations — and the same costs, imposed freshly on a new entrant, function as a barrier that protects the controller's market position from that entrant.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, established_data_controllers, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, established_data_controllers, payer).

% Early-stage companies handling personal data must build erasure-propagation capability across every system, backup, log, and third-party integration from day one, or face fines up to 4% of global turnover. The fixed cost of building compliant deletion architecture is the same in absolute terms as for an incumbent, but represents a vastly larger share of limited runway — some delay product launches or avoid EU markets entirely to dodge the requirement.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, startup_data_processors, payer,
    moderate, immediate, constrained, national).

% Compete against dominant ad-tech incumbents for advertiser budgets, but erasure obligations across programmatic bidding chains, data brokers, and re-targeting pipelines require infrastructure investment that only the largest players can absorb. Several have exited the EU market or been acquired by incumbents who can absorb the compliance cost, rather than compete on it.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, small_ad_tech_challengers, payer,
    powerless, immediate, trapped, national).

% Volunteer-maintained or thinly-resourced data tools and federated services (e.g., small social platforms, research data commons) that process personal data have no legal or engineering capacity to build robust erasure-propagation systems across forks, mirrors, and distributed nodes. Many simply cease EU operations or restrict features rather than risk non-compliance exposure.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, open_source_data_projects, payer,
    powerless, immediate, trapped, national).

% Exercise the erasure right against companies holding their data. Genuinely benefit from the right's existence in principle, but in this reading their exercise of the right is also the lever that entrenches whichever controller already has the resources to comply cheaply — they are the coordination function that the extraction rides on, not the extraction's target.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, individual_data_subjects, beneficiary,
    powerless, biographical, constrained, continental).

% Draft, interpret, and enforce Article 17 obligations, including guidance on what counts as adequate erasure across backups and third parties. Have limited visibility into differential compliance costs across firm size and have not built graduated obligations that would relieve the disproportionate burden on small entrants.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, eu_data_protection_regulators, agenda_setter,
    institutional, generational, analytical, continental).

% Investigate market concentration in digital markets and could, in principle, examine whether regulatory compliance costs function as a barrier to entry, but data protection and competition enforcement operate in separate institutional silos with little coordination on this specific dynamic.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, eu_competition_authorities, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__competitive_moat_reading, large_platform_incumbents).
narrative_ontology:fixing_cost_class(article17_erasure_right__competitive_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article 17 solves a genuine collective-action problem: without a legal erasure entitlement, individuals have no leverage to compel any given data controller to delete their information, since unilateral deletion requests can simply be ignored by any single firm with no penalty.
% TRANSFER_FUNCTION: Under this reading, compliance-cost asymmetry moves competitive advantage from resource-constrained new entrants to resource-rich incumbents: the same nominal obligation (build and maintain full erasure-propagation infrastructure) costs a smaller company a much larger share of its viable capital than it costs an incumbent, so market share flows toward whoever can already absorb the fixed cost.
% ABSENT_VOICES: Failed and never-launched EU market entrants are not represented in any policy consultation record — a startup that decided not to launch in the EU because of Article 17 compliance costs leaves no complaint, no regulatory filing, and no visible trace, so the barrier-to-entry effect is structurally undercounted in every review of the regulation's impact.
% DISAPPEARANCE_RATIONALE: Individual data subjects would lose a real and valued legal entitlement if Article 17 vanished, so the world would rearrange for them. But under this reading, if the specific compliance-cost structure (full-propagation, cross-system, cross-backup erasure obligations) were replaced by a lighter-touch equivalent achieving the same subject-facing outcome, the competitive landscape would rearrange substantially — challengers could enter markets currently foreclosed to them — while data subjects would notice little difference in their erasure experience. The contest is over whether the current implementation's stringency is necessary to the coordination function or is separable from it.
% FOUNDING_PROBLEM: Individuals had no enforceable mechanism to compel deletion of personal data held by companies, especially data that had been shared, sold, cached, or replicated across the internet after the individual lost any relationship with the original collector.
% FOUNDING_PROBLEM_CORROBORATION: Privacy advocacy organizations and the regulators who drafted the provision attest the founding problem remains fully live and the right is functioning as intended. Independent competition economists and digital-market researchers outside both the regulatory and privacy-advocacy communities attest that whatever the founding problem's continued vitality, the specific compliance architecture has developed a documented secondary effect — favoring incumbents with amortized infrastructure — that was not the provision's design intent and is not tracked by the bodies that administer it.
narrative_ontology:disappearance_verdict(article17_erasure_right__competitive_moat_reading, contested).
narrative_ontology:founding_problem_status(article17_erasure_right__competitive_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__competitive_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__competitive_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__competitive_moat_reading, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__competitive_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__competitive_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.66) reflects that market position measurably shifts toward incumbents as compliance-technology costs and case law interpretation (e.g., cross-border erasure propagation standards) have hardened since 2018 — the temporal series shows extraction rising as enforcement guidance solidified and case law (e.g., on backup deletion, search delisting scope) accumulated additional technical requirements. Suppression (0.58) is moderate: the barrier is not enforced coercively against challengers directly, but operates by making market entry commercially irrational for firms that cannot absorb the fixed cost — a structural rather than a punitive suppression. Theater ratio (0.42) is meaningfully elevated because a portion of large-incumbent compliance activity (privacy dashboards, transparency reports, certification badges) functions as market-differentiation signaling layered atop the substantive erasure-propagation work, rather than purely serving data subjects.
 *
 * DIRECTIONALITY LOGIC:
 *   Large incumbents and compliance vendors sit near the full-beneficiary end: the same nominal obligation generates a relative advantage for them because their marginal compliance cost is near zero while their competitors' is prohibitive. Individual data subjects are also coded as beneficiaries — their entitlement is genuinely exercised and genuinely serves the coordination function — but under this reading they are the mechanism by which extraction operates on the challenger seats, not a target of extraction themselves. Startups, small ad-tech firms, and open-source data projects sit near the full-target end: they bear the same absolute compliance cost as incumbents but on a fraction of the capital base, and their exit options are constrained-to-trapped because avoiding the EU market entirely forfeits access to it rather than escaping the cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (individuals lack leverage to compel deletion) remains genuinely live — this is why the classification is tangled_rope rather than snare: there is real, uncontested coordination function alongside the asymmetric extraction. Reading it as pure extraction would mislabel a right that demonstrably serves data subjects; reading it as pure coordination would erase the documented, measurable competitive-entrenchment effect this story exists to isolate. The tangled_rope classification requires exactly this coexistence: named beneficiaries (incumbents, vendors), named victims (challengers), active enforcement (regulatory fines and audit regimes), and a genuine coordination function (data subject erasure) all present simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_cost_separability,
    'Is the incumbent-favoring compliance-cost structure a necessary feature of enforcing a meaningful erasure right, or is it a separable implementation choice (e.g., graduated obligations by firm size, or simplified deletion standards for smaller processors) that could preserve the coordination function while removing the barrier-to-entry effect?',
    'Comparative regulatory analysis of jurisdictions or later amendments that have introduced tiered/proportionate compliance obligations by firm size or data volume, tracking whether erasure efficacy (subject-facing outcomes) is preserved when compliance burden is redistributed.',
    'If separable, the extraction measured here is an artifact of implementation choices rather than an inherent feature of the erasure right, supporting reform toward proportionate obligations. If inseparable, the compliance cost is the necessary price of a robust erasure guarantee and the barrier-to-entry effect is a genuine tradeoff rather than a removable defect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_separability, empirical, 'Whether Article 17''s incumbent-favoring cost structure is necessary to its coordination function or a separable implementation artifact.').

omega_variable(
    kernel_reading_coexistence,
    'Do the three readings of Article 17 (competitive_moat, privacy_fundamental, censorship_mechanism) describe genuinely independent structural effects that coexist simultaneously in the same legal text''s operation, or does one reading''s mechanism actually explain or subsume another''s (e.g., is the censorship effect a downstream consequence of the same compliance-cost dynamics analyzed here)?',
    'Structural tracing of specific erasure-request case studies to determine whether cases classified under the censorship_mechanism_reading share the same technical/cost mechanism analyzed in this story, or arise from a distinct legal-strategic mechanism (e.g., reputation-management requests exploiting search delisting).',
    'If the mechanisms are genuinely independent, the three-way decomposition is correct and each story''s ε stands alone. If one reading''s mechanism subsumes another''s, the kernel decomposition may need revision — potentially merging two readings or adding a fourth to isolate the shared mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether the three kernel readings are structurally independent or share an underlying mechanism that the decomposition has not yet isolated.').

omega_variable(
    regulatory_awareness_gap,
    'Do EU data protection regulators and competition authorities lack awareness of the compliance-cost-asymmetry effect, or are they aware but have made a deliberate policy tradeoff favoring robust individual entitlement over market-entry ease?',
    'Review of regulatory impact assessments, parliamentary debate records, and any competition-authority correspondence addressing Article 17''s market-structure effects; interviews or freedom-of-information requests targeting whether this tradeoff was explicitly considered during drafting or subsequent guidance revisions.',
    'If the effect is unrecognized, this constitutes a genuine policy blind spot correctable through improved impact assessment. If it is a deliberate tradeoff, the tangled_rope classification is a known and accepted cost of the coordination function rather than an oversight, changing the normative valence of the mandatrophy analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_awareness_gap, empirical, 'Whether regulators are aware of the compliance-cost asymmetry effect or have made a deliberate tradeoff.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__competitive_moat_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2018, article17_erasure_right__competitive_moat_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(arti_tr_t2019, article17_erasure_right__competitive_moat_reading, theater_ratio, 2019, 0.29).
narrative_ontology:measurement(arti_tr_t2020, article17_erasure_right__competitive_moat_reading, theater_ratio, 2020, 0.33).
narrative_ontology:measurement(arti_tr_t2022, article17_erasure_right__competitive_moat_reading, theater_ratio, 2022, 0.37).
narrative_ontology:measurement(arti_tr_t2024, article17_erasure_right__competitive_moat_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement(arti_tr_t2026, article17_erasure_right__competitive_moat_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t2018, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2018, 0.42).
narrative_ontology:measurement(arti_be_t2019, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2019, 0.47).
narrative_ontology:measurement(arti_be_t2020, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2020, 0.53).
narrative_ontology:measurement(arti_be_t2022, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2022, 0.59).
narrative_ontology:measurement(arti_be_t2024, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2024, 0.63).
narrative_ontology:measurement(arti_be_t2026, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2026, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2018, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement(arti_su_t2019, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2019, 0.44).
narrative_ontology:measurement(arti_su_t2020, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement(arti_su_t2022, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2022, 0.53).
narrative_ontology:measurement(arti_su_t2024, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2024, 0.56).
narrative_ontology:measurement(arti_su_t2026, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__competitive_moat_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__competitive_moat_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single Article 17 kernel text under the ε-invariance principle. The competitive_moat_reading (this story) authors high extraction (0.66) with incumbents as beneficiaries and challengers as victims, classified tangled_rope. The privacy_fundamental_reading authors low extraction with data subjects as the primary beneficiary and no clear victim, plausibly rope or near-mountain. The censorship_mechanism_reading authors high extraction with speakers/publishers as victims and strategic requesters as beneficiaries, plausibly snare or tangled_rope. All three share the same legal text as their nominal referent but have different ε values, different stakeholder sets, and different classifications — they are not the same constraint measured three ways; they are three constraints sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
