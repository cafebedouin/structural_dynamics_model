% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__competitive_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Article 17 Erasure Right as Competitive Moat
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   Article 17 GDPR (Right to Erasure) is read here as a constraint whose
 *   operational effect amplifies incumbent advantage through asymmetric
 *   compliance costs. The regulation mandates that data controllers honor
 *   erasure requests within 30 days, verify identity, notify downstream
 *   processors, and maintain audit trails. Incumbents (Meta, Google, X,
 *   TikTok) have spent billions building this infrastructure; for them the
 *   marginal cost per request is negligible. Challengers face the full fixed
 *   cost upfront — engineering teams, legal review, vendor contracts — before
 *   serving a single user. The coordination function (individual data
 *   control) is real but the extraction function (barrier to entry) is
 *   structurally inseparable from the compliance architecture. This reading
 *   does not claim Article 17 was *designed* as incumbent protection; it
 *   claims the constraint's *operation* in the current market structure
 *   extracts from challengers and subsidizes incumbents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, 0.68).
domain_priors:suppression_score(article17_erasure_right__competitive_moat_reading, 0.55).
domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__competitive_moat_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__competitive_moat_reading, "Article 17 Erasure Right as Competitive Moat").
narrative_ontology:topic_domain(article17_erasure_right__competitive_moat_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__competitive_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__competitive_moat_reading, '98179a03-afab-47f6-8c46-6e929e7541a3').
narrative_ontology:cs_kernel_codification('98179a03-afab-47f6-8c46-6e929e7541a3', formalized).
narrative_ontology:cs_authority_grounding('98179a03-afab-47f6-8c46-6e929e7541a3', lineage).
narrative_ontology:cs_interpretation_layer_present('98179a03-afab-47f6-8c46-6e929e7541a3').
narrative_ontology:cs_reading_relation('98179a03-afab-47f6-8c46-6e929e7541a3', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('98179a03-afab-47f6-8c46-6e929e7541a3', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('98179a03-afab-47f6-8c46-6e929e7541a3', foundational, compliance_cost_asymmetry_creates_structural_advantage).
narrative_ontology:cs_axiom_status(compliance_cost_asymmetry_creates_structural_advantage, holdable).
narrative_ontology:cs_axiom_grounding('98179a03-afab-47f6-8c46-6e929e7541a3', compliance_cost_asymmetry_creates_structural_advantage, empirically_contingent).
narrative_ontology:cs_axiom('98179a03-afab-47f6-8c46-6e929e7541a3', foundational, regulation_can_function_as_competitive_moat).
narrative_ontology:cs_axiom_status(regulation_can_function_as_competitive_moat, holdable).
narrative_ontology:cs_axiom_grounding('98179a03-afab-47f6-8c46-6e929e7541a3', regulation_can_function_as_competitive_moat, instrumental).
narrative_ontology:cs_reference_frame('98179a03-afab-47f6-8c46-6e929e7541a3', data_sovereignty_regulatory_baseline).
narrative_ontology:cs_drift_state('98179a03-afab-47f6-8c46-6e929e7541a3', post_gdpr_implementation_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('98179a03-afab-47f6-8c46-6e929e7541a3', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__competitive_moat_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, incumbent_platforms).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, established_social_media).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, large_tech_corps_with_compliance_infrastructure).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, new_entrants).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, small_platforms).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, startup_social_media).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, resource_constrained_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, compliance_tool_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess mature content moderation pipelines, automated erasure workflows, and legal teams to handle Article 17 requests at scale. The compliance cost is a marginal operational expense. They benefit when rivals cannot afford equivalent infrastructure, cementing market position.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, incumbent_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Shape regulatory guidance through lobbying and standards bodies. Their existing compliance architecture becomes the de facto standard. They absorb compliance cost as barrier-to-entry amplification against challengers.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, established_social_media, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, established_social_media, agenda_setter).

% Cloud providers and enterprise platforms with pre-built data governance stacks. They sell compliance-as-a-service to smaller players, extracting rent from the constraint they help sustain. Not directly regulated as hosts but profit from the compliance ecosystem.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, large_tech_corps_with_compliance_infrastructure, beneficiary,
    powerful, biographical, mobile, global).

% Must build erasure request handling, verification workflows, audit trails, and dispute resolution from scratch before launch. Capital requirement exceeds seed funding for many categories. Non-compliance risk includes fines up to 4% global revenue — existential for early-stage companies.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, new_entrants, payer,
    powerless, immediate, trapped, global).

% Operating with thin margins; compliance team hiring competes directly with product development. Many shut down user-generated features or exit markets rather than build erasure infrastructure. Those that persist pay disproportionate revenue share to compliance.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, small_platforms, payer,
    moderate, biographical, constrained, regional).

% Network effects require open participation, but Article 17 makes every user a potential compliance trigger. The cost curve is convex: first million users cost more per capita than the next hundred million. Incumbents have already paid the fixed cost; challengers face it upfront.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, startup_social_media, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, startup_social_media, excluded).

% Independent builders of niche communities, forums, tools. Cannot afford dedicated legal/compliance staff. Rely on third-party compliance services that add per-request fees. Some abandon EU markets entirely — a structural exit that incumbents do not face.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, resource_constrained_developers, payer,
    powerless, immediate, constrained, local).

% Enforce Article 17 through investigations, fines, and guidance. Their interpretation shapes the compliance burden. Resource asymmetry: they target high-profile cases (often incumbents) but the regulatory text applies uniformly, disproportionately burdening those without compliance infrastructure.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Sell automated erasure workflows, request management dashboards, audit log generators. Their business model exists because Article 17 created a mandatory purchase category. They lobby for expansive interpretation to grow the addressable market.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, compliance_tool_vendors, beneficiary,
    organized, biographical, mobile, global).

% Investigate whether data protection law creates anti-competitive effects. Have opened market studies but no enforcement actions yet. Their remedial power could restructure the compliance cost curve but operates on a slower timeline than market exit.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, competition_authorities, observer,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized mechanism for individuals to request deletion of their personal data from platform databases, reducing indefinite retention and enabling data sovereignty.
% TRANSFER_FUNCTION: Moves compliance capital (engineering, legal, operational) from challengers to incumbents and compliance vendors. Incumbents pay marginal cost; challengers pay full fixed cost. Compliance vendors extract rent from both. Regulatory risk (fines) transfers from platforms with infrastructure to those without.
% ABSENT_VOICES: Would-be founders who never launch because Article 17 compliance raises minimum viable product cost above available capital. Users on platforms that never form. Competitors in non-EU jurisdictions who face the same barrier to enter the EU market. These voices are absent because the constraint prevents their emergence — they are not excluded from a conversation, they are excluded from the market.
% DISAPPEARANCE_RATIONALE: If Article 17 vanished overnight, the fixed compliance cost barrier would collapse. New entrants would launch user-generated features without erasure infrastructure. Incumbents would lose a structural advantage. Compliance tool vendors would lose their mandatory market. The competitive landscape would reorganize around product differentiation rather than regulatory moats.
% FOUNDING_PROBLEM: Individuals had no effective remedy when platforms retained personal data indefinitely, sold it without consent, or refused deletion requests. The power asymmetry between data subjects and data controllers was absolute.
% FOUNDING_PROBLEM_CORROBORATION: Privacy advocates and data protection authorities attest the founding problem remains live: platforms still over-retain, dark patterns obstruct deletion, and enforcement is uneven. Competition economists from OECD and national authorities corroborate that the compliance cost curve now functions as a barrier to entry — a finding outside the benefiting parties (incumbents and compliance vendors) that confirms dual function.
narrative_ontology:disappearance_verdict(article17_erasure_right__competitive_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__competitive_moat_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__competitive_moat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article17_erasure_right__competitive_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__competitive_moat_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the capital transfer from entrants to incumbents/vendors via mandatory compliance spend. Suppression (0.55) is moderate: alternatives exist (don't operate in EU, don't host user content, use compliance vendors) but each entails competitive sacrifice. Theater ratio (0.42) captures the growing gap between the regulation's stated purpose (user empowerment) and its structural effect (market consolidation) — compliance vendors and incumbents publicly champion 'privacy compliance' while lobbying against interoperability mandates that would lower the barrier. Accessibility collapse (0.35) is low because the constraint doesn't foreclose all alternatives — niche platforms survive by avoiding user-generated content or geofencing EU. Resistance (0.60) is high: startup mortality, market exit, and competition authority scrutiny all push back.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent seat, Article 17 is a coordination cost of doing business — a Rope they've mastered. From the challenger seat, it's a Snare — a fixed cost that extracts capital without delivering proportional user value. From the compliance vendor seat, it's a Rope they sell. The engine computes per-seat types from the same structural data; this divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents and compliance vendors are structural beneficiaries (d near 0): they collect rents from the constraint's operation. Challengers are structural targets (d near 1): they bear the full fixed cost. Data protection authorities sit near symmetric (d ~ 0.5): they enforce the constraint but also bear political cost when enforcement visibly consolidates markets. Competition authorities are analytical observers (d = 0.5 by definition). The derivation chain from beneficiary/victim declarations + power + exit produces this gradient without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncontrolled data retention) remains live — platforms still over-retain and under-delete. But the constraint's *current* extraction profile (barrier to entry) has diverged from its *founding* extraction profile (individual remedy). This is not mandatrophy in the classic sense (function vanished, form persists) but function drift: the coordination function persists while an extraction function has grown around it. The mandate has not atrophied; it has been captured by a secondary function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_cost_curve_shape,
    'Is the compliance cost curve genuinely convex (high fixed cost, low marginal cost) or can modular tooling flatten it for new entrants?',
    'Empirical study of actual compliance spend by platform age/size cohort, controlling for feature set. Track whether compliance-as-a-service vendors reduce the fixed cost below the ''build from scratch'' threshold.',
    'If convex, the barrier is structural and the tangled_rope classification holds. If modular tooling flattens the curve, the extraction diminishes and the constraint trends toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_curve_shape, empirical, 'Whether the compliance cost asymmetry is a durable structural feature or a transient tooling gap.').

omega_variable(
    coordination_extraction_separability,
    'Can the erasure right''s coordination function (user data control) be implemented without the extraction function (incumbent barrier)?',
    'Counterfactual design: specify an interoperable erasure protocol (e.g., standardized API, shared verification layer, portable deletion receipts) and measure adoption cost for a greenfield platform vs. incumbent retrofit.',
    'If separable, the extraction is a design choice, not a necessity — policy could mandate interoperability. If inseparable, the tangled_rope is intrinsic to the right''s architecture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s dual function is structurally necessary or contingently coupled.').

omega_variable(
    reading_contest_location,
    'Where exactly do the three readings of Article 17 disagree structurally?',
    'Map each reading''s beneficiary/victim declarations, claimed_type, and ε. The privacy reading names data_subjects as beneficiaries, platforms as payers. The censorship reading names political_actors as beneficiaries, speakers as victims. This reading names incumbents as beneficiaries, challengers as victims. The disagreement is in the beneficiary/victim structure — each reading sees a different transfer function in the same text.',
    'Confirms this is a kernel reading set (one text, multiple constraints) not a measurement dispute. Each reading should be authored as a separate constraint story with its own ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_location, conceptual, 'Structural location of the kernel reading disagreement: beneficiary/victim structure and transfer function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__competitive_moat_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2018, article17_erasure_right__competitive_moat_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(arti_tr_t2020, article17_erasure_right__competitive_moat_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(arti_tr_t2022, article17_erasure_right__competitive_moat_reading, theater_ratio, 2022, 0.3).
narrative_ontology:measurement(arti_tr_t2024, article17_erasure_right__competitive_moat_reading, theater_ratio, 2024, 0.38).
narrative_ontology:measurement(arti_tr_t2025, article17_erasure_right__competitive_moat_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement(arti_tr_t2026, article17_erasure_right__competitive_moat_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t2018, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement(arti_be_t2020, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(arti_be_t2022, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2022, 0.55).
narrative_ontology:measurement(arti_be_t2024, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement(arti_be_t2025, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement(arti_be_t2026, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2018, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement(arti_su_t2020, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(arti_su_t2022, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2022, 0.5).
narrative_ontology:measurement(arti_su_t2024, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement(arti_su_t2025, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2025, 0.54).
narrative_ontology:measurement(arti_su_t2026, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__competitive_moat_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, gdpr_compliance_ecosystem).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, digital_markets_act_interoperability).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, eu_startup_ecosystem_viability).

% DUAL FORMULATION NOTE:
% Article 17 erasure right decomposes into three constraint stories: privacy_fundamental_reading (Mountain/Rope hybrid — low extraction, rights coordination), censorship_mechanism_reading (Snare — high extraction, speech suppression), competitive_moat_reading (this story — Tangled Rope — coordination + incumbent barrier). The ε values differ substantially: privacy reading ε ≈ 0.15, censorship reading ε ≈ 0.75, this reading ε ≈ 0.68. They share the same statutory text but instantiate different constraints because the transfer function differs by reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
