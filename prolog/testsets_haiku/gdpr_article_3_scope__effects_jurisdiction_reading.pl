% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__effects_jurisdiction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Effects-Based Jurisdiction Over Non-EU Controllers
 *   domain: regulatory/international/privacy
 *
 * SUMMARY:
 *   GDPR Article 3(2) establishes that the regulation applies to processing
 *   of personal data of data subjects in the Union by a controller or
 *   processor not established in the Union, where the processing activities
 *   are related to: (a) the offering of goods or services to data subjects in
 *   the Union; or (b) the monitoring of their behavior. This reading
 *   instantiates the effects-jurisdiction interpretation: jurisdiction
 *   follows the effects on EU residents, not the controller's physical
 *   location. The regulation asserts extraterritorial authority over non-EU
 *   entities based on targeting or monitoring EU residents. The constraint is
 *   CLAIMED as tangled rope (genuine coordination of data protection +
 *   asymmetric extraction of compliance costs from non-EU controllers). The
 *   authored metrics describe substantial extraction (0.68) and active
 *   enforcement (suppression 0.52), with growing theater over time (0.28
 *   endpoint) as regulatory interpretations expand monitoring/targeting
 *   definitions. The measurement series tracks extractiveness rising as
 *   enforcement guidance tightens and theater rising as borderline cases are
 *   interpreted expansively. This is one reading of a contested kernel: the
 *   territorial sovereignty reading would locate jurisdiction within EU
 *   borders only; the market-access reading would frame GDPR as a conditional
 *   access requirement rather than a jurisdictional assertion.
 *
 * KEY AGENTS:
 *   - EU Regulatory Authorities (DPA network, EDPB): Set and enforce the effects-jurisdiction test; interpret targeting/monitoring; issue guidance; impose fines.
 *   - Non-EU Controllers (tech platforms, SaaS providers): Bear compliance costs; navigate targeting/monitoring ambiguities; face enforcement exposure.
 *   - Third-Country Service Providers: Smaller providers; identity-locked into EU-dependent business models; face cascading compliance pressure.
 *   - Third-Country Governments: Structurally excluded; experience as regulatory imperialism; limited policy responses.
 *   - EU Residents (data subjects): Formal beneficiaries of rights; actual enforcement barriers; scattered collective action.
 *   - Territorial Sovereignty Reading: Competing interpretation; excluded from consensus; remains live in policy discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.68).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.52).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Effects-Based Jurisdiction Over Non-EU Controllers").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "regulatory/international/privacy").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, 'c7eff006-1cc6-42c3-aefa-960ef43e55bc').
narrative_ontology:cs_kernel_codification('c7eff006-1cc6-42c3-aefa-960ef43e55bc', fixed_text).
narrative_ontology:cs_authority_grounding('c7eff006-1cc6-42c3-aefa-960ef43e55bc', extraction).
narrative_ontology:cs_interpretation_layer_present('c7eff006-1cc6-42c3-aefa-960ef43e55bc').
narrative_ontology:cs_reading_relation('c7eff006-1cc6-42c3-aefa-960ef43e55bc', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('c7eff006-1cc6-42c3-aefa-960ef43e55bc', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('c7eff006-1cc6-42c3-aefa-960ef43e55bc', foundational, jurisdiction_follows_effects_on_persons).
narrative_ontology:cs_axiom_status(jurisdiction_follows_effects_on_persons, holdable).
narrative_ontology:cs_axiom_grounding('c7eff006-1cc6-42c3-aefa-960ef43e55bc', jurisdiction_follows_effects_on_persons, deontological).
narrative_ontology:cs_axiom('c7eff006-1cc6-42c3-aefa-960ef43e55bc', foundational, fundamental_right_transcends_territorial_boundaries).
narrative_ontology:cs_axiom_status(fundamental_right_transcends_territorial_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('c7eff006-1cc6-42c3-aefa-960ef43e55bc', fundamental_right_transcends_territorial_boundaries, deontological).
narrative_ontology:cs_reference_frame('c7eff006-1cc6-42c3-aefa-960ef43e55bc', effects_based_jurisdictional_authority).
narrative_ontology:cs_drift_state('c7eff006-1cc6-42c3-aefa-960ef43e55bc', contemporary_targeting_monitoring_expansion, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('c7eff006-1cc6-42c3-aefa-960ef43e55bc', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subject_protection_regime).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_regulatory_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, third_country_service_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_residents).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, extraterritorial_regulatory_authority_doctrine).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, effects_based_jurisdiction_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European Data Protection Board and national DPAs interpret and enforce Article 3(2), determining which non-EU controllers fall within GDPR scope via the targeting/monitoring test. They issue guidance documents (EDPB Guidelines 3/2018, recital 24 interpretations), investigate complaints from EU residents, and impose fines on non-EU entities found in scope. Their position presumes extraterritorial regulatory authority based on effects within EU territory.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_regulatory_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% The institutional framework of EU data protection law. The effects-based jurisdiction reading vindicates this framework's claim to regulate any processing that targets or monitors EU residents, regardless of controller location. The regime is vindicated: Article 3(2) operationalizes its normative commitments about data subject protection as a fundamental right extending to all processing that affects EU residents.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subject_protection_regime, beneficiary,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subject_protection_regime).

% Non-EU tech platforms, service providers, and data processors whose operations target or monitor EU residents — including targeting via location inference, behavioral profiling, or targeted advertising. They must comply with GDPR requirements (consent, privacy by design, data subject rights, transfer mechanisms, DPA notification) or face fines up to €20M or 4% global turnover. Exit options are constrained: they cannot freely ignore EU residents without losing market access; they cannot rely on home-country regulatory exemptions because Article 3(2) reaches across borders; they must either comply, geofence EU traffic, or litigate jurisdictional scope.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers, payer,
    powerful, generational, constrained, global).

% Smaller non-EU SaaS providers, analytics firms, marketing platforms, and data brokers that process data on EU residents' behalf. They face compliance pressure from their EU customers (who are liable as joint controllers) and direct enforcement exposure. Their identity is often locked into the business model of serving EU clients; exit means fundamental business model change. They lack the resources of large tech platforms to maintain separate systems or negotiate adequacy agreements.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, third_country_service_providers, payer,
    moderate, biographical, identity_locked, global).

% Third-country governments whose tech sectors are subject to EU extraterritorial regulation. They are excluded from the decision-making process that applies GDPR to their corporations but bear the consequence: their companies face compliance costs, market fragmentation pressure (between EU-compliant and non-EU-regulated operations), and potential fines. They experience this as regulatory imperialism. Their policy options are limited: they can impose counter-regulations (data localization requirements, EU-data bans), but these do not resolve the underlying extraterritorial reach.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments, excluded).

% Individuals in the EU whose personal data is processed by non-EU controllers. They gain enforceable rights to access, correction, erasure, and portability; the right to object to processing; and the right to lodge complaints with DPAs. These rights are real and valuable — they represent genuine coordination (protection of data subjects as a collective good). However, exercise of these rights requires awareness, legal standing, and willingness to engage regulatory enforcement; many EU residents lack effective access to these protections despite their formal existence.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_residents, beneficiary,
    powerless, biographical, constrained, continental).

% A competing legal reading that locates GDPR jurisdiction within EU territorial boundaries, treating extraterritorial reach as exceeding legitimate regulatory authority. This reading is excluded from the effects-jurisdiction framework but remains live in international law discourse, litigation strategy, and third-country policy responses.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, territorial_sovereignty_reading, excluded,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(gdpr_article_3_scope__effects_jurisdiction_reading, territorial_sovereignty_reading).

% The European Commission's formal assessment of third-country data protection adequacy, which conditionally exempts certain non-EU jurisdictions from GDPR compliance requirements. This mechanism operationalizes the reading by creating tiers of compliance: EU (fully regulated), adequacy-certified (substantially exempted), and non-certified (fully regulated extraterritorially). The mechanism is both an implementation detail and a legitimacy structure.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, adequacy_determination_mechanism, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(gdpr_article_3_scope__effects_jurisdiction_reading, adequacy_determination_mechanism).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__effects_jurisdiction_reading, eu_regulatory_authorities).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__effects_jurisdiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends data subject protection — a collective good in EU law — to all processing operations affecting EU residents, regardless of controller location. The coordination problem solved: harmonizing privacy protection across borders so that EU residents receive consistent protections without controller jurisdiction creating regulatory arbitrage (geofence-and-process-elsewhere loops).
% TRANSFER_FUNCTION: Moves compliance obligations and enforcement exposure from EU-based controllers (who operate under EU jurisdiction by default) to non-EU controllers (who must now comply to access EU markets). Moves regulatory authority from third countries to the EU DPA network. Moves data subject remedies from national courts to DPA enforcement and individual rights mechanisms.
% ABSENT_VOICES: Third-country governments and non-EU controllers are structurally excluded from the interpretation process that determines their regulatory obligations. They experience this as regulatory imposition. International law scholars emphasizing territorial sovereignty are excluded from the primary GDPR institutions that develop this reading. The competing territorial-sovereignty reading advocates would argue that extraterritorial reach violates customary international law principles; they are excluded from the consensus-building within EU institutions.
% DISAPPEARANCE_RATIONALE: If Article 3(2) effects-jurisdiction disappeared overnight and the constraint reverted to territorial application only, non-EU controllers would immediately restructure: EU processing arms would be isolated into separate legal entities under EU controllers, or EU operations would be geofenced and processed in third countries, or the constraint would be litigated into jurisdictional bankruptcy. EU residents would lose enforcement mechanisms against non-EU controllers. The EU regulatory regime's extraterritorial reach would collapse; the incentive structure for global compliance would vanish.
% FOUNDING_PROBLEM: Early internet-era regulatory fragmentation: privacy regulation was territorially bounded, but internet-mediated data flows crossed borders freely. EU residents were processed by non-EU platforms with no legal protection. The founding problem: how to extend the EU's privacy protection commitments across borders to protect its residents from extraterritorial processing without territorial jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: EU regulatory institutions (EDPB, DPA statements) attest the founding problem is live: non-EU platforms continue to target EU residents with behavioral tracking, targeted advertising, and algorithmic profiling. Independent researchers (Stanford Internet Observatory, academic privacy scholars, civil society organizations like NOYB) confirm the founding problem via empirical evidence of tracking and processing. Third-country governments dispute whether the founding problem justifies extraterritorial regulation, but they do not dispute the factual existence of cross-border processing.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint imposes significant compliance costs on non-EU controllers without reciprocal benefits to those controllers — they must restructure systems, implement privacy-by-design infrastructure, and maintain DPA compliance mechanisms that EU controllers have already internalized. The costs are real and measurable: hiring compliance officers, legal review of processing activities, documentation, Data Transfer Impact Assessment processes, and audit exposure. Suppression is moderate (0.52) because enforcement depends on active DPA policing (complaints, investigations, fines) and controller fear of regulatory exposure — it is not a self-enforcing norm. The suppression requirement has risen over the interval (0.38 to 0.52) as DPA guidance narrowed the targeting/monitoring test boundary, forcing ambiguous cases into the compliant category. Theater has risen modestly (0.15 to 0.28) as the regulatory framework has layered in legitimacy theater: adequacy determinations, transfer mechanism frameworks (Standard Contractual Clauses, Binding Corporate Rules), and EDPB guidance documents that present the effects-jurisdiction test as a principled legal framework rather than regulatory imposition. The measurement series is authoritative on one shared time grid (every metric at every point): early extractiveness is dominated by compliance infrastructure costs and the novelty of DPA enforcement (2016–2018 post-GDPR); mid-interval extractiveness plateaus as controllers internalize compliance structures (2019–2022); theater rises continuously as regulatory guidance expands and legitimacy structures elaborate.
 *
 * PERSPECTIVAL GAP:
 *   From the EU regulatory seat, this is genuine coordination: protecting data subjects as a collective good, extending hard-won privacy protection commitments to all processing that affects EU residents, creating uniform obligations that prevent regulatory arbitrage. From the non-EU controller seat, the same structure operates as jurisdictional overreach: imposing obligations in a jurisdiction where the controller has no physical presence, no political representation, and no reciprocal protections (US controllers subject to GDPR face no reciprocal US regulation of their data practices). From the third-country government seat, it is regulatory imperialism: the EU using the attractiveness of its market to impose its normative commitments on global corporations. From the EU resident seat, the constraint delivers rights but leaves enforcement gaps: formal access to data rights without awareness or procedural access to exercise them. The engine computes these divergences from the stakeholder power atoms, exit options, and declared roles; the divergence is a structural property of the constraint, not an epistemic disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   The EU regulatory authority seat computes as a full beneficiary (d ≈ 0.0): it sets the rules, enforces them, collects fines revenue, and operates in an institutional environment aligned with the reading's premises. Non-EU controllers compute as full targets (d ≈ 1.0): they bear high compliance costs, limited exit options (geofencing loses market access; litigating the jurisdiction is expensive and uncertain), and no control over the rules' development. Third-country governments compute as trapped payers (d ≈ 0.95): they experience regulatory imposition and their exit options are limited to counter-regulation, which does not eliminate the underlying exposure. EU residents compute as beneficiaries with a caveat: they gain formal rights (d ≈ 0.2) but enforcement barriers are substantial — awareness is low, legal standing requires individual agency, and DPA resources are scarce. The beneficiary role is genuine (the rights are real) but the de facto benefit is constrained by friction in the enforcement chain. No directionality override is needed; the structural data drives the divergence from the claimed rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cross-border processing without privacy protection) is live and widely attested. The constraint persists because the coordination function (data subject protection) is genuine and EU residents do benefit from the formal rights, even if enforcement friction is substantial. The classification as tangled rope is correct: a real coordination function (protecting data subjects from processing that tracks or targets them) paired with asymmetric extraction (compliance costs borne by non-EU controllers who gain no benefits). The mandatrophy question (does the founding problem still justify the constraint?) is contested: EU regulatory institutions say yes; third-country governments and affected controllers say the founding problem justifies some coordination but not extraterritorial jurisdiction to this extent. The theater ratio is moderate and rising, suggesting regulatory guidance is increasingly performing legitimacy work as the targeting/monitoring test boundary expands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    targeting_monitoring_boundary_ambiguity,
    'Where does the targeting/monitoring test boundary lie? What processing constitutes targeting or monitoring sufficient to trigger Article 3(2) scope, and what does not?',
    'EDPB guidance interpretation history and DPA enforcement patterns. Litigation (C-311/18 Facebook Ireland, C-252/23 Meta Platforms) that produce binding interpretations of the statutory language. Real-time monitoring of DPA decisions on borderline cases (e.g., analytics that infer location vs. explicit location collection).',
    'If the boundary is narrow (only explicit behavioral targeting), extractiveness and scope compress; if the boundary is broad (any processing that could identify or profile EU residents), extractiveness rises and more non-EU controllers fall into scope. Theater ratio rises with boundary expansion because increasingly debatable cases must be explicitly justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(targeting_monitoring_boundary_ambiguity, empirical, 'Whether targeting/monitoring is construed narrowly or expansively determines effective scope and compliance burden.').

omega_variable(
    extraterritorial_authority_legitimacy,
    'Does the effects-based jurisdiction reading rest on a legitimate authority grounding, or is it regulatory overreach that violates customary international law norms of territorial sovereignty?',
    'International law scholarship and state practice. Third-country legal challenges to GDPR''s extraterritorial application (diplomatic protests, counter-regulations like Russia''s data localization, India''s Personal Data Protection Act development). Consensus-building in international bodies (UN, UNCITRAL, WTO).',
    'If extraterritorial reach is deemed legitimate, the effects-jurisdiction reading holds; if deemed overreach, the territorial-sovereignty reading gains legal standing and the constraint''s classification drifts toward snare (extraction not offset by genuine coordination). This omega defines the core disagreement between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraterritorial_authority_legitimacy, conceptual, 'Whether effects-based jurisdiction is a legitimate regulatory principle or regulatory imperialism.').

omega_variable(
    adequacy_determination_credibility,
    'Are third-country adequacy determinations credible assessments of equivalent protection, or are they theater that preserves jurisdiction reach while appearing to defer to third-country law?',
    'Post-adequacy data flows and enforcement patterns. If data flows from adequacy-deemed countries violate rights at comparable rates to non-adequacy countries, the determination was theater. If rights protection actually improves post-adequacy, the determination was substantive.',
    'If adequacy determinations are credible, they operationalize a principled exception that softens the constraint''s extraction. If they are theater, the constraint remains effectively applied to the entire non-EU world, and suppression rises (the mechanism exists to create the appearance of reciprocity without reciprocal protection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_determination_credibility, empirical, 'Whether adequacy determinations represent genuine reciprocity or performative legitimacy.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the effects-jurisdiction reading foreclose the territorial-sovereignty reading within a single legal framework, or can both coexist as live interpretations held by different parties?',
    'Jurisprudential analysis of whether Article 3(2)''s statutory language supports one or both readings as coherent legal positions. If the text underdetermines the choice (effects vs. territory), both remain live. If the text strongly implies effects, the territorial reading is marginalized.',
    'If foreclosure holds, the territorial-sovereignty constraint story does not represent a live alternative within EU law — it represents a rejected interpretation; EU law has settled on effects-jurisdiction, and the contest is between readings that accept effects-jurisdiction and disagree on its limits. If coexistence holds, both readings remain live within EU jurisprudence and the constraint story represents one live reading among others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether this reading''s core premise logically forecloses the territorial-sovereignty reading or both remain live.').

omega_variable(
    eu_resident_enforcement_gap,
    'Are EU residents genuine beneficiaries of Article 3(2) enforcement, or does the enforcement gap (awareness, legal standing, DPA resource scarcity) make the formal rights a theater benefit?',
    'Empirical data on EU resident awareness of data protection rights; DPA caseload and completion rates for individual complaints; access-to-justice outcomes; comparative analysis with jurisdictions that provide direct private rights of action for data violations.',
    'If the enforcement gap is substantial, the beneficiary role for EU residents is conditional and partial — extractiveness to non-EU controllers is not matched by de facto benefit to residents, making the tangled rope claim weaker. If enforcement access is good, the beneficiary role is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_resident_enforcement_gap, empirical, 'Whether the formal rights granted to EU residents translate into de facto enforcement access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(gdpr_tr_t0, observed).
narrative_ontology:measurement(gdpr_tr_t3, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 3, 0.18).
narrative_ontology:measurement_basis(gdpr_tr_t3, observed).
narrative_ontology:measurement(gdpr_tr_t6, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(gdpr_tr_t6, observed).
narrative_ontology:measurement(gdpr_tr_t10, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(gdpr_tr_t10, observed).
narrative_ontology:measurement(gdpr_tr_t15, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(gdpr_tr_t15, observed).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(gdpr_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(gdpr_be_t0, observed).
narrative_ontology:measurement(gdpr_be_t3, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(gdpr_be_t3, observed).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(gdpr_be_t6, observed).
narrative_ontology:measurement(gdpr_be_t10, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(gdpr_be_t10, observed).
narrative_ontology:measurement(gdpr_be_t15, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(gdpr_be_t15, observed).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(gdpr_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(gdpr_su_t0, observed).
narrative_ontology:measurement(gdpr_su_t3, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement_basis(gdpr_su_t3, observed).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(gdpr_su_t6, observed).
narrative_ontology:measurement(gdpr_su_t10, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(gdpr_su_t10, observed).
narrative_ontology:measurement(gdpr_su_t15, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(gdpr_su_t15, observed).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(gdpr_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__effects_jurisdiction_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, standard_contractual_clauses_transfer_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, eu_adequacy_determination_process).

% DUAL FORMULATION NOTE:
% This constraint is part of the GDPR_ARTICLE_3_SCOPE constraint family. Article 3(2) is a contested kernel that three different readings interpret as structurally distinct constraints with different jurisdictional reach, beneficiary/victim structures, and classifications. The effects-jurisdiction reading (this file) asserts jurisdiction based on targeting/monitoring EU residents. The territorial-sovereignty reading locates jurisdiction within EU borders only. The market-access reading frames GDPR as a conditional access requirement rather than a jurisdictional assertion. Each reading has a different ε (targeting/monitoring effects: high extraction; territorial: zero extraterritorial extraction; market access: moderate extraction conditional on choosing to serve EU market). Each reading has different victims (effects: all non-EU controllers; territorial: EU controllers without non-EU presence; market-access: only those choosing EU market). All three readings are linked via network.affects_constraints; this file links to the siblings. The decomposition follows ε-invariance (OQ-258): measuring the constraint via effects on EU residents vs. measuring it via territorial boundaries vs. measuring it via market conditionality yields different ε values; these are not measurement perspectives on one constraint—they are three distinct constraints grounded in three readings of the same written text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
