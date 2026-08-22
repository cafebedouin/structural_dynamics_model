% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__market_access_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3 Scope as Conditional Market Access (Brussels Effect Reading)
 *   domain: technology/regulatory/international_law
 *
 * SUMMARY:
 *   The EU's GDPR, particularly Article 3's scope rules, can be read as
 *   either a jurisdictional assertion over non-EU actors (effects
 *   jurisdiction reading) or as a conditional market-access requirement that
 *   achieves global regulatory influence through standard diffusion rather
 *   than legal authority (market-access reading, this constraint). Under the
 *   market-access reading, GDPR compliance is not imposed by Brussels
 *   asserting extraterritorial jurisdiction—a claim that would trigger
 *   sovereignty objections—but rather emerges when companies rationally
 *   choose to standardize globally on GDPR-level privacy to access the EU
 *   market. This reading frames the constraint as coordination with lower
 *   enforcement tension: compliance is voluntary (companies can exit by not
 *   serving EU users), beneficiaries are EU residents and the EU regulatory
 *   institution (whose standard-setting influence expands globally), and the
 *   constraint is structurally more rope-like (genuine coordination,
 *   economies of scale from harmonization) than extractive (no single actor
 *   coerces compliance outside the EU). The claim/metric divergence is
 *   structural: this reading CLAIMS rope (coordination benefit via unified
 *   standard) while metrics show moderate suppression (companies with global
 *   operations face effective mandatory compliance even where EU presence is
 *   marginal) and moderate extractiveness (compliance costs are transferred
 *   to processors without corresponding per-user negotiation). The engine
 *   measures this divergence.
 *
 * KEY AGENTS:
 *   - EU residents: beneficiaries of unified privacy floor across global platforms
 *   - EU regulatory authority (DPAs, European Commission): agenda-setters; benefit from standard diffusion
 *   - Global data processors (Google, Meta, Microsoft, etc.): payers; face compliance costs as condition of EU market access
 *   - US tech companies: dual role—payers (compliance costs) and secondary beneficiaries (single global standard simplifies operations vs. jurisdiction-by-jurisdiction fragmentation)
 *   - Non-EU jurisdictions (US, China, others): observers; debate whether GDPR is legitimate market coordination or regulatory overreach
 *   - Data subjects outside EU: excluded; receive no protection from GDPR despite processing by GDPR-subject companies
 *   - Competing regulatory models: observers; represent alternative standard-setting frameworks (sectoral, human-rights, property-based)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.52).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.38).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Scope as Conditional Market Access (Brussels Effect Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology/regulatory/international_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, '1e65e805-b714-4df4-9e6c-8fd0927612a7').
narrative_ontology:cs_kernel_codification('1e65e805-b714-4df4-9e6c-8fd0927612a7', fixed_text).
narrative_ontology:cs_authority_grounding('1e65e805-b714-4df4-9e6c-8fd0927612a7', extraction).
narrative_ontology:cs_interpretation_layer_present('1e65e805-b714-4df4-9e6c-8fd0927612a7').
narrative_ontology:cs_reading_relation('1e65e805-b714-4df4-9e6c-8fd0927612a7', gdpr_article_3_scope__effects_jurisdiction_reading, influences).
narrative_ontology:cs_reading_relation('1e65e805-b714-4df4-9e6c-8fd0927612a7', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('1e65e805-b714-4df4-9e6c-8fd0927612a7', foundational, market_access_mechanism_legitimacy).
narrative_ontology:cs_axiom_status(market_access_mechanism_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1e65e805-b714-4df4-9e6c-8fd0927612a7', market_access_mechanism_legitimacy, conventional).
narrative_ontology:cs_axiom('1e65e805-b714-4df4-9e6c-8fd0927612a7', foundational, standard_diffusion_not_jurisdictional_assertion).
narrative_ontology:cs_axiom_status(standard_diffusion_not_jurisdictional_assertion, holdable).
narrative_ontology:cs_axiom_grounding('1e65e805-b714-4df4-9e6c-8fd0927612a7', standard_diffusion_not_jurisdictional_assertion, conventional).
narrative_ontology:cs_reference_frame('1e65e805-b714-4df4-9e6c-8fd0927612a7', eu_market_coordination_via_standard_setting).
narrative_ontology:cs_drift_state('1e65e805-b714-4df4-9e6c-8fd0927612a7', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e65e805-b714-4df4-9e6c-8fd0927612a7', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_residents).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_regulatory_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, us_tech_companies).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, global_data_processors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, us_tech_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive standardized privacy protections on personal data processing by any organization that offers goods or services to them or monitors their online behavior. Under the market-access reading, EU residents benefit from coordination on a unified privacy floor that applies globally—any company offering them services must meet GDPR standards, not the jurisdiction where the company is incorporated. Exit is available (they can relocate outside the EU or stop using digital services) but costly relative to the benefit.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_residents, beneficiary,
    organized, biographical, mobile, continental).

% Designs, enforces, and revises GDPR through a network of data protection authorities (DPAs) in member states, the European Commission, and the European Data Protection Board. Under the market-access reading, the EU's authority is not jurisdictional assertion but legitimate coordination of a regulatory standard. Benefits accrue through regulatory influence: GDPR becomes a de facto global standard companies standardize to, expanding EU regulatory soft power without legal claims to authority over non-EU actors. Enforcement is via market access (exclusion from EU operations) and fines, not extraterritorial police power.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_regulatory_authority, agenda_setter,
    institutional, generational, analytical, continental).

% Must comply with GDPR standards when processing data of EU residents, regardless of the processor's own jurisdiction. Compliance requires substantial engineering (consent mechanisms, data request infrastructure, audit trails), legal expertise, and ongoing operational overhead. Exit from compliance is available (stop serving EU users) but constrained by EU market size and user base—most global processors cannot afford to exit. Under the market-access reading, compliance is not coerced but is a rational market choice; however, the constraint still transfers compliance costs from residents to processors.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, global_data_processors, payer,
    powerful, biographical, constrained, global).

% Incorporated and primarily regulated under US law but face GDPR compliance obligations for EU user data. Bear direct compliance costs (engineering, legal, audit) and indirect costs (disclosure obligations, data-subject rights that reduce data-monetization potential). Secondary benefit: GDPR's global adoption as a de facto standard simplifies global operations relative to fragmenting per-jurisdiction (avoiding the need for different privacy architectures in California, Brazil, China, etc.). Debate internally whether to frame GDPR as market advantage (secondary benefit) or regulatory burden (primary cost).
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, us_tech_companies, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, us_tech_companies, beneficiary).

% Are functionally excluded from the EU market because their data governance architecture (integration with state surveillance, cross-border flows to mainland China without user consent, absence of individual data-subject rights) is fundamentally incompatible with GDPR standards. Could theoretically comply but would require abandoning core operational partnerships with Chinese government and redesigning global data flows. Exit is not available: Chinese platforms cannot exit the EU market because they were never admitted. The exclusion operates as a market-access mechanism, not a jurisdictional ban.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, chinese_platforms, excluded,
    powerful, biographical, trapped, global).

% Include the United States, China, India, Brazil, and other nations that each have their own privacy regulatory frameworks. Observe GDPR as a standard-setting exercise that creates global de facto pressure because large companies choose to standardize globally on GDPR-level privacy rather than maintaining parallel systems. Some jurisdictions (Brazil, South Africa) adopt GDPR-like frameworks; others (US) resist it as regulatory overreach; others (China) see it as incompatible with state surveillance models. Under the market-access reading, they face downstream pressure to harmonize not through legal authority but through companies' market preferences.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_jurisdictions, observer,
    institutional, generational, analytical, global).

% Remain unprotected by GDPR even when their data is processed by EU companies or flows through EU infrastructure. Their data can be processed, sold, and monetized without the consent and rights protections GDPR affords EU residents. The constraint's beneficiary status (EU residents only) is a structural feature: GDPR's scope is territorial in its beneficiary definition (location of data subject) even while its obligation scope is global (applies to all processors of EU resident data).
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, data_subjects_outside_eu, excluded,
    powerless, immediate, trapped, global).

% Represent alternative privacy frameworks: UN human-rights tradition (privacy as fundamental right); US sectoral regulation (HIPAA, CCPA, FTC); data-as-property models (Silicon Valley, some tech-forward jurisdictions); and authoritarian data-gathering models (China, Russia). Observe GDPR as one among competing legitimate standard-setting approaches. Under the market-access reading, they compete in the global marketplace of regulatory influence—GDPR succeeds not because Brussels has authority but because it aligns with EU resident preferences and company market incentives. Their competing models can coexist; the constraint is not jurisdictionally exclusive.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, competing_regulatory_models, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__market_access_reading, eu_regulatory_authority).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified, high-bar privacy standard for data processing that applies to any organization offering goods/services to or monitoring EU residents. Solves the coordination problem of privacy fragmentation: absent the GDPR, companies would face inconsistent privacy requirements across EU member states (and globally would face pressure to fragment systems per jurisdiction). The standard enables EU residents to expect consistent protections and gives data processors a single compliance target rather than a patchwork.
% TRANSFER_FUNCTION: Transfers compliance costs (system engineering, audit maintenance, data-subject-request handling) from EU residents (who would otherwise negotiate privacy individually or remain unexposed) to global data processors (who absorb standardized requirements as a condition of market access). Also transfers regulatory authority: decisions about privacy standards shift from individual national governments to the EU institution, creating economies of scale in standard-setting.
% ABSENT_VOICES: Non-EU residents whose data is processed by companies subject to GDPR but who receive no equivalent protections; competing regulatory traditions (sectoral US model, data-as-property framings) that would argue for alternative standard-setting mechanisms; companies whose business models depend on data monetization without user consent (some analytics, ad-tech, data-broker models) and would argue for lower compliance bars or sector carve-outs.
% DISAPPEARANCE_RATIONALE: If GDPR enforcement vanished, EU companies would likely fragment into compliant and non-compliant tiers for EU vs. non-EU users, non-EU companies would optimize globally around lowest-common-denominator privacy standards (likely lower), and EU residents would lose the coordination benefit of a unified floor. The market-access mechanism that makes GDPR sticky would dissolve: companies would no longer have an incentive to standardize globally on GDPR-level privacy once EU-specific compliance was optional.
% FOUNDING_PROBLEM: Massive post-internet fragmentation: after 1995, the EU faced a choice between preserving national data-protection directives (creating internal barriers to data flow) or harmonizing on a single standard. The founding problem was: how to enable the digital single market without surrendering member-state autonomy over data protection policy?
% FOUNDING_PROBLEM_CORROBORATION: EU legislative records (GDPR recitals, especially recital 3 on 'distortions of competition'), statements from DPAs and the European Commission positioning GDPR as essential to the digital single market, and independent analysis (e.g., WP29 opinions, academic literature on regulatory coordination) confirm the founding problem remains: the EU must coordinate privacy standards to prevent member-state fragmentation while harmonizing the internal market. Non-EU actors (US tech companies, other jurisdictions) attest that the founding problem motivates GDPR's scope but dispute whether the solution is legitimate market coordination or overreach.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__market_access_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because compliance costs are real and decoupled from service marginal cost, but the mechanism is market-driven (companies choose to comply to access EU users) rather than coercive (no direct punishment for non-EU refusal). Suppression is low (0.38) because exit is structurally available (non-EU companies can legally refuse EU market; this is not economic trapping, only exit from EU revenue). Theater is low (0.22) because the coordination function (unified standard enabling the digital single market) is genuine and accounts for most enforcement activity; a small fraction of DPA activity is theatrical (publicly visible enforcement actions that deter compliance violations without directly implementing protection). Accessibility collapse is moderate (0.48): once a company understands GDPR applies to their EU users, alternatives (sectoral carve-outs, non-compliance with penalties) are substantially closed unless they exit the EU market entirely. Resistance is high (0.61): the constraint meets real pushback from US tech companies, non-EU governments claiming regulatory overreach, data-monetization-dependent businesses, and privacy-skeptical academics. The measurement series show extractiveness and suppression rising slightly over 2018–2026 as GDPR enforcement matured and as companies increasingly standardized globally on GDPR standards (what started as EU-specific requirement became default global practice), but the trajectory is shallow—the constraint stabilized at a rope-like profile rather than sliding toward snare dynamics.
 *
 * PERSPECTIVAL GAP:
 *   From the EU regulatory seat: GDPR is legitimate market coordination solving a genuine collective-action problem (EU member-state fragmentation; global lack of privacy standards). Compliance is voluntary—companies choose it. Enforcement is soft (fines relative to global revenue, applied by independent DPAs) and justified by harm to residents. From the US tech company seat: GDPR is a unilateral standard imposed by a powerful economic bloc, framed as privacy protection but functioning as regulatory capture (locking in the EU's policy preferences globally). Compliance is mandatory for market access even where the company is incorporated outside the EU. Enforcement is unpredictable (DPA interpretation varies; fines sometimes reach billions). From the non-EU government seat: GDPR is economic leverage disguised as privacy advocacy, externalizing EU regulatory preferences onto other jurisdictions without their consent. The market-access reading brackets this dispute by reframing it: GDPR works because companies choose it, not because Brussels has authority. The engine computes per-seat classification divergence from the structural data (power, exit options, beneficiary/victim status); the reading's argumentative move is to shift the frame from authority to market mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   EU residents (beneficiary, moderate power, mobile exit): d near 0.25–0.30 (they benefit from the coordination without bearing compliance costs directly; they can theoretically exit by moving to a jurisdiction with lower privacy protections, but exit is costly relative to the benefit). EU regulatory authority (agenda-setter, institutional power, analytical exit): d near 0.0 (it sets the standard and faces no costs; it collects authority and legitimacy). Global data processors (payer, powerful power, constrained exit—can exit EU but lose market): d near 0.65–0.75 (they bear compliance costs; exit from compliance means exiting EU revenue, which is constrained for most large tech). US tech companies (dual payer/beneficiary, powerful power, constrained exit): d near 0.55–0.65 (they pay compliance costs but benefit from avoiding per-jurisdiction fragmentation globally; the secondary benefit lowers their directionality relative to pure-payer processors). Non-EU jurisdictions (observer, institutional power, analytical exit): d = 0.5 (no direct cost or benefit; they face downstream pressure to harmonize but analytically can maintain autonomy). The market-access reading supports this structure: beneficiaries are real (EU residents do coordinate on a unified floor; processors do avoid per-jurisdiction fragmentation), which keeps directionality moderate rather than high. If the constraint were read as pure jurisdictional assertion (effects-jurisdiction reading), directionality would shift: non-EU actors would move from d=0.5 (observer) to d near 0.75 (target of assertion).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (EU market fragmentation preventing digital single market) remains live and is solved by GDPR: member states coordinate on a unified privacy standard, eliminating internal barriers. Mandatrophy would emerge if the founding problem died but the constraint persisted. Current status: the founding problem is live and GDPR addresses it, so no mandatrophy yet. Future risk: if EU member states and tech companies voluntarily harmonize on privacy standards below GDPR-level (e.g., via business pressure or regulatory capture), and if GDPR becomes a historical artifact enforced only for theatrical compliance, mandatrophy would trigger. The market-access reading makes mandatrophy less likely than the effects-jurisdiction reading because market mechanisms are self-correcting (if GDPR becomes unnecessary, companies will stop standardizing globally on it; if it becomes unjustified, EU residents can demand lower privacy bars without triggering jurisdictional conflict). The effects-jurisdiction reading makes mandatrophy more likely because legal jurisdiction can persist after function dies (law books stay written even after they stop being obeyed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_access_vs_jurisdictional_assertion,
    'Does GDPR Article 3 scope represent legitimate market-access conditioning (companies voluntarily standardize to access EU users) or overreach of jurisdictional authority (Brussels asserts legal control over non-EU actors)?',
    'Analyze company-internal compliance decisions and communications: do companies frame GDPR as voluntary market strategy (market-access reading) or as imposed legal requirement (jurisdiction reading)? Cross-check with regulatory framing by DPAs and European Commission: do they present GDPR as authority claim or as market coordination? Survey non-EU jurisdictions'' adoption of GDPR-like standards: if adoption is market-driven (companies want global standardization), the market-access reading strengthens; if adoption is regulatory coercion (non-EU governments feel compelled to harmonize), the jurisdiction reading strengthens.',
    'If market-access reading is correct, GDPR is a rope-like coordination mechanism with soft enforcement and voluntary compliance—misclassifying it as jurisdiction (snare) would overstate the extraction. If jurisdiction reading is correct, GDPR overreaches and misclassifying it as market-access would understate the suppression and extractiveness. The reading determination is structural, not empirical—it depends on the normative framing, not on whether GDPR ''actually'' has market-access properties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_access_vs_jurisdictional_assertion, conceptual, 'Whether GDPR scope is legitimately framed as market conditioning or regulatory overreach.').

omega_variable(
    voluntary_compliance_standardization,
    'To what extent do global companies standardize on GDPR-level privacy because of market incentives (maximize EU access) versus regulatory coercion (fear of DPA fines)?',
    'Comparative analysis of companies with different EU dependency profiles: do low-EU-revenue companies still comply with GDPR? Do companies in high-privacy-demand jurisdictions (California, Brazil) standardize globally on GDPR before local regulation compels it, suggesting market preference? Interview tech executives on compliance rationale. Analyze DPA enforcement data: what fraction of compliance activity is preventive (companies self-auditing to avoid violation) versus reactive (DPAs enforcing violations after detection)?',
    'High voluntary compliance and market-driven standardization supports the market-access reading (rope). High coercion-driven compliance and strategic non-compliance where fines are manageable relative to revenue supports the jurisdiction reading (snare). The constraint''s extractiveness and suppression profiles are sensitive to this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_compliance_standardization, empirical, 'The degree to which GDPR compliance is market-driven versus coercion-driven.').

omega_variable(
    kernel_reading_sibling_competition,
    'Which reading of GDPR Article 3 scope is endorsed by the dominant institutional actor (EU regulatory authority) and the dominant private sector (tech companies)?',
    'Document EU regulatory framing in legislative records, DPA guidance, and enforcement communications. Document tech-company framing in compliance policies, regulatory testimony, and internal documents. Track movement over time: has the dominant reading shifted from effects-jurisdiction (early GDPR era) toward market-access (as companies standardized globally) or toward territorial-sovereignty (as non-EU jurisdictions pushed back)?',
    'The reading that gains institutional endorsement will shape global privacy regulation''s trajectory. A market-access reading becoming dominant would entrench GDPR as a de facto global standard through competitive dynamics rather than legal authority (rope → piton risk as standardization becomes inertial). A jurisdiction reading becoming dominant would trigger sovereignty-based resistance and fragmentation (snare dynamics intensify). A territorial-sovereignty reading becoming dominant would collapse GDPR''s extraterritorial reach (mountain—legal boundaries as natural law).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_sibling_competition, conceptual, 'Which reading of GDPR''s Article 3 scope is institutionally endorsed and will shape global regulation.').

omega_variable(
    suppression_internalized_vs_structural,
    'To what extent is non-EU company compliance to GDPR enforced by structural barriers (DPA enforcement, market exclusion, reputational risk) versus internalized norms (acceptance of privacy-as-human-right framing, adoption of privacy-by-design philosophy)?',
    'Post-exit suppression trajectory: if a company is exempted from GDPR (e.g., geopolitical exception, data-adequacy finding later withdrawn), does it immediately revert to pre-GDPR privacy practices or maintain GDPR-level standards? Track adoption of GDPR-like standards in jurisdictions with no enforcement power over tech companies: if standards spread without enforcement, internalization is high; if they plateau at low adoption, suppression is structural.',
    'If suppression is internalized, companies carry GDPR compliance norms globally even absent enforcement, supporting a rope reading (coordination internalized). If suppression is structural, non-compliance would spike absent enforcement pressure, supporting a snare reading (coercion-dependent). This is the suppression-mechanism ambiguity per OQ-83 guidance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Whether GDPR compliance is structurally enforced or internalized as a norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__market_access_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement_basis(gdpr_tr_t2018, observed).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__market_access_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement_basis(gdpr_tr_t2020, observed).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__market_access_reading, theater_ratio, 2022, 0.21).
narrative_ontology:measurement_basis(gdpr_tr_t2022, observed).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__market_access_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(gdpr_tr_t2024, observed).
narrative_ontology:measurement(gdpr_tr_t2026, gdpr_article_3_scope__market_access_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(gdpr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2018, 0.44).
narrative_ontology:measurement_basis(gdpr_be_t2018, observed).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement_basis(gdpr_be_t2020, observed).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2022, 0.51).
narrative_ontology:measurement_basis(gdpr_be_t2022, observed).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement_basis(gdpr_be_t2024, observed).
narrative_ontology:measurement(gdpr_be_t2026, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(gdpr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement_basis(gdpr_su_t2018, observed).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2020, 0.36).
narrative_ontology:measurement_basis(gdpr_su_t2020, observed).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2022, 0.37).
narrative_ontology:measurement_basis(gdpr_su_t2022, observed).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2024, 0.38).
narrative_ontology:measurement_basis(gdpr_su_t2024, observed).
narrative_ontology:measurement(gdpr_su_t2026, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2026, 0.38).
narrative_ontology:measurement_basis(gdpr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__market_access_reading, 0.1).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the GDPR Article 3 scope kernel. Three sibling constraints instantiate different readings of the same formal rule. The effects-jurisdiction reading treats GDPR as legitimate extraterritorial jurisdiction grounded in effects on EU residents; the territorial-sovereignty reading treats GDPR as overreach that violates state sovereignty. This market-access reading treats GDPR as conditional market access achieved through standard diffusion, not legal authority. The three readings do NOT observe GDPR from different angles—they are competing normative frameworks that instantiate different constraint structures from the same legal text. Epsilon values differ across readings: the market-access reading has moderate extractiveness (0.52) because compliance is voluntary; the jurisdiction reading would have higher extractiveness (0.65+) because compliance is legally imposed; the sovereignty reading would have near-zero extractiveness (0.15) because it denies GDPR's legitimacy entirely. The family link is via the kernel: all three share constraint_id root 'gdpr_article_3_scope' but are differentiated by reading_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__market_access_reading, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
