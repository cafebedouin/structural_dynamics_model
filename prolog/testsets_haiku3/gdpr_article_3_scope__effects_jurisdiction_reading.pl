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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Effects Jurisdiction - EU Data Subject Protection Reading
 *   domain: technology governance / international law / privacy regulation
 *
 * SUMMARY:
 *   Article 3(2) of the GDPR establishes jurisdiction over controllers
 *   processing personal data of EU residents when the processing is 'directed
 *   at' or 'monitors' their behavior. The effects jurisdiction reading
 *   interprets this clause to assert extraterritorial reach: any non-EU
 *   controller whose services target EU residents or collect their behavioral
 *   data falls under GDPR compliance obligations, regardless of where
 *   processing occurs or the controller's physical location. This reading
 *   instantiates the EU's assertion that data protection is a fundamental
 *   right that follows the resident globally, not a territorial regulatory
 *   boundary. The constraint is claimed as tangled_rope because it genuinely
 *   solves a coordination problem (unified protection for globally mobile
 *   data subjects) while simultaneously extracting compliance costs and
 *   regulatory burdens from non-EU actors. The measured suppression (0.52)
 *   reflects the constraint's operation through enforcement and legal risk,
 *   not physical coercion: non-EU platforms suppress alternative legal
 *   interpretations and business models by facing fines, audit, and market
 *   consequences if they contest the effects jurisdiction reading.
 *
 * KEY AGENTS:
 *   - EU data subjects: Protected party, beneficiary of extraterritorial protection, but constrained exit (cannot opt out of digital services)
 *   - European Commission and EDPB: Agenda-setter, interpreter of Article 3(2), enforcement coordinator, bearer of institutional interest in regulatory authority
 *   - Non-EU technology platforms: Payer, high-power actors (Meta, Google, Amazon, etc.) subject to GDPR compliance costs and fine exposure
 *   - International platforms and smaller service providers: Payer, mobile exit options via geo-blocking or service withdrawal
 *   - Third-country governments: Payer indirectly, trapped by geopolitical and trade consequences, observer in enforcement but not design
 *   - Data subjects outside EU: Excluded, structurally absent from the framework despite bearing indirect compliance costs
 *   - European digital industry: Beneficiary, gains competitive advantage through compliance barriers and services market
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
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Effects Jurisdiction - EU Data Subject Protection Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology governance / international law / privacy regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '54ccfede-6a1b-4e67-b4e7-1c29e6f1966c').
narrative_ontology:cs_kernel_codification('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', fixed_text).
narrative_ontology:cs_authority_grounding('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', lineage).
narrative_ontology:cs_interpretation_layer_present('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c').
narrative_ontology:cs_reading_relation('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', foundational, data_protection_as_fundamental_right_follows_resident).
narrative_ontology:cs_axiom_status(data_protection_as_fundamental_right_follows_resident, holdable).
narrative_ontology:cs_axiom_grounding('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', data_protection_as_fundamental_right_follows_resident, deontological).
narrative_ontology:cs_axiom('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', foundational, regulatory_authority_extends_where_effects_occur).
narrative_ontology:cs_axiom_status(regulatory_authority_extends_where_effects_occur, holdable).
narrative_ontology:cs_axiom_grounding('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', regulatory_authority_extends_where_effects_occur, instrumental).
narrative_ontology:cs_reference_frame('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', extraterritorial_rights_protection_via_effects_targeting).
narrative_ontology:cs_drift_state('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', contemporary_trade_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('54ccfede-6a1b-4e67-b4e7-1c29e6f1966c', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, european_commission).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_privacy_enforcement_regime).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_digital_service_providers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, international_technology_platforms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, european_digital_industry).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, national_data_protection_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, third_country_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% EU residents whose personal data is processed anywhere in the world. Under the effects jurisdiction reading, they gain substantive legal protection and recourse against non-EU controllers targeting or monitoring them, regardless of where processing occurs. Their practical exit is limited by the fact that digital services globally collect EU-resident data by default; protection follows them rather than requiring them to exit services.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, global).

% Interprets Article 3(2) to assert jurisdiction over any controller whose processing is directed at EU residents or monitors their behavior, even if the controller is outside the EU and processing occurs outside the EU. Sets enforcement policy through guidance documents (WP29, EDPB), coordinates national data protection authorities, and negotiates adequacy determinations with third countries. Bears no direct cost from compliance; enforcement capacity and regulatory authority are its primary interests.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, european_commission, agenda_setter,
    institutional, generational, analytical, global).

% Implement and enforce Article 3(2) within their member states, issue decisions against non-EU controllers processing EU-resident data, impose fines, and investigate complaints. They gain authority and investigative reach but also bear administrative costs and legal challenge burden from non-EU defendants claiming lack of jurisdiction. Their exit options are constrained by the EU legal hierarchy and supremacy of GDPR interpretation.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, national_data_protection_authorities, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, national_data_protection_authorities, payer).

% Large US, Chinese, and other non-EU technology platforms (social media, search, cloud services, analytics) that process data of EU residents. Under the effects jurisdiction reading, they are subject to GDPR compliance and enforcement even without a physical presence in the EU, if they target EU users or monitor their behavior. Compliance costs include data protection impact assessments, privacy by design, breach notification, data subject rights fulfillment, and exposure to substantial fines (up to 4% of global revenue). They cannot exit by simply withdrawing services; the effects jurisdiction reaches them wherever they operate.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_digital_service_providers, payer,
    powerful, biographical, constrained, global).

% Smaller, non-dominant service providers (emerging platforms, data brokers, fintech, analytics startups) with international operations and EU-resident user bases. They face the same effects jurisdiction exposure as dominant platforms but with lower compliance budgets and less negotiating power with EU authorities. Their exit options are higher than massive platforms: they can withdraw from EU service markets or geo-block EU traffic, at the cost of lost revenue and user base.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, international_technology_platforms, payer,
    powerful, biographical, mobile, global).

% Non-EU governments (US, China, India, Russia, etc.) whose resident and national technology companies face extraterritorial GDPR enforcement and compliance obligations. They have limited direct enforcement power against the EU but carry the downstream cost of domestic companies bearing compliance burdens and facing fines. They are trapped in the sense that their exit option—blocking EU access entirely—carries geopolitical and economic consequences. Some governments negotiate adequacy status or alternative frameworks (SCCs, BCRs) but remain subject to EU jurisdiction interpretation.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, third_country_governments, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, third_country_governments, observer).

% Residents of third countries whose data is processed by the same platforms and services, but who are excluded from the effects jurisdiction framework because Article 3(2) is read as protecting only EU residents. They would have stronger privacy protections if the effects jurisdiction reading extended to them, but they have no voice in the GDPR legislative or enforcement structure and remain subject to whatever privacy standards (or lack thereof) their own jurisdictions establish.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, data_subjects_outside_eu, excluded,
    powerless, biographical, trapped, global).

% EU-based technology companies, data protection consultants, compliance service providers, and legal firms that benefit from GDPR's extraterritorial reach. They gain competitive advantage through compliance expertise and regulatory cost barriers to foreign competitors; EU compliance services are a growing industry. They can exit by relocating but derive strategic benefit from the effects jurisdiction reading's market-shaping effects.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, european_digital_industry, beneficiary,
    organized, generational, mobile, global).

% WTO, bilateral trade negotiators, and international commerce institutions that observe and adjudicate disputes over the GDPR's consistency with free trade obligations and market access rights. They take testimony from affected parties but have limited enforcement mechanisms against the EU's sovereign regulatory choices. They are analytical observers with institutional standing but constrained practical authority.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, international_trade_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__effects_jurisdiction_reading, european_commission).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__effects_jurisdiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified data protection standard protecting EU residents globally: controllers processing EU-resident data face consistent legal obligations regardless of where they operate, eliminating the race-to-the-bottom dynamic where protection varies by jurisdiction. Solves the coordination problem that data flows globally but privacy rights are territorially bounded—the effects jurisdiction reading brings rights protection to follow the resident rather than the territory.
% TRANSFER_FUNCTION: Moves compliance costs from non-EU technology platforms to EU-resident data subjects (as a tax on data processing) and from the EU as a market jurisdiction to the entire world (extraterritorial scope). In exchange, EU residents receive data protection that follows them. Non-EU controllers transfer capital and operational expenses into compliance infrastructure; enforcement authorities gain power and investigative reach; EU compliance industries gain market opportunity.
% ABSENT_VOICES: Non-EU data subjects are structurally absent from the framework—they would argue that a truly universal data protection standard should apply equally to all residents, not just EU residents. US government and major technology companies argue for territorial sovereignty limits but are excluded from GDPR design and amendment processes; their objections enter only through trade and WTO channels. Non-dominant international platforms have limited representation in the EDPB and EC's interpretation process. Developing-country data processors and their governments are not in the conversation despite bearing compliance costs.
% DISAPPEARANCE_RATIONALE: If the effects jurisdiction reading of Article 3(2) disappeared—if jurisdiction reverted to territorial limits—global data processing would reorganize: EU residents would lose extraterritorial protection; non-EU platforms would face no GDPR compliance obligation for EU-resident data; adequate data protection would become conditional on platform choice rather than legal requirement; EU enforcement capacity would shrink; third-country negotiating positions on data flows and adequacy would strengthen. The entire architecture of EU-resident data protection would revert to territorial boundaries.
% FOUNDING_PROBLEM: Early digital markets created a governance gap: EU residents were protected by strong local privacy laws, but non-EU platforms and controllers processing their data globally faced no uniform legal obligation to honor those protections. Privacy rights stopped at the border while data flowed everywhere. The EU's reading of Article 3(2) was constructed to extend the jurisdiction of EU law and its data subject protections to match the actual flow of resident data, asserting that privacy rights follow the resident, not the territory.
% FOUNDING_PROBLEM_CORROBORATION: The EU Commission and EDPB affirm the founding problem: territorial privacy law is inadequate for a globalized data economy and EU residents are structurally vulnerable to unaccountable non-EU processors. They attest the effects jurisdiction reading is the necessary remedy. Non-EU governments, technology platforms, and international trade bodies contest the founding problem framing: they argue the real problem is regulatory overreach and extraterritorial assertion of authority beyond legitimate sovereign scope. Independent commentators (legal scholars, human rights organizations) remain divided: human rights bodies support extraterritorial protection of rights; sovereignty and trade-focused institutions oppose it. No unanimous corroboration exists outside the EU's own institutional interests.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   The extractiveness metric (0.68) reflects the high compliance burden placed on non-EU controllers—Article 3(2) effectively imposes EU privacy law on the entire world where EU residents are served, without corresponding benefits for non-EU actors. The measurement series shows rising extractiveness from 2018 (0.54) to 2024 (0.68), driven by accumulating enforcement action, settled cases establishing precedent, and growing compliance infrastructure costs. Suppression rises sharply from 2018 (0.38) to 2020 (0.45), reflecting the period after GDPR entry when enforcement became active and non-EU platforms faced meaningful fines and injunctions; it stabilizes at 0.52 by 2022 as major platforms adopted compliance and accepted the new normal rather than fighting jurisdiction. Theater ratio rises throughout the interval (0.12 to 0.28), indicating increasing proportion of enforcement activity devoted to defending the jurisdictional claim itself (through WTO disputes, adequacy negotiations, trade threats) rather than addressing substantive data protection violations. The effects jurisdiction reading sustains itself through this theater because EU institutional interest in regulatory authority, combined with fundamental-rights framing, makes the claim legitimate to EU audiences even when it extracts costs from non-EU actors. Accessibility collapse is high (0.71) because the effects jurisdiction reading has become the dominant legal interpretation; non-EU platforms have limited alternative legal positions within the GDPR's text. Resistance is moderate-high (0.62) because major platforms continue to contest enforcement in courts, engage in adequacy negotiations, and lobby for carve-outs, but face structural defeat because they cannot avoid serving EU markets.
 *
 * PERSPECTIVAL GAP:
 *   The EU institutional seat (Commission, EDPB) reads Article 3(2) as a legitimate extension of fundamental rights protection to match modern data flows: from their perspective, the effects jurisdiction reading is pure coordination, solving a genuine problem (unaccountable non-EU processors harming EU residents). From the non-EU platform seat, the same rule operates as extraterritorial extraction: they pay compliance costs for serving EU markets without having designed or consented to EU regulatory frameworks. From the EU-resident seat, it is genuine protection; from the non-EU developer seat, it is an exclusionary cost barrier. The engine computes these divergent types from the structural data: the EU institutional agenda-setter sees coordination (beneficiary, low directionality); the non-EU payer sees extraction (high directionality target). The claim/metric independence is intentional: the constraint is CLAIMED as tangled_rope (the EU's legitimate framing: real coordination + active enforcement) while the authored metrics describe substantially extractive operation (high compliance costs, rising theater, non-EU resistance). This gap is exactly what the framework measures—whether the coordination claim matches the actual operation.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects are beneficiaries with constrained exit (d ≈ 0.25): they gain substantive protection that follows them globally, but cannot exit digital services without significant social cost, so their exit options do not amplify the beneficiary position. The European Commission and EDPB are agenda-setters with analytical exit (d ≈ 0.15): they set the rules, control interpretation, and gain institutional authority, with no real exit cost. Non-EU technology platforms are targets with mobile exit (d ≈ 0.75): they bear compliance costs and fine exposure, but retain the option to geo-block or reduce EU-market presence, which prevents full-target lock-in. The directionality derivation places them near the target end because the constraint's persistence does not depend on their agreement—the EU enforces it unilaterally—but their mobile exit options prevent the d value from reaching 1.0 (trapped). National data protection authorities sit between (d ≈ 0.40): they gain authority but also bear administrative burden and legal challenge costs, and their exit is constrained by EU hierarchy. Third-country governments are trapped payers (d ≈ 0.85): they bear indirect costs but lack exit (geopolitical and trade dependence prevents them from simply rejecting EU regulatory reach).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unaccountable non-EU processors harming EU residents, privacy rights bounded by territory in a globalized data economy) is CONTESTED: the EU Commission attests it is live and urgent; non-EU governments and platforms contest whether it is framed accurately or whether the effects jurisdiction reading overstates the problem and the solution. The disappearance verdict is WORLD_REARRANGES, indicating arrangements depend on the constraint. This combination (contested founding problem + world_rearranges + high extraction + high theater) flags a potential mandate slippage: if the founding problem becomes sufficiently contested or is superseded (e.g., by international data protection standards agreed outside the GDPR, or by rival enforcement frameworks), the constraint could persist through institutional inertia and theater rather than mission accomplishment. The theater ratio (rising from 0.12 to 0.28 over the interval) suggests the constraint is increasingly maintained through defense of the jurisdiction claim itself rather than substantive data protection activity. This is consistent with early mandatrophy but not definitive: a constraint can legitimately shift focus to jurisdiction defense when that jurisdiction is under external challenge (trade disputes, adequacy negotiations, alternative frameworks). The mandatrophy verdict remains NOT RESOLVED because the EU's fundamental-rights framing gives the constraint intrinsic legitimacy independent of whether the original founding problem remains live—rights protection does not become illegitimate if the original threat recedes. A true mandate death for this constraint would require either (a) a new global data protection standard that supersedes Article 3(2)'s extraterritoriality, making the reading obsolete, or (b) a successful global coalition (major trading partners, UN, WTO) formally rejecting the effects jurisdiction reading and establishing a territorial norm. Neither has occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    territorial_sovereignty_challenge,
    'Is the EU''s effects jurisdiction reading a legitimate exercise of regulatory authority, or does it exceed the territorial boundaries of sovereign lawmaking?',
    'International legal consensus (UN, WTO, bilateral treaties) codifying jurisdictional limits; successful legal challenge in a major trading partner''s courts establishing that GDPR extraterritoriality conflicts with public international law; negotiated global data protection framework that supersedes Article 3(2)''s reading.',
    'If territorial sovereignty reading prevails, Article 3(2) would be reinterpreted as applying only to controllers within the EU, and the constraint would shift to market_access_reading (conditional entry) or territorial_sovereignty_reading (bounded authority). The effects jurisdiction reading would be classified as foreclosed by a competing authority framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(territorial_sovereignty_challenge, conceptual, 'Whether effects jurisdiction is a legitimate regulatory move or an overreach of territorial authority.').

omega_variable(
    fundamental_rights_vs_market_power,
    'Is the extraterritorial reach of Article 3(2) motivated by genuine data subject protection (fundamental rights), or is it instrumentalizing rights-protection language to extend EU market and regulatory power?',
    'Comparative analysis of GDPR enforcement patterns: if enforcement concentrates on dominant platforms and large processing operations (suggesting market regulation), the reading is more extractive than rights-protective. If enforcement distributes across all processing types proportionally to risk (suggesting rights-focused), the fundamental rights motivation is stronger. Discourse analysis of EU institutional framing in different contexts (rights language in EU forums; market/competition language in WTO disputes; adequacy language in third-country negotiations).',
    'If enforcement is revealed as market-power motivated (high extraction without proportional benefit for subjects), the constraint could reclassify from tangled_rope toward snare. If rights-protection motivation is vindicated, tangled_rope remains stable and the constraint''s legitimacy increases despite extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_rights_vs_market_power, empirical, 'Whether Article 3(2) enforcement is driven by data subject protection or regulatory market power.').

omega_variable(
    third_country_sovereignty_framing_contest,
    'Is the effects jurisdiction reading codifying a legitimate global privacy right, or is it a reassertion of EU market hegemony dressed in human-rights language?',
    'Evolution of third-country trade positions and adequacy negotiations: if third countries accept the effects jurisdiction reading (formally adopt GDPR-aligned standards, negotiate adequacy, incorporate Article 3(2) logic into domestic law), the global legitimacy of the reading increases. If third countries establish counter-coalitions or alternative standards explicitly rejecting effects jurisdiction (CCPA and its territoriality, Indian DPA model, Chinese data governance), the reading''s legitimacy fractures.',
    'This omega distinguishes between two readings coexisting as structurally equivalent (symmetrical coexistence) versus one reading achieving global institutional dominance while others persist as localized counter-framings. If third countries formalize rejection, the constraint''s spatial scope changes: effects jurisdiction reaches globally where EU enforcement apparatus can reach, but legitimacy is contested at every boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_country_sovereignty_framing_contest, conceptual, 'Whether effects jurisdiction is a universal human-right or a hegemonic regulatory move by the EU.').

omega_variable(
    targeting_monitoring_interpretation_stability,
    'What counts as ''targeting'' or ''monitoring'' under Article 3(2)? Does this interpretation remain stable, or is it subject to drift that could expand or contract jurisdiction?',
    'EDPB guidance evolution and case law accumulation: if EDPB opinions consistently expand targeting/monitoring to include indirect targeting (affiliate networks, data brokers, inferred intent), jurisdiction grows. If opinions narrow the test to direct targeting only, jurisdiction contracts. Analysis of cases where courts ruled Article 3(2) did not apply—these set negative precedent for jurisdiction boundaries.',
    'Interpretation drift could shift the constraint''s extractiveness: narrowing the test would reduce compliance burden and move the constraint toward genuine coordination; expanding the test would increase extraction and move toward snare. This omega tracks whether the reading''s boundaries are stable or subject to mission creep.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(targeting_monitoring_interpretation_stability, empirical, 'Stability and drift of the targeting/monitoring interpretation that defines Article 3(2) scope.').

omega_variable(
    reading_kernel_ambiguity,
    'Does the text of Article 3(2) genuinely permit the effects jurisdiction reading, or have EU institutions stretched an ambiguous clause beyond its reasonable interpretation?',
    'Independent legal scholarship on the text''s ordinary meaning; comparison with legislative history and negotiation records; analysis by non-EU legal systems of what Article 3(2) would mean if read under their own interpretive traditions (common law strict construction, civil law textual literalism).',
    'If the reading is revealed as an interpretive overreach (reasonable people reading the text differently), the constraint loses doctrinal grounding and becomes vulnerable to foreclosure by a territorial reading once political conditions permit. If the reading is vindicated as the text''s most reasonable interpretation, it becomes more durable. This omega identifies the reading as potentially subject to foreclosure via interpretive reversion rather than amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, empirical, 'Whether Article 3(2) text permits effects jurisdiction reading or stretches beyond reasonable interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2018, 0.12).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2022, 0.24).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement(gdpr_tr_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2018, 0.54).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2022, 0.66).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement(gdpr_be_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2018, 0.38).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2022, 0.51).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement(gdpr_su_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2026, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__effects_jurisdiction_reading, 0.18).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, adequacy_determination_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, brussels_effect_standard_setting).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of GDPR Article 3 scope. The kernel is the text of Article 3(2) (formalized, fixed_text); three readings coexist representing different institutional positions and normative commitments: effects_jurisdiction_reading (EU institutional, fundamental rights framing, high extraction); market_access_reading (EU in trade context, conditional entry framing, moderate extraction); territorial_sovereignty_reading (third countries, sovereignty framing, low extraction from non-EU perspective). Each reading is a separate constraint with its own ε, beneficiaries, victims, and cs_structure.reading_relations. Sibling constraints must be generated from each reading's actual structural position, not averaged or compromised toward a single reading. The family link (network.affects_constraints) routes the constraint competition: effects jurisdiction reading influences adequacy and market_access readings (shapes the terms on which they operate) and coexists with territorial_sovereignty reading as a competing framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, powerful, 0.75).
constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
