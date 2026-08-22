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
 *   human_readable: GDPR Article 3(2) Extraterritorial Scope — Effects/Targeting Jurisdiction Reading
 *   domain: technology governance / international law / privacy regulation
 *
 * SUMMARY:
 *   Article 3(2) of the GDPR extends EU data protection obligations to
 *   controllers and processors with no EU establishment, so long as they
 *   offer goods or services to EU residents or monitor their behavior. This
 *   story authors the effects-jurisdiction reading of that provision:
 *   jurisdiction is grounded in the effects of data processing on EU
 *   residents, not in the physical location of the processing entity. Under
 *   this reading, a targeting/monitoring nexus is a legitimate and sufficient
 *   basis for regulatory authority, paralleling effects-based jurisdiction
 *   doctrines in competition and securities law. This is a distinct
 *   constraint from the market_access_reading (which treats the same text as
 *   a conditional-access toll rather than a jurisdictional claim, producing a
 *   lower suppression/enforcement-coercion profile and different beneficiary
 *   framing) and from the territorial_sovereignty_reading (which treats the
 *   identical text as an illegitimate assertion of authority beyond the EU's
 *   territory, and would author near-zero coordination function and a much
 *   higher victim-weighted extraction picture). Each reading is authored as
 *   its own constraint story with its own ε and stakeholder set, per the
 *   ε-invariance principle; they are linked here only through network and
 *   cs_structure fields, not blended.
 *
 * KEY AGENTS:
 *   - eu_data_subjects: primary beneficiary (organized/constrained) — protection follows them wherever their data goes
 *   - eu_data_protection_authorities: agenda_setter (institutional/analytical) — define and enforce the targeting/monitoring test
 *   - non_eu_controllers_without_eu_establishment: primary target (moderate/trapped) — bear compliance cost with no jurisdictional escape
 *   - non_eu_ad_tech_and_analytics_firms: primary target (powerful/constrained) — captured by the monitoring prong specifically
 *   - foreign_governments_and_regulators: excluded — bear diplomatic/economic spillover with no rulemaking voice
 *   - eu_courts_and_edpb: analytical observer — refine the test's boundaries case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.58).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.61).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritorial Scope — Effects/Targeting Jurisdiction Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology governance / international law / privacy regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '1343f3aa-7b4e-45a2-8596-b2fadf2152af').
narrative_ontology:cs_kernel_codification('1343f3aa-7b4e-45a2-8596-b2fadf2152af', formalized).
narrative_ontology:cs_authority_grounding('1343f3aa-7b4e-45a2-8596-b2fadf2152af', extraction).
narrative_ontology:cs_interpretation_layer_present('1343f3aa-7b4e-45a2-8596-b2fadf2152af').
narrative_ontology:cs_reading_relation('1343f3aa-7b4e-45a2-8596-b2fadf2152af', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('1343f3aa-7b4e-45a2-8596-b2fadf2152af', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('1343f3aa-7b4e-45a2-8596-b2fadf2152af', foundational, effects_on_residents_ground_jurisdiction).
narrative_ontology:cs_axiom_status(effects_on_residents_ground_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('1343f3aa-7b4e-45a2-8596-b2fadf2152af', effects_on_residents_ground_jurisdiction, conventional).
narrative_ontology:cs_axiom('1343f3aa-7b4e-45a2-8596-b2fadf2152af', secondary, targeting_monitoring_nexus_is_sufficient_basis).
narrative_ontology:cs_axiom_status(targeting_monitoring_nexus_is_sufficient_basis, holdable).
narrative_ontology:cs_axiom_grounding('1343f3aa-7b4e-45a2-8596-b2fadf2152af', targeting_monitoring_nexus_is_sufficient_basis, instrumental).
narrative_ontology:cs_reference_frame('1343f3aa-7b4e-45a2-8596-b2fadf2152af', territorial_establishment_jurisdiction).
narrative_ontology:cs_drift_state('1343f3aa-7b4e-45a2-8596-b2fadf2152af', post_gdpr_enforcement_maturation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1343f3aa-7b4e-45a2-8596-b2fadf2152af', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_based_digital_service_providers).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_smes_targeting_eu_users).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_ad_tech_and_analytics_firms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers_without_eu_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals in the EU whose personal data is collected or monitored by any controller worldwide, provided goods/services are offered to them or their behavior is monitored. They gain a uniform protection floor regardless of where the processor is based, and rely on regulators and courts to enforce it since they individually cannot pursue foreign controllers.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, generational, constrained, continental).

% Already compliant by virtue of establishment in the EU, they benefit competitively when foreign rivals must absorb the same compliance costs to serve EU customers, narrowing a cost advantage foreign entrants would otherwise hold.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_based_digital_service_providers, beneficiary,
    powerful, biographical, mobile, continental).

% Interpret and enforce Article 3(2)'s targeting/monitoring test, issue guidance on what counts as 'offering goods or services' or 'monitoring behavior,' investigate non-EU controllers, and impose fines. Their authority and budget expand with the scope of extraterritorial reach they successfully assert.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Small foreign businesses that accept EU customers or run EU-facing marketing must build GDPR-compliant data handling, appoint an EU representative, and face enforcement risk despite having no physical presence in the EU. Exiting the EU market is possible but often commercially costly; compliance costs are largely fixed and fall disproportionately hard on smaller operations.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_smes_targeting_eu_users, payer,
    moderate, biographical, constrained, global).

% Large data-driven firms whose core business is monitoring behavior (tracking, profiling, ad targeting) are squarely captured by the 'monitoring' prong regardless of physical location. They have resources to comply but describe the extraterritorial reach as regulatory overreach into activity occurring on their own servers, under their own laws, targeting a market they did not physically enter.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_ad_tech_and_analytics_firms, payer,
    powerful, biographical, constrained, global).

% Entities with no EU office or subsidiary but with EU users are pulled into the EU's enforcement orbit purely through the effects of their processing. They cannot easily contest jurisdiction because the targeting/monitoring test looks at conduct effects, not physical presence, leaving them exposed to fines enforceable through international cooperation mechanisms and adequacy-linked pressure on their home jurisdictions.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers_without_eu_establishment, payer,
    moderate, biographical, trapped, global).

% Governments whose domestic firms are regulated indirectly by a foreign authority have no seat in EU rulemaking or enforcement decisions affecting their nationals, despite bearing diplomatic and economic consequences (e.g., adequacy negotiations, trade friction).
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, foreign_governments_and_regulators, excluded,
    institutional, generational, constrained, national).

% Adjudicate contested applications of the targeting/monitoring test, issue guidelines refining its boundaries, and mediate between the protective and overreach readings as disputes and enforcement actions accumulate.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_courts_and_edpb, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__effects_jurisdiction_reading, diffuse).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__effects_jurisdiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents circumvention of EU data protection law by controllers who simply relocate servers or incorporate outside the EU while continuing to target or surveil EU residents — closes the jurisdictional loophole that a purely territorial test would leave open.
% TRANSFER_FUNCTION: Moves compliance burden (legal, technical, administrative) from EU regulators and residents onto any global controller that offers goods/services to, or monitors, people in the EU; moves enforcement leverage from the country of incorporation to EU authorities.
% ABSENT_VOICES: Non-EU legislatures and regulators whose domestic firms are subject to EU rules they had no role in drafting; small non-EU businesses without resources for EU regulatory affairs representation, who are rarely consulted in EDPB guidance processes despite bearing the compliance cost.
% DISAPPEARANCE_RATIONALE: If effects-based jurisdiction were withdrawn and Article 3(2) reverted to a strict establishment/territorial test, foreign controllers with no EU presence would face no EU enforcement exposure for targeting or monitoring EU residents; EU data subjects would lose protection against exactly the class of actors (offshore ad-tech, foreign platforms) the provision was designed to reach, and EU-based competitors would lose the compliance-cost parity effects-jurisdiction currently imposes on rivals.
% FOUNDING_PROBLEM: Pre-GDPR, EU data protection law (the 1995 Directive) was widely evaded by controllers establishing outside the EU while still targeting or profiling EU residents online — jurisdiction tied to physical establishment let extraterritorial data harvesting escape EU law entirely.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic legal scholarship (e.g., analyses of pre-GDPR enforcement gaps against non-EU search and social platforms) and non-EU regulators negotiating adequacy and cross-border enforcement cooperation both attest that offshore targeting of EU residents remains a live evasion vector; this corroboration comes from outside the EU data protection authorities who administer and benefit from the expanded scope.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate-high (0.58 at interval end) and rising: as EDPB guidance and enforcement actions accumulate, more borderline foreign actors are pulled within scope, and the compliance burden compounds without a corresponding narrowing of who counts as 'targeted.' Suppression (0.61) reflects the real coercive lever — fines up to 4% of global turnover, enforceable through EU representative requirements and adequacy-linked pressure on data transfers — which foreign controllers cannot simply out-lawyer their way around once EU effects exist. Theater ratio stays low (0.22) because enforcement is substantively active (real fines, real investigations) rather than symbolic. Accessibility collapse is moderate (0.45): controllers can in principle exit the EU-facing market to escape scope, but for many digital businesses EU users are not cleanly severable from a global service, so the alternative of non-targeting is often commercially unavailable in practice, not just legally difficult.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects and EU-domiciled competitors sit near the beneficiary end: subjects gain protection they could not otherwise enforce against offshore actors, and EU firms gain a leveling of compliance costs against foreign rivals. Non-EU controllers without EU establishment sit near the full-target end — trapped, because the targeting/monitoring test does not permit an opt-out based on absence of physical presence, only cessation of the targeting conduct itself, which for many is commercially equivalent to exiting the market. Non-EU ad-tech firms are powerful but still constrained: their scale gives them compliance capacity but not jurisdictional escape, since the monitoring prong was drafted with exactly their business model in mind.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — evasion of EU protection via offshore incorporation — remains live (corroborated by scholarship and by foreign regulators' continued negotiation over cross-border enforcement), which weighs against treating this as inertial mandatrophy. But the tangled_rope classification (rather than rope) is warranted because the same enforcement machinery that closes the evasion loophole also captures actors whose EU nexus is thin (a small non-EU SME with a handful of EU customers), imposing costs disproportionate to any protective gain. The genuine coordination function (preventing extraterritorial evasion) and the asymmetric extraction (uniform fixed compliance costs falling hardest on smaller foreign actors, enforced by an authority with growing institutional stake in broad scope) coexist in the same structure, which is exactly the tangled_rope signature rather than either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effects_test_scope_creep,
    'Does the targeting/monitoring test have a principled stopping point, or does it expand indefinitely as EDPB guidance and case law accumulate to capture ever more attenuated EU contacts?',
    'Track the EDPB''s and CJEU''s evolving interpretation of ''offering goods or services'' and ''monitoring behavior'' over a multi-year window; compare the population of entities found in-scope at T0 versus T+10 years for equivalent fact patterns.',
    'If the test''s boundary is stable, the tangled_rope reading holds with a bounded victim class; if scope keeps creeping to capture thinner and thinner EU contacts, the classification drifts toward snare as the coordination rationale (preventing evasion) increasingly fails to justify the captured population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effects_test_scope_creep, empirical, 'Whether effects-based jurisdiction under Article 3(2) has a stable or an expanding boundary.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is Article 3(2) genuinely a jurisdictional assertion (this reading), a conditional-market-access mechanism (market_access_reading), or an illegitimate extraterritorial overreach (territorial_sovereignty_reading) — or is the text itself underdetermined between these, with the ''true'' answer depending on which international law framework the reader brings?',
    'Comparative analysis of how EU courts, foreign courts, and international law scholars characterize the provision''s jurisdictional basis; track whether adequacy negotiations and mutual legal assistance treaties implicitly adopt one reading over the others.',
    'If courts and treaty practice converge on one reading, the sibling readings lose practical force even though they remain logically statable; if no convergence occurs, all three readings persist as live, separately-instantiated constraints indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel text itself resolves toward one reading or remains genuinely contested across all three.').

omega_variable(
    compliance_cost_incidence,
    'Do non-EU SMEs actually bear the compliance cost directly, or is it passed through to EU consumers via price increases, effectively making EU residents partial payers of the protection regime they benefit from?',
    'Empirical pass-through studies of pricing changes for EU-facing digital services from non-EU providers following GDPR compliance investment.',
    'High pass-through would mean EU data subjects are not pure beneficiaries but partially self-fund their own protection through higher prices, softening the beneficiary/victim asymmetry authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_cost_incidence, empirical, 'Whether compliance costs are absorbed by foreign controllers or passed through to EU consumers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(gdpr_tr_t16, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(gdpr_tr_t24, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(gdpr_be_t16, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(gdpr_be_t24, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(gdpr_su_t12, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(gdpr_su_t16, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(gdpr_su_t24, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 24, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the gdpr_article_3_scope kernel. effects_jurisdiction_reading (this file) authors moderate-high extraction with real coordination function (closing offshore evasion) and real asymmetric cost (fixed compliance burden falling hardest on smaller foreign actors) — a tangled_rope. territorial_sovereignty_reading would author much higher extraction and near-zero coordination benefit, since from that reading's premises the entire extraterritorial apparatus is illegitimate overreach with no valid coordination function to offset the cost it imposes — likely a snare. market_access_reading would author a different ε again: framed as a voluntary conditional-access toll rather than a jurisdictional claim, coercion and suppression read lower because non-compliant firms are understood to simply forgo the EU market rather than being subjected to extraterritorial enforcement — likely a rope or scaffold depending on how the Brussels Effect's permanence is authored. The three do not share an ε; they are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
