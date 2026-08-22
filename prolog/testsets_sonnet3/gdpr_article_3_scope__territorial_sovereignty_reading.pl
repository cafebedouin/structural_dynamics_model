% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__territorial_sovereignty_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3(2) Extraterritorial Scope — Territorial Sovereignty Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint instantiates the territorial-sovereignty reading of the
 *   contested GDPR Article 3(2) scope kernel: the claim that the provision's
 *   extraterritorial application — reaching controllers and processors with
 *   no establishment in the EU whenever they offer goods/services to or
 *   monitor EU residents — exceeds what legitimate regulatory jurisdiction,
 *   grounded in territorial sovereignty, permits. On this reading, the
 *   mechanism chosen to protect EU residents' data has metastasized from a
 *   coordination fix into a unilateral extension of EU regulatory authority
 *   into other states' domestic governance, provoking data localization
 *   statutes and blocking legislation as resistance mechanisms and escalating
 *   jurisdictional conflict between the EU and third-country regulators. This
 *   is a distinct constraint from the effects_jurisdiction_reading (which
 *   treats the same Article 3(2) text as legitimately grounded in a
 *   targeting/monitoring nexus test) and from the market_access_reading
 *   (which treats extraterritoriality as a conditional-access requirement,
 *   not a jurisdictional assertion at all) — each reading has a different
 *   beneficiary structure and a different ε, and is authored as its own
 *   constraint story per the ε-invariance principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.61).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.52).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3(2) Extraterritorial Scope — Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '4b858c5d-3cf5-428d-b43b-f20f3115db45').
narrative_ontology:cs_kernel_codification('4b858c5d-3cf5-428d-b43b-f20f3115db45', formalized).
narrative_ontology:cs_authority_grounding('4b858c5d-3cf5-428d-b43b-f20f3115db45', extraction).
narrative_ontology:cs_interpretation_layer_present('4b858c5d-3cf5-428d-b43b-f20f3115db45').
narrative_ontology:cs_reading_relation('4b858c5d-3cf5-428d-b43b-f20f3115db45', gdpr_article_3_scope__effects_jurisdiction_reading, forecloses).
narrative_ontology:cs_reading_relation('4b858c5d-3cf5-428d-b43b-f20f3115db45', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('4b858c5d-3cf5-428d-b43b-f20f3115db45', foundational, territorial_situs_of_conduct_bounds_legitimate_jurisdiction).
narrative_ontology:cs_axiom_status(territorial_situs_of_conduct_bounds_legitimate_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('4b858c5d-3cf5-428d-b43b-f20f3115db45', territorial_situs_of_conduct_bounds_legitimate_jurisdiction, conventional).
narrative_ontology:cs_axiom('4b858c5d-3cf5-428d-b43b-f20f3115db45', secondary, consent_or_reciprocity_required_for_binding_foreign_regulatory_authority).
narrative_ontology:cs_axiom_status(consent_or_reciprocity_required_for_binding_foreign_regulatory_authority, holdable).
narrative_ontology:cs_axiom_grounding('4b858c5d-3cf5-428d-b43b-f20f3115db45', consent_or_reciprocity_required_for_binding_foreign_regulatory_authority, conventional).
narrative_ontology:cs_reference_frame('4b858c5d-3cf5-428d-b43b-f20f3115db45', westphalian_territorial_jurisdiction).
narrative_ontology:cs_drift_state('4b858c5d-3cf5-428d-b43b-f20f3115db45', post_edpb_guidelines_2018_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4b858c5d-3cf5-428d-b43b-f20f3115db45', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_domiciled_competitors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_domiciled_processors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulatory_sovereignty).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, small_non_eu_online_service_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce Article 3(2) to reach any controller or processor anywhere in the world that offers goods/services to or monitors EU residents, regardless of where the entity is incorporated, operates its servers, or where its own legal system sits. They set the extraterritorial reach through guidance (EDPB Guidelines 3/2018) and enforcement actions, and collect fines and compliance leverage from entities that have no seat, employees, or assets within the Union.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities, beneficiary).

% Compete against foreign firms that would otherwise face lighter compliance burdens; the extraterritorial reach levels a cost that EU-domiciled firms already bear structurally by virtue of being subject to the regulation at home. They benefit indirectly from a rule that imposes matching costs on offshore rivals.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_domiciled_competitors, beneficiary,
    organized, biographical, mobile, continental).

% Firms incorporated and operating entirely outside the EU, with no physical presence there, that nonetheless must build GDPR-compliant data pipelines, appoint EU representatives, and expose themselves to EU enforcement jurisdiction merely because EU residents can access their service online. Genuine exit — simply refusing compliance — risks EU market exclusion, asset seizure of any EU-reachable holdings, and reputational contagion in third markets; the practical alternative is geo-blocking EU traffic, which many cannot afford to implement precisely.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_domiciled_processors, payer,
    moderate, biographical, constrained, global).

% The abstract principle that a state's own regulatory framework — its own privacy law, its own enforcement priorities, its own balance between innovation and protection — governs conduct occurring on its own soil. Article 3(2), on this reading, displaces that framework by having EU authorities effectively set compliance requirements for firms operating under a different state's law, without that state's consent or reciprocal mechanism. Named for completeness; it does not act, but its erosion is the story's central claim.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulatory_sovereignty, payer,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_non_agent(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulatory_sovereignty).

% Small businesses and independent operators outside the EU — a regional e-commerce site, a niche SaaS tool — that acquire EU users incidentally through the open internet, without ever targeting the EU market deliberately. They often lack the legal sophistication to determine whether Article 3(2)'s 'offering goods or services' or 'monitoring behaviour' tests apply to them, and lack the resources to geo-block, litigate, or appoint an EU representative. Their practical choice is uninformed noncompliance and hoped-for obscurity, or costly retreat from an open internet posture.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, small_non_eu_online_service_providers, payer,
    powerless, biographical, trapped, global).

% Elected bodies outside the EU that write their own data protection statutes calibrated to domestic conditions and priorities. They have no vote, veto, or formal consultative role in how the EDPB or CJEU interprets Article 3(2)'s reach into their jurisdictions, and can only respond after the fact through countermeasures like data localization mandates or blocking statutes.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_national_legislatures, excluded,
    institutional, generational, constrained, national).

% The Court of Justice of the EU and the European Data Protection Board adjudicate and interpret disputed applications of Article 3(2), including how far the targeting and monitoring tests extend. They shape doctrine that either narrows or widens the sovereignty conflict, and are the forum where the territorial-sovereignty objection is formally raised and (so far) largely rejected.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, cjeu_and_edpb, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, cjeu_and_edpb, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__territorial_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within EU territory, Article 3(2) coordinates a genuine collective-action problem: without extraterritorial reach, EU residents' data would be trivially exported to jurisdictions with weaker protections, hollowing out the regulation's protective core. Read through the territorial-sovereignty lens, however, the mechanism chosen to solve that problem — asserting jurisdiction over conduct occurring entirely outside EU territory — exceeds what coordinating protection for EU residents requires and instead constitutes a unilateral extension of regulatory authority into other states' domestic governance.
% TRANSFER_FUNCTION: Moves compliance cost, legal exposure, and de facto rule-setting authority from EU institutions to entities and states with no seat at the table: non-EU firms absorb the cost of building EU-compliant systems for incidental EU traffic, and non-EU legislatures see their own calibrated privacy frameworks superseded in practice by EU standards for any firm that wants global reach.
% ABSENT_VOICES: Non-EU national legislatures and their domestic regulatory agencies have no formal role in interpreting or narrowing Article 3(2)'s reach into their own jurisdictions; they discover the scope of the constraint through EDPB guidance and CJEU rulings issued without their participation, and can only respond reactively through countermeasures such as data localization statutes or blocking legislation.
% DISAPPEARANCE_RATIONALE: If Article 3(2)'s extraterritorial reach were struck down or abandoned tomorrow, non-EU firms serving EU residents would face no direct EU enforcement exposure absent an EU establishment; compliance would revert to voluntary adoption or purely commercial pressure (the Brussels Effect without the jurisdictional teeth), data localization mandates built as defensive countermeasures in several states would lose their primary justification, and the EDPB/CJEU's global enforcement docket against foreign-domiciled entities would collapse. Non-EU legislatures would regain uncontested primacy over privacy rules governing conduct on their own soil.
% FOUNDING_PROBLEM: In an internet economy where data crosses borders trivially, a purely territorial reading of 'establishment in the EU' would let any firm evade EU privacy protections for EU residents simply by locating servers and headquarters offshore, making the substantive protections in the Regulation meaningless for anyone dealing with a foreign-domiciled service.
% FOUNDING_PROBLEM_CORROBORATION: EU regulators and EU-domiciled firms attest the problem remains live and that extraterritorial reach is the only workable fix. Independent international law scholars (e.g., writing in the American Journal of International Law and in submissions to the UN Special Rapporteur on privacy) and several non-EU national legislatures and trade ministries — parties outside the EU's own regulatory apparatus — attest instead that the mechanism chosen has moved well past solving the evasion problem into unilateral standard-setting for conduct occurring entirely on other states' territory, corroborating the territorial-sovereignty reading's claim of overreach rather than the EU's own self-assessment of necessity.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.38 to 0.61) as EDPB guidance and CJEU rulings progressively widen the practical reach of the targeting/monitoring tests, drawing more non-EU firms with thinner EU nexus into compliance exposure — from clear cases (a US retailer shipping to EU customers) toward marginal ones (a regional SaaS tool with incidental EU signups). Suppression tracks a parallel rise (0.30 to 0.52) as enforcement infrastructure matures: EU representative-appointment requirements, cross-border enforcement cooperation agreements, and the practical difficulty of geo-blocking precisely enough to escape scope all harden over time. Theater ratio stays moderate-low (0.30) because the underlying protective function for EU residents is real even on this reading — the objection is to the reach of the mechanism, not to its complete absence of function. Resistance is high (0.72) because this reading is precisely the one under which resistance — data localization laws, blocking statutes, diplomatic protest — is the expected and observed response.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat (EU DPAs, CJEU/EDPB) experiences Article 3(2) as coordination completing an obviously necessary protective function — data doesn't respect borders, so neither can enforcement. The payer seats (non-EU processors, small providers, and the sovereignty interest itself) experience the identical text as unilateral rule-export: a EU institution setting binding compliance requirements for conduct on their own soil, without their consent, vote, or reciprocal obligation running the other direction. Both readings describe the same rule; the divergence is exactly what the territorial-sovereignty reading is built to name.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data protection authorities and EU-domiciled competitors sit at the beneficiary end: the former administers and gains enforcement authority and fine revenue reaching far beyond EU borders; the latter gains a competitive leveling effect against offshore rivals. Non-EU domiciled processors, small non-EU providers, and non-EU state regulatory sovereignty itself sit at the target end: they bear compliance cost, enforcement exposure, or doctrinal displacement without having participated in setting the rule. Small non-EU providers are weighted toward the most extreme target position (trapped exit) because they lack the resources of larger non-EU processors to geo-block or litigate — same nominal position in the victim class, materially worse exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (data trivially evading protection via offshore relocation) was real in 1995-era Directive terms and remains partially live for firms deliberately structuring around EU nexus. But this reading holds that the chosen fix has outrun the problem: the current targeting/monitoring test sweeps in firms with no intent to evade anything, merely incidental EU reach, converting a narrowly-justified anti-evasion mechanism into a general assertion of extraterritorial authority. Classifying this as tangled_rope rather than snare preserves the genuine coordination residue (protecting EU residents from data export is not pure pretext) while recognizing the asymmetric extraction imposed on non-EU parties who receive no corresponding voice or benefit — a pure snare framing would deny the coordination function this reading itself grants is partially real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_versus_evasion_prevention_boundary,
    'Is there a principled line between ''jurisdiction necessary to prevent trivial evasion of EU residents'' data protections'' and ''jurisdiction that constitutes unilateral extraterritorial rule-setting over conduct on foreign soil,'' or is the distinction itself contested along the same lines as the kernel dispute?',
    'Comparative international law analysis of accepted extraterritoriality doctrines (effects doctrine in antitrust, protective principle in criminal law) applied to the specific targeting/monitoring test language, cross-checked against how non-EU courts and legislatures have formally responded (blocking statutes, mutual legal assistance refusals, WTO-adjacent disputes).',
    'If a principled line exists and Article 3(2) falls within it, the territorial-sovereignty reading''s overreach claim weakens substantially. If no such line exists or Article 3(2) falls outside accepted extraterritoriality doctrine, the reading''s core claim is corroborated by established international law categories rather than resting on contested framing alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_versus_evasion_prevention_boundary, conceptual, 'Whether the sovereignty-overreach claim maps to an existing, non-question-begging international law distinction.').

omega_variable(
    resistance_as_evidence_or_self_fulfilling,
    'Does the observed rise in data localization statutes and blocking legislation among non-EU states constitute independent evidence that Article 3(2) exceeds legitimate jurisdictional bounds, or does the territorial-sovereignty reading''s own framing (and this story''s authoring choice to treat such statutes as ''resistance'') partly manufacture that interpretation of otherwise routine data-policy divergence?',
    'Trace the explicit legislative history and stated justifications of specific non-EU data localization statutes (e.g., India''s DPDP Act provisions, various Latin American and Southeast Asian localization mandates) to determine how many cite GDPR extraterritorial reach as a motivating grievance versus independent domestic policy goals.',
    'If most localization statutes cite GDPR reach explicitly as provocation, the reading''s claim that extraterritoriality drives jurisdictional conflict escalation is well-grounded. If most have independent domestic motivations, this reading may overstate its own explanatory power for what is actually a broader multipolar data-governance trend.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_as_evidence_or_self_fulfilling, empirical, 'Whether observed resistance mechanisms are caused by, or merely coincide with, the extraterritorial reading''s asserted overreach.').

omega_variable(
    cs_framing_kernel_versus_supra_legitimacy_claim,
    'Is the relevant kernel the Article 3(2) text itself (as adjudicated by CJEU/EDPB), or is there a less obvious kernel — the deeper claim of EU regulatory legitimacy to act as a de facto global standard-setter — layered above the text and doing the actual work in this reading''s overreach argument?',
    'Examine whether territorial-sovereignty objections target the specific statutory language of Article 3(2) or instead target the broader pattern of EU regulatory ambition (GDPR, DMA, AI Act) treated as a single legitimacy question; if objections are consistently raised at the pattern level rather than the clause level, the true kernel may be ''EU claim to global regulatory authority'' rather than ''Article 3(2) scope'' specifically.',
    'If the true kernel is the broader legitimacy claim, this story''s cs_structure (kernel_codification: formalized, tied to the specific statutory text) may understate the contest — the real dispute might be better modeled as a reading of a much larger kernel spanning multiple EU digital regulations, changing which siblings and axioms are relevant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_versus_supra_legitimacy_claim, conceptual, 'Whether Article 3(2) is the actual kernel or a visible instance of a broader, unstated EU-global-authority kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(gdpr_tr_t16, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(gdpr_tr_t24, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(gdpr_be_t16, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(gdpr_be_t24, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(gdpr_su_t12, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(gdpr_su_t16, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(gdpr_su_t24, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, data_localization_mandates_non_eu_states).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings decomposed from the single colloquial label 'GDPR's extraterritorial scope' per the ε-invariance principle: territorial_sovereignty_reading (this file, ε=0.61, tangled_rope), effects_jurisdiction_reading (legitimate targeting-nexus jurisdiction, lower ε, likely rope or tangled_rope with a narrower victim set), and market_access_reading (Brussels Effect conditional access, ε likely lower still, plausibly rope). Each carries its own beneficiary/victim structure and its own claimed_type; they are linked here rather than merged because measuring 'GDPR extraterritoriality' by the sovereignty-violation observable versus the market-access observable produces materially different ε values — exactly the signal the ε-invariance test is designed to catch. Also linked to data_localization_mandates_non_eu_states as a downstream effect this reading identifies as a resistance mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
