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
 *   human_readable: GDPR Article 3(2) Extraterritorial Effects-Based Jurisdiction
 *   domain: technology governance/international law/privacy regulation
 *
 * SUMMARY:
 *   This constraint instantiates the effects-jurisdiction reading of GDPR
 *   Article 3(2): jurisdiction attaches to the EU residency of the data
 *   subject and the targeting/monitoring conduct of the controller, not to
 *   territorial presence or market-access conditionality. Under this reading,
 *   a non-EU firm with no EU establishment that offers goods or services to,
 *   or monitors the behavior of, EU residents falls squarely within scope,
 *   and enforcement follows through fines, EU representative requirements,
 *   and adequacy-linked cooperation mechanisms. This is a distinct constraint
 *   from the market-access reading (which frames the same text as a Brussels
 *   Effect standard-setting device with no jurisdictional claim) and the
 *   territorial-sovereignty reading (which holds the same extraterritorial
 *   application to exceed legitimate regulatory authority). Each carries a
 *   different ε, a different beneficiary/victim structure, and a different
 *   classification; they are linked here only through
 *   network.affects_constraints, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - eu_data_subjects: primary beneficiary (organized/constrained) — protection follows them regardless of controller location
 *   - eu_data_protection_authorities: agenda_setter (institutional/analytical) — apply and enforce the targeting/monitoring test
 *   - non_eu_controllers_targeting_eu_market: primary target (powerful/constrained) — bear compliance cost and fine exposure
 *   - small_non_eu_online_businesses: secondary target (moderate/trapped) — disproportionate compliance uncertainty relative to resources
 *   - eu_based_competitors: secondary beneficiary (organized/mobile) — competitive leveling effect
 *   - non_eu_national_governments: excluded (institutional/constrained) — no voice, sovereignty functionally overridden for EU-facing conduct
 *   - cjeu_and_edpb: analytical observer (institutional/analytical) — interpret and bound the test's reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.58).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.62).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritorial Effects-Based Jurisdiction").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology governance/international law/privacy regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '83b61243-dc70-4d5c-a93d-4b446c9cc3c5').
narrative_ontology:cs_kernel_codification('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', formalized).
narrative_ontology:cs_authority_grounding('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', extraction).
narrative_ontology:cs_interpretation_layer_present('83b61243-dc70-4d5c-a93d-4b446c9cc3c5').
narrative_ontology:cs_reading_relation('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', foundational, protection_follows_the_data_subject).
narrative_ontology:cs_axiom_status(protection_follows_the_data_subject, holdable).
narrative_ontology:cs_axiom_grounding('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', protection_follows_the_data_subject, deontological).
narrative_ontology:cs_axiom('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', foundational, targeting_conduct_grounds_jurisdiction_not_territory).
narrative_ontology:cs_axiom_status(targeting_conduct_grounds_jurisdiction_not_territory, holdable).
narrative_ontology:cs_axiom_grounding('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', targeting_conduct_grounds_jurisdiction_not_territory, conventional).
narrative_ontology:cs_reference_frame('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', effects_based_protective_jurisdiction).
narrative_ontology:cs_drift_state('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', post_schrems_ii_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83b61243-dc70-4d5c-a93d-4b446c9cc3c5', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_based_competitors).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers_targeting_eu_market).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_online_businesses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents in the EU whose personal data is collected by non-EU controllers offering goods/services to them or monitoring their behavior. Under this reading they retain the same protections regardless of where the controller is incorporated or where processing occurs; they cannot themselves relocate to escape data practices, so extraterritorial reach is the mechanism that follows them.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, generational, constrained, continental).

% Apply the targeting/monitoring test under Article 3(2) to determine whether a non-EU controller falls within scope, then investigate and fine controllers found to process EU residents' data without complying with GDPR obligations. Their authority to reach outside EU territory rests entirely on the effects doctrine; enforcement leverage comes from fines, adequacy findings, and cooperation with EU-based intermediaries.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Large non-EU platforms and data brokers that knowingly offer goods/services to or monitor EU residents. They must build GDPR-compliant data architectures, appoint EU representatives, and face fines up to 4% of global turnover, even though they have no physical presence in the EU. Exit means abandoning the EU market entirely, which is commercially costly for firms with meaningful EU user bases.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers_targeting_eu_market, payer,
    powerful, biographical, constrained, global).

% Small merchants and app developers outside the EU who incidentally attract EU visitors or customers online. They often lack the legal resources to determine whether the targeting test applies to them, and disproportionately bear compliance uncertainty and cost relative to their revenue; many simply geoblock EU users rather than assess exposure, which is itself a form of trapped exit.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_online_businesses, payer,
    moderate, biographical, trapped, national).

% Firms operating inside the EU that already bear compliance costs by virtue of territorial presence. Article 3(2)'s extraterritorial reach levels the playing field by imposing equivalent obligations on non-EU rivals competing for the same EU customers, removing a potential competitive advantage that non-EU firms would otherwise hold.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_based_competitors, beneficiary,
    organized, biographical, mobile, continental).

% Governments whose domestic firms are regulated by a foreign authority's rules despite no domestic presence of that authority. They have no vote in the EU's rulemaking or enforcement decisions affecting their firms, and their own regulatory sovereignty over firms physically located and licensed within their own territory is functionally overridden for EU-facing conduct.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_national_governments, excluded,
    institutional, generational, constrained, national).

% The Court of Justice of the EU and European Data Protection Board interpret the targeting/monitoring test, issue guidelines, and adjudicate contested scope determinations, shaping how far the effects-based reading extends in practice.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, cjeu_and_edpb, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__effects_jurisdiction_reading, diffuse).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__effects_jurisdiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a regulatory gap in which non-EU controllers could freely collect and process EU residents' personal data by routing operations offshore, ensuring that protection follows the data subject rather than the controller's place of incorporation.
% TRANSFER_FUNCTION: Moves compliance burden and enforcement exposure from EU residents (who would otherwise bear unmitigated privacy risk) onto non-EU controllers who target or monitor them; secondarily shifts competitive advantage from unregulated non-EU firms toward EU-based and already-compliant firms.
% ABSENT_VOICES: Non-EU governments and their domestic regulators have no formal voice in how the targeting/monitoring test is drawn or enforced against their firms; small non-EU businesses uncertain of their exposure are rarely represented in EDPB guideline consultations, which draw disproportionately on large-firm and EU civil-society input.
% DISAPPEARANCE_RATIONALE: If Article 3(2)'s extraterritorial reach were withdrawn, non-EU controllers offering goods or services to EU residents could process their data under home-jurisdiction rules alone; EU residents' protections would depend entirely on controller location, and much current compliance infrastructure (EU representatives, cross-border data processing agreements, geofenced compliance builds) built specifically to satisfy the effects test would become unnecessary.
% FOUNDING_PROBLEM: Purely territorial jurisdiction let non-EU controllers process EU residents' data by locating servers and incorporation outside the EU, defeating the substantive protections the GDPR was meant to guarantee — a target could be reached in Europe while the controller sat entirely beyond EU territorial writ.
% FOUNDING_PROBLEM_CORROBORATION: EU data protection authorities and academic privacy-law scholars attest the loophole was real pre-GDPR (citing the 1995 Directive's weaker extraterritorial reach and documented offshore data-processing arbitrage). Independent trade-law scholars outside the EU institutions corroborate that a territorial-only rule would have created a straightforward regulatory arbitrage path, though they dispute whether the chosen remedy's scope is proportionate to the problem it solves.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.58 (moderate-high): the effects test imposes real compliance costs and fine exposure on firms with no EU territorial presence, but the coordination function it serves — closing the offshore-processing loophole — is genuine and not merely pretextual, so extraction is substantial rather than total. Suppression is 0.62: alternatives to compliance (geoblocking EU users, restructuring to avoid targeting characterization) exist but are increasingly foreclosed as enforcement practice matures and the targeting test is applied more expansively by the CJEU and EDPB guidance — the suppression_requirement series shows this hardening over the interval (0.35 to 0.62) as case law and guidance accumulate. Theater ratio is low (0.22) because enforcement activity substantively tracks the stated function (protecting EU residents' data) rather than drifting toward proxy metrics. Accessibility collapse is moderate (0.40): non-EU firms retain the option of exiting the EU market entirely, which is a real if costly alternative, so alternatives have not collapsed as completely as in a true mountain case. Resistance is moderate (0.55): non-EU controllers and their home governments actively contest scope determinations, file objections during EDPB consultations, and some jurisdictions have raised sovereignty objections through trade channels.
 *
 * PERSPECTIVAL GAP:
 *   From the EU data protection authority seat, this is a coordination mechanism closing a jurisdictional gap that would otherwise let controllers evade substantive protection through mere geographic arbitrage — a rope. From the small non-EU business seat with no meaningful EU presence and no realistic capacity to assess targeting exposure, the same structure operates as an externally imposed cost with no corresponding voice in its design — closer to extraction riding on a real coordination story. The engine computing tangled_rope from the beneficiary/victim/enforcement structure captures both readings without needing to pick one as authoritative.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects and EU data protection authorities sit near the beneficiary end: the former receive protection without effort, the latter administer and gain enforcement authority. Non-EU controllers targeting the EU market and small non-EU online businesses sit near the target end: they bear the transfer (compliance cost, fine exposure) with constrained or trapped exit — the small-business tier especially, since geoblocking is a blunt, welfare-reducing exit rather than a genuine choice. EU-based competitors are structural beneficiaries of the leveling effect despite bearing their own compliance costs directly, because the extraterritorial reach removes what would otherwise be a competitive disadvantage relative to unregulated non-EU rivals — this justifies their beneficiary role despite mobile exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (territorial arbitrage defeating substantive data protection) remains live — cross-border data flows and offshore processing capacity have only grown since GDPR's enactment, so the disappearance_verdict of world_rearranges is not a residual claim from an obsolete mandate. This distinguishes the constraint from mandatrophy: the mandate has not outlived its function, though the SCOPE of its application (how expansively 'targeting' and 'monitoring' are read) continues to be actively contested and could itself drift toward overreach if enforcement practice broadens without corresponding proportionality checks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    targeting_test_scope_creep,
    'Does the targeting/monitoring test, as applied by EDPB guidance and CJEU case law, remain bounded to firms that deliberately orient activity toward EU residents, or is it drifting toward capturing any firm whose website is merely accessible from the EU?',
    'Track the ratio of enforcement actions against firms with clear EU-directed marketing/currency/language signals versus firms with only incidental EU accessibility, across successive EDPB guideline revisions and CJEU rulings.',
    'If scope creeps toward mere accessibility, effective extraction rises sharply for the small-business victim tier and the constraint drifts from tangled_rope toward snare; if the test stays bounded to deliberate targeting, the coordination function remains proportionate to the extraction it imposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_test_scope_creep, empirical, 'Whether the targeting/monitoring test is scope-stable or expanding beyond deliberate EU-directed conduct.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the effects-jurisdiction reading the legally correct interpretation of Article 3(2), or is it one contestable reading among the territorial-sovereignty and market-access alternatives, with the statutory text itself underdetermining which applies?',
    'This is inherently a conceptual/legal-interpretive question resolved by evolving CJEU doctrine and comparative international law scholarship, not by empirical measurement alone; track whether CJEU jurisprudence explicitly forecloses the territorial-sovereignty reading or leaves it live as a minority position among member state courts and international law scholars.',
    'If CJEU doctrine forecloses the territorial-sovereignty reading, this reading''s enforcement legitimacy strengthens and non-EU resistance framed in sovereignty terms loses doctrinal traction; if the readings remain genuinely contested, enforcement against non-EU controllers carries persistent legitimacy risk that could affect adequacy negotiations and cross-border enforcement cooperation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the effects-jurisdiction reading has doctrinal priority or remains one of several live contested readings of the same text.').

omega_variable(
    compliance_cost_proportionality,
    'Is the compliance cost actually borne by non-EU controllers proportionate to the privacy harm the effects test prevents, or does it systematically overshoot for the small-business tier relative to actual EU-resident data volume processed?',
    'Comparative empirical study of GDPR compliance cost as a share of revenue for small non-EU firms versus the actual EU-resident data volumes and sensitivity they process, benchmarked against equivalent EU-domestic small-firm compliance costs.',
    'If costs are disproportionate for small non-EU firms relative to actual data-protection risk, the victim classification for small_non_eu_online_businesses strengthens and the constraint''s extraction profile for that tier moves closer to a snare than the aggregate tangled_rope reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_proportionality, empirical, 'Whether compliance burden on small non-EU firms is proportionate to the data-protection risk they actually pose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(gdpr_tr_t16, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(gdpr_tr_t24, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(gdpr_be_t16, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(gdpr_be_t24, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(gdpr_su_t12, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(gdpr_su_t16, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(gdpr_su_t24, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__effects_jurisdiction_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language concept 'GDPR Article 3(2) extraterritorial scope': this story (effects_jurisdiction_reading, tangled_rope, ε=0.58 — jurisdiction follows the data subject via targeting/monitoring), market_access_reading (frames the same text as Brussels Effect conditional market access rather than jurisdictional assertion, lower expected ε), and territorial_sovereignty_reading (frames the same extraterritorial application as exceeding legitimate regulatory authority, likely reads as snare from the non-EU sovereign's seat with high suppression and low legitimacy). Each carries independent ε, beneficiary/victim data, and classification per the ε-invariance principle; they are linked here as siblings under the shared gdpr_article_3_scope kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
