% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Extraterritorial Scope Assertion (Territorial Sovereignty Reading)
 *   domain: legal/technological/international
 *
 * SUMMARY:
 *   This constraint models GDPR Article 3(2) under the territorial
 *   sovereignty reading: the EU asserts extraterritorial regulatory
 *   jurisdiction over any entity targeting or monitoring EU residents' data,
 *   overriding the home-state jurisdiction of the entity and the jurisdiction
 *   of other states whose residents' data might be in the same systems. The
 *   reading treats this assertion as exceeding legitimate jurisdictional
 *   authority and construes the arrangement as extractive enforcement of EU
 *   regulatory will through market access threat and financial coercion. This
 *   is ONE reading of a contested kernel (gdpr_article_3_scope); sibling
 *   readings — effects_jurisdiction and market_access — construct the same
 *   Article 3(2) differently. The claim (snare) and metrics (0.68
 *   extractiveness, 0.52 suppression) are authored independently: a genuine
 *   snare has victims, active enforcement, and suppressed exit; the metrics
 *   describe a constraint fitting that profile. The claim and metrics are NOT
 *   tuned to each other.
 *
 * KEY AGENTS:
 *   - EU regulatory authority: institutional agenda-setter, enforces Article 3(2) by asserting jurisdiction; benefits from expanded regulatory reach
 *   - Non-EU state regulators: institutional payers, lose regulatory authority over their own residents' data and domestic operators
 *   - Extraterritorial service operators: powerful payers, face dual compliance and legal uncertainty; constrained exit (market dependency on EU)
 *   - Jurisdictional sovereignty claimants: organized payers, sovereignty claims (data localization, national champions) are overridden
 *   - Affected EU residents: powerless beneficiaries, receive protection but have identity_locked exit (cannot opt out of GDPR)
 *   - Competing privacy frameworks: excluded from the legitimacy debate; their own claims to regulate are subordinated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.68).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.52).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, snare).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Extraterritorial Scope Assertion (Territorial Sovereignty Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "legal/technological/international").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '52d3f6a7-95a3-4425-865a-22544252e208').
narrative_ontology:cs_kernel_codification('52d3f6a7-95a3-4425-865a-22544252e208', fixed_text).
narrative_ontology:cs_authority_grounding('52d3f6a7-95a3-4425-865a-22544252e208', extraction).
narrative_ontology:cs_interpretation_layer_present('52d3f6a7-95a3-4425-865a-22544252e208').
narrative_ontology:cs_reading_relation('52d3f6a7-95a3-4425-865a-22544252e208', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('52d3f6a7-95a3-4425-865a-22544252e208', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('52d3f6a7-95a3-4425-865a-22544252e208', foundational, jurisdiction_bounded_by_territory).
narrative_ontology:cs_axiom_status(jurisdiction_bounded_by_territory, holdable).
narrative_ontology:cs_axiom_grounding('52d3f6a7-95a3-4425-865a-22544252e208', jurisdiction_bounded_by_territory, conventional).
narrative_ontology:cs_axiom('52d3f6a7-95a3-4425-865a-22544252e208', secondary, unilateral_extraterritorial_assertion_unlawful).
narrative_ontology:cs_axiom_status(unilateral_extraterritorial_assertion_unlawful, holdable).
narrative_ontology:cs_axiom_grounding('52d3f6a7-95a3-4425-865a-22544252e208', unilateral_extraterritorial_assertion_unlawful, deontological).
narrative_ontology:cs_reference_frame('52d3f6a7-95a3-4425-865a-22544252e208', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('52d3f6a7-95a3-4425-865a-22544252e208', contemporary_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('52d3f6a7-95a3-4425-865a-22544252e208', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulatory_authority).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, extraterritorial_service_operators).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, jurisdictional_sovereignty_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, affected_eu_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces GDPR Article 3(2) by asserting jurisdictional reach over any entity that targets or monitors EU residents' data, regardless of where the entity is incorporated or where processing occurs. Uses enforcement discretion to define 'targeting,' escalates conflicts with other jurisdictions over whose law applies, and treats extraterritorial assertion as legitimate because of EU residents' vulnerability and lack of consent.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Cannot regulate data practices of entities within their own jurisdiction independently; GDPR application forces compliance with foreign law even for domestic operations. Domestic services processing data of EU residents fall under EU enforcement; domestic entities exporting data to EU face dual compliance burdens. Their regulatory authority over their own territory is subordinated to EU determinations of scope.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators, payer,
    institutional, generational, constrained, national).

% Face compliance demands from EU authority for practices that would not trigger legal obligation under the law of the jurisdiction where they operate or are incorporated. They bear the cost of dual compliance (home jurisdiction + GDPR), legal uncertainty about whether their services 'target' EU residents, and the risk that EU enforcement reinterprets scope retroactively. Larger operators absorb this; smaller ones may exit EU markets.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, extraterritorial_service_operators, payer,
    powerful, biographical, constrained, global).

% States asserting their own data-sovereignty doctrines (e.g., data localization, national champions, autonomous infrastructure) are positioned as violating EU law if they restrict EU-regulated entities from accessing their residents' data. Their sovereignty claims are overridden by the reading that EU jurisdiction extends to the protection of 'their' residents anywhere on Earth.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, jurisdictional_sovereignty_claimants, payer,
    organized, generational, trapped, national).

% Receive protection from their data being processed outside EU borders without GDPR consent and governance. However, they have no voice in the jurisdictional dispute; the beneficiary framing ('you are protected') masks that EU regulatory authority asserts control over where their data can go without consulting them or acknowledging alternative regulatory frameworks' legitimacy.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, affected_eu_residents, beneficiary,
    powerless, biographical, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, affected_eu_residents, observer).

% Alternative privacy regulation systems (UK DPA, California CCPA, LGPD, national schemes) cannot establish coexisting or equivalent protection without EU recognition. They are excluded from the debate about whose law defines the legitimate boundary of jurisdiction; GDPR's extraterritorial application crowds out their own territorial claims.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, competing_privacy_frameworks, excluded,
    institutional, generational, trapped, continental).

% Analyze whether GDPR Article 3(2) constitutes a permissible assertion of jurisdiction under international law principles (territorial, effects, nationality, protective, universal). They note tension between EU law and customary international law on jurisdictional boundaries; their analysis feeds into disputes but does not determine outcomes.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulatory_authority).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__territorial_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function. The arrangement asserts unilateral regulatory reach and enforces it through market access pressure and threat of fines.
% TRANSFER_FUNCTION: Transfers regulatory authority from the jurisdiction where an entity operates or is incorporated to the EU, even when the entity's home state has its own privacy law. Also transfers compliance costs to non-EU operators and restricts the jurisdictional autonomy of non-EU states.
% ABSENT_VOICES: Non-EU state regulators are excluded from the question 'whose law legitimately applies to data about EU residents?' Their views on jurisdictional boundaries are overridden. Competing privacy frameworks (UK, California, others) are not parties to the legitimacy question; they are treated as subordinate or harmonizable rather than coordinate.
% DISAPPEARANCE_RATIONALE: If GDPR Article 3's extraterritorial assertion were withdrawn and replaced with territorial-only application, non-EU jurisdictions would immediately exercise independent regulatory authority over their own residents' data, service operators would face one compliance regime per jurisdiction (not EU-override), and other privacy frameworks would regain autonomy to set their own rules without EU override.
% FOUNDING_PROBLEM: EU residents' data was being processed by non-EU entities without any privacy protection or consent mechanism; private companies could export data to jurisdictions with minimal oversight.
% FOUNDING_PROBLEM_CORROBORATION: EU authority and privacy advocates argue the problem is live: non-EU jurisdictions still have weaker privacy law, and EU residents deserve protection regardless of where processing happens. Non-EU regulators and international law scholars argue the founding problem has been solved by competing privacy frameworks and that GDPR's extraterritorial reach now exceeds what was necessary — it asserts jurisdiction as a solution to a narrower problem.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) because the constraint transfers regulatory authority to the EU without compensation or reciprocal jurisdiction-sharing; non-EU regulators must enforce GDPR within their borders on the EU's terms. Suppression is moderate-high (0.52) because resistance is active and sustained (non-EU data localization mandates, national sovereignty doctrines, competing frameworks), but the EU's institutional power and market-access leverage suppress its expression — non-EU states cannot successfully override GDPR without economic consequences. Theater is moderate (0.41): the constraint is genuinely enforced (fines are real, scope is actively interpreted), but the framing as protection-of-residents masks the underlying jurisdictional claim and its asymmetric beneficiary structure. The measurement series shows extractiveness and theater rising 2018–2022 (increased enforcement clarity and scope interpretation) then plateauing 2022–2026 (established enforcement pattern, diminishing marginal assertion returns). Suppression similarly plateaus (the resistance mechanisms — localization, national frameworks — have reached steady-state pushback). One shared time grid throughout.
 *
 * PERSPECTIVAL GAP:
 *   The EU regulatory authority's seat sees this as legitimate protection of vulnerable residents against powerful actors; the non-EU state regulators' seat sees it as sovereignty violation and jurisdictional overreach; operators see it as asymmetric compliance burden imposed by market power. The engine computes these divergences from the structural data: the beneficiary collects jurisdiction and faces no exit cost, deriving a beneficiary directionality; the victims lose jurisdiction and face suppressed exits, deriving target directionality. The claimed type (snare) reflects a reading that the legitimacy story (resident protection) is a cover for institutional jurisdiction-capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The EU regulatory authority sits at the beneficiary end (d near 0.0): it collects regulatory jurisdiction, faces no exit cost, and can enforce unilaterally through market access. Non-EU state regulators sit at the target end (d near 1.0): they lose jurisdiction over their own territory and operators, face constrained exit (cannot simply ignore GDPR without economic consequences), and are identity-locked as sovereigns unable to exit the state identity. Extraterritorial service operators sit intermediate (d ~0.65): they are targets (pay compliance costs, face legal uncertainty), but retain some arbitrage (operate in multiple jurisdictions, can absorb costs). Affected EU residents sit near symmetric or slight-beneficiary (d ~0.3): they benefit from protection, but this is never asked them; their identity_locked exit and powerlessness mean the benefit is imposed, not chosen.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unprotected EU residents' data in non-EU systems) was real in 2018 but is contested by 2026. Competing privacy frameworks (CCPA, UK DPA, LGPD) now provide meaningful protection outside the EU; the problem is not dead but substantially solved by alternative means. Yet GDPR Article 3's extraterritorial scope has continued to expand through enforcement interpretation and scope rulings, decoupling the enforcement machinery from the founding problem it was meant to solve. This is a mandatrophy trajectory: the problem that justified the constraint has been displaced by new alternative mechanisms, but the constraint's enforcement apparatus persists and expands, now driven by jurisdictional institutional interests rather than resident protection. The constraint should be reclassified based on this divergence; the snare classification captures this — the coordination story (protecting residents) has become cover for institutional extraction (regulatory jurisdiction capture).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    targeting_definition_scope_creep,
    'What counts as ''targeting'' EU residents under Article 3(2)? Does offering services in multiple languages, having a country selector, or merely being accessible from EU IP addresses constitute targeting?',
    'CJEU rulings defining targeting narrowly vs. broadly; empirical observation of enforcement discretion over time.',
    'If targeting is narrowly defined (intentional-targeting test), the jurisdictional reach is bounded and the constraint is less extractive. If broad (accessibility + capacity to infer EU residence), the reach is expansive and extraction is high. This definition is entirely within EU enforcement discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_definition_scope_creep, conceptual, 'Whether Article 3(2) scope is determined by principle or by enforcement discretion.').

omega_variable(
    alternative_framework_equivalence,
    'Are competing privacy frameworks (CCPA, UK DPA, LGPD, etc.) now sufficiently protective that GDPR''s extraterritorial reach is no longer justified by the need to protect EU residents?',
    'Systematic empirical comparison of privacy outcomes under GDPR vs. competing frameworks; independent privacy audits; evidence of whether residents in non-EU jurisdictions under competing regimes experience equivalent protections.',
    'If equivalence is established, the founding problem is solved and the constraint is pure extraction, not coordination. This would support mandatrophy reclassification to piton (atrophied coordination, now inertial).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_equivalence, empirical, 'Whether the founding problem remains live or has been solved by alternative means.').

omega_variable(
    jurisdictional_legitimacy_benchmark,
    'What principle legitimates extraterritorial jurisdiction? Is it effects-on-nationals, market access, territorial control, or something else?',
    'International law scholarship consensus; state practice in competing jurisdictional claims; CJEU reasoning about why extraterritorial reach is permissible.',
    'Different benchmarks yield different ε values: if legitimacy rests on effects (protecting harmed residents), ε is lower; if resting on institutional assertion of control, ε is higher. This reading assumes territorial principle is the relevant benchmark, making extraterritoriality unjustified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jurisdictional_legitimacy_benchmark, preference, 'Which principle of jurisdictional legitimacy is binding — and whether the reading''s choice is justified.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.52) structural (non-EU states cannot exit without economic harm; market access is conditional) or internalized (non-EU regulators internalize EU authority as legitimate; sovereignty is a fading norm)?',
    'Longitudinal observation of non-EU state resistance: if resistance is sustained and escalating (localization mandates, national champions, competing frameworks), suppression is structural; if resistance is declining and states internalize GDPR as binding, suppression is internalized.',
    'If internalized, the effective suppression is higher than the structural measure suggests and the constraint is more binding. If structural, removal of market-access threat would release substantial counter-movement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether non-EU state suppression is structural or internalized.').

omega_variable(
    reading_distinguishing_axiom_territorial_principle,
    'This reading asserts territorial principle as the legitimate boundary of jurisdiction. Do the sibling readings reject this principle or simply give it lower weight?',
    'Reconstruction of each reading''s core premises and axioms. If the effects_reading and market_reading both accept territorial principle but override it with other considerations (effects on residents, market conditions), they coexist. If they reject the principle entirely, they foreclose this reading.',
    'If coexistence, the three readings represent different frameworks, each internally consistent; if foreclosure, this reading''s premise is untenable. This determines the reading_relations field in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_distinguishing_axiom_territorial_principle, conceptual, 'Whether territorial principle is a shared premise that sibling readings weight differently, or a rejected premise that forecloses this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement_basis(gdpr_tr_t2018, observed).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2020, 0.33).
narrative_ontology:measurement_basis(gdpr_tr_t2020, observed).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement_basis(gdpr_tr_t2022, observed).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(gdpr_tr_t2024, observed).
narrative_ontology:measurement(gdpr_tr_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(gdpr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2018, 0.52).
narrative_ontology:measurement_basis(gdpr_be_t2018, observed).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(gdpr_be_t2020, observed).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement_basis(gdpr_be_t2022, observed).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(gdpr_be_t2024, observed).
narrative_ontology:measurement(gdpr_be_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(gdpr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement_basis(gdpr_su_t2018, observed).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2020, 0.47).
narrative_ontology:measurement_basis(gdpr_su_t2020, observed).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2022, 0.5).
narrative_ontology:measurement_basis(gdpr_su_t2022, observed).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(gdpr_su_t2024, observed).
narrative_ontology:measurement(gdpr_su_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2026, 0.52).
narrative_ontology:measurement_basis(gdpr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__territorial_sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% GDPR Article 3 scope is a contested kernel admitting at least three structurally distinct readings, each with different ε and beneficiary/victim structures. This story instantiates the territorial_sovereignty_reading; sibling stories (effects_jurisdiction_reading, market_access_reading) instantiate competing readings of the same text. The three stories share a kernel (GDPR Article 3(2)) but decompose into separate constraints because the readings assign different legitimacy bases and different structural beneficiaries. Network links in both directions: each reading influences the others' boundary conditions and institutional plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__territorial_sovereignty_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
