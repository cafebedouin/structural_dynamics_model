% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3 Scope â Market Access Reading
 *   domain: technology governance / international law / privacy regulation
 *
 * SUMMARY:
 *   This constraint instantiates the market_access_reading of the contested
 *   kernel gdpr_article_3_scope. Rather than asserting extraterritorial
 *   jurisdiction over foreign actors, the EU conditions access to its
 *   internal digital market on compliance with GDPR. The resulting Brussels
 *   Effect induces global firms to adopt EU standards by default. The
 *   constraint therefore coordinates a transnational privacy baseline while
 *   asymmetrically extracting compliance costs from non-EU service providers
 *   and concentrating regulatory influence in the EU institutional sphere.
 *   Sibling readings frame the same text as effects-based jurisdiction or as
 *   territorial overreach.
 *
 * KEY AGENTS:
 *   - eu_regulatory_authorities: agenda_setter (institutional/arbitrage) â sets standard and gains global influence
 *   - non_eu_digital_service_providers: primary payer (powerful/constrained) â bears compliance costs for market retention
 *   - eu_based_digital_firms: beneficiary (moderate/constrained) â gains competitive moat from foreign compliance barriers
 *   - eu_data_subjects: beneficiary (powerless/constrained) â receives privacy protection via market leverage
 *   - third_country_regulators: excluded (institutional/constrained) â sovereignty concerns absent from EU process
 *   - privacy_ngos: observer (organized/analytical) â monitors outcomes without gatekeeping role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.58).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.42).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Scope â Market Access Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology governance / international law / privacy regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, '6eb10794-19e3-4089-a00d-442180976cba').
narrative_ontology:cs_kernel_codification('6eb10794-19e3-4089-a00d-442180976cba', formalized).
narrative_ontology:cs_authority_grounding('6eb10794-19e3-4089-a00d-442180976cba', lineage).
narrative_ontology:cs_interpretation_layer_present('6eb10794-19e3-4089-a00d-442180976cba').
narrative_ontology:cs_reading_relation('6eb10794-19e3-4089-a00d-442180976cba', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6eb10794-19e3-4089-a00d-442180976cba', gdpr_article_3_scope__territorial_sovereignty_reading, influences).
narrative_ontology:cs_axiom('6eb10794-19e3-4089-a00d-442180976cba', foundational, market_access_not_jurisdiction).
narrative_ontology:cs_axiom_status(market_access_not_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('6eb10794-19e3-4089-a00d-442180976cba', market_access_not_jurisdiction, conventional).
narrative_ontology:cs_axiom('6eb10794-19e3-4089-a00d-442180976cba', secondary, brussels_effect_legitimate_governance).
narrative_ontology:cs_axiom_status(brussels_effect_legitimate_governance, holdable).
narrative_ontology:cs_axiom_grounding('6eb10794-19e3-4089-a00d-442180976cba', brussels_effect_legitimate_governance, instrumental).
narrative_ontology:cs_reference_frame('6eb10794-19e3-4089-a00d-442180976cba', eu_internal_market_integrity).
narrative_ontology:cs_drift_state('6eb10794-19e3-4089-a00d-442180976cba', post_brussels_effect_recognition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6eb10794-19e3-4089-a00d-442180976cba', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_regulatory_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_based_digital_firms).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_digital_service_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the data protection compliance conditions for accessing the EU digital single market through GDPR Article 3(2); enforces via national DPAs and the EDPB; gains expanded global regulatory influence as firms adopt EU standards by default to maintain market access.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_regulatory_authorities, agenda_setter,
    institutional, generational, arbitrage, universal).

% Bear compliance costs, legal uncertainty, and operational redesign to meet GDPR requirements as a condition of serving EU users; frequently extend EU-compliant practices globally because the EU market is too large to abandon; cannot easily exit without major revenue loss.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_digital_service_providers, payer,
    powerful, biographical, constrained, global).

% Already compliant with domestic data protection frameworks; benefit from a competitive moat when non-EU rivals face high barriers to EU market entry and from regulatory harmonization that reduces intra-EU fragmentation.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_based_digital_firms, beneficiary,
    moderate, biographical, constrained, regional).

% Receive data protection benefits from the EU market access condition which pulls global service providers into compliance; do not choose the constraint but are the stated beneficiaries of its extraterritorial reach.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_data_subjects, beneficiary,
    powerless, biographical, constrained, regional).

% Oversee data protection and digital trade policy in their own jurisdictions; find their domestic firms subject to EU rulemaking without their participation in the standard-setting process; sovereignty concerns are not addressed in EU enforcement proceedings.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, third_country_regulators, excluded,
    institutional, generational, constrained, national).

% Monitor GDPR enforcement and the Brussels Effect globally; provide external corroboration of privacy outcomes and compliance patterns without participating in the market-access gatekeeping function.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, privacy_ngos, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, high-standard data protection baseline for firms seeking access to the EU digital single market, reducing regulatory fragmentation for global operators that adopt GDPR as a unified compliance target.
% TRANSFER_FUNCTION: Moves compliance costs and regulatory standard-setting power from non-EU digital service providers to the EU institutional sphere, while transferring privacy protections and competitive advantages to EU data subjects and EU-based firms.
% ABSENT_VOICES: Third-country regulators and trade authorities who view the extraterritorial reach as sovereignty-infringing are structurally excluded from the EU enforcement conversation; small non-EU firms that silently exit the EU market rather than challenge the rule are absent from enforcement data.
% DISAPPEARANCE_RATIONALE: If the conditional market access requirement vanished, non-EU firms would segment their services by jurisdiction, EU consumers would lose leverage-derived privacy protections, and the Brussels Effect would attenuate as global compliance pull disappeared â the transnational data governance landscape would fragment.
% FOUNDING_PROBLEM: Fragmented national data protection laws across Europe created compliance complexity and weak enforcement, while the rise of borderless digital services enabled data exploitation outside any effective regulatory framework.
% FOUNDING_PROBLEM_CORROBORATION: EU privacy scholars and consumer advocacy organizations attest the problem remains live. US trade representatives and international law scholars outside the EU beneficiary set attest that the extraterritorial solution exceeds the founding problem's territorial scope; the European Commission defends the current scope as necessary for effectiveness.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.58) reflects real compliance costs imposed on non-EU firms and the capture of regulatory influence by EU institutions. Suppression (0.42) is moderate: enforcement exists and fines are substantial, but the market_access_reading frames compliance as strategic market entry rather than raw coercion, lowering the measured suppression relative to a pure jurisdictional assertion. Theater (0.38) captures the performative dimension of Brussels Effect rhetoric while acknowledging genuine privacy coordination. Accessibility collapse (0.72) is high because once a firm commits to the EU market, non-compliant operational alternatives effectively disappear. Resistance (0.48) reflects ongoing US and third-country contestation without the acute sovereignty clash of a jurisdictional-overreach framing.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as legitimate single-market governance and successful regulatory export; the non-EU payer seat experiences it as a non-negotiable cost of market participation. EU-based firms and data subjects occupy a subsidized position, receiving coordination benefits without bearing the extraterritorial compliance burden. The engine computes this divergence from the structural asymmetry in exit options (arbitrage for the agenda setter versus constrained for payers) and the declared beneficiary/victim structure.
 *
 * DIRECTIONALITY LOGIC:
 *   EU regulatory authorities are declared beneficiaries and agenda setters with arbitrage-grade exit (they shape the global standard rather than escaping it), placing them near the full-beneficiary pole (d â 0.1). Non-EU digital service providers are declared victims with constrained exit and high power, placing them near the full-target pole (d â 0.85). EU-based firms and EU data subjects are beneficiaries with constrained exit, sitting in the low-beneficiary midrange (d â 0.2â0.3) because they gain from the constraint without controlling it. No directionality overrides are needed: the structural derivation captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfragmented data protection and weak enforcement in the digital single marketâremains sufficiently live to prevent piton classification. However, the constraint's functional expansion beyond EU territory through the Brussels Effect risks mandatrophy if the coordination rationale (protecting EU residents) becomes entirely decoupled from the operational reality (global standard-setting via market leverage). The tangled_rope classification captures the hybridity: genuine coordination in privacy protection is structurally fused with asymmetric extraction of compliance costs and regulatory influence. A snare classification would require the coordination story to be mere cover, which is not supported here because EU data subjects verifiably benefit and the privacy standards have independent normative force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_access_vs_jurisdiction,
    'Does Article 3(2) function in practice as a market access condition, or as an assertion of effects-based jurisdiction?',
    'Comparative analysis of CJEU jurisprudence and enforcement patterns: if enforcement tracks market presence more closely than harmful effects on EU residents, the market_access reading is descriptively validated.',
    'If validated as market access, the constraint''s suppression and resistance scores should be lower than under a jurisdictional reading; if validated as effects jurisdiction, the current classification understates the coercive element.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_access_vs_jurisdiction, conceptual, 'Indeterminacy between market access and jurisdictional framings of Article 3(2).').

omega_variable(
    brussels_effect_extraction,
    'Is the diffusion of EU data protection standards via market leverage a form of regulatory extraction from third countries?',
    'Economic measurement of compliance costs borne by non-EU firms versus benefits captured by EU regulatory influence and domestic firms.',
    'If the cost asymmetry is large, the constraint is more extractive than the market_access framing suggests; if symmetric, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brussels_effect_extraction, empirical, 'Whether Brussels Effect standard diffusion constitutes asymmetric extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_ma_tr_t0, gdpr_article_3_scope__market_access_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gdpr_ma_tr_t2, gdpr_article_3_scope__market_access_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(gdpr_ma_tr_t4, gdpr_article_3_scope__market_access_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(gdpr_ma_tr_t6, gdpr_article_3_scope__market_access_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(gdpr_ma_tr_t8, gdpr_article_3_scope__market_access_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement(gdpr_ma_tr_t10, gdpr_article_3_scope__market_access_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(gdpr_ma_be_t0, gdpr_article_3_scope__market_access_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gdpr_ma_be_t2, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(gdpr_ma_be_t4, gdpr_article_3_scope__market_access_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(gdpr_ma_be_t6, gdpr_article_3_scope__market_access_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(gdpr_ma_be_t8, gdpr_article_3_scope__market_access_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(gdpr_ma_be_t10, gdpr_article_3_scope__market_access_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_ma_su_t0, gdpr_article_3_scope__market_access_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gdpr_ma_su_t2, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2, 0.4).
narrative_ontology:measurement(gdpr_ma_su_t4, gdpr_article_3_scope__market_access_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(gdpr_ma_su_t6, gdpr_article_3_scope__market_access_reading, suppression_requirement, 6, 0.41).
narrative_ontology:measurement(gdpr_ma_su_t8, gdpr_article_3_scope__market_access_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(gdpr_ma_su_t10, gdpr_article_3_scope__market_access_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, global_infrastructure).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The label 'GDPR extraterritoriality' conflates three structurally distinct readings of Article 3: effects-based jurisdiction, market-access conditionality, and territorial sovereignty violation. Each reading carries a distinct epsilon, beneficiary structure, and classification. They are linked as a constraint family under the gdpr_article_3_scope kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
