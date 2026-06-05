% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Effects-Based Jurisdiction Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   Article 3(2) of the GDPR extends the regulation's scope beyond the
 *   territorial boundaries of the EU through an effects-based test: the
 *   regulation applies to any organization processing the personal data of EU
 *   residents, regardless of where the organization is established or where
 *   processing occurs. The operative clause declares that GDPR applies to
 *   'the processing of personal data of data subjects who are in the Union by
 *   a controller or processor not established in the Union, where the
 *   processing activities are related to... (a) the offering of goods or
 *   services, irrespective of whether a payment of the data subject is
 *   required, to such data subjects in the Union; or (b) the monitoring of
 *   their behavior.' This creates a direct extraterritorial jurisdictional
 *   claim that operates through effects on EU residents rather than through
 *   territorial control or corporate establishment. The effects-based
 *   jurisdiction reading interprets this clause as the legitimate scope
 *   boundary for GDPR: if your processing targets, monitors, or affects EU
 *   residents, the regulation applies regardless of your location. This
 *   reading competes with at least two sibling interpretations: (1) the
 *   territorial sovereignty reading, which argues that jurisdiction should
 *   follow territorial boundaries and that extraterritorial application
 *   violates customary international law; (2) the market access reading,
 *   which grounds GDPR jurisdiction in the EU's power to control access to
 *   its market rather than in direct extraterritorial regulatory authority.
 *   The effects-based reading has won dominance in EU jurisprudence and
 *   institutional practice, but the sibling readings remain live in academic
 *   debate and non-EU regulatory frameworks.
 *
 * KEY AGENTS:
 *   - EU Data Subjects: Beneficiary (powerful/mobile in-aggregate) — protected from data extraction across borders; benefit from coordinated privacy standard
 *   - EU Regulatory Authority (EDPB, national DPAs): Primary beneficiary (institutional/arbitrage) — extends regulatory reach and authority globally without requiring territorial presence
 *   - Non-EU Digital Service Providers: Primary victim (powerless/trapped) — cannot exit jurisdiction without abandoning EU market; face mandatory compliance costs and fine risk
 *   - International Commerce Coalition: Secondary victim (organized/constrained) — faces coordination burden (positive: unified standard) and extraction burden (negative: asymmetric enforcement on non-EU actors)
 *   - EU-Based Tech Giants: Strategic beneficiary (powerful/mobile) — existing GDPR compliance becomes competitive moat; rivals face higher friction
 *   - Analytical Observer: Civilian context (analytical/analytical) — observes potential naturalization of regulatory power asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.62).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.68).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Effects-Based Jurisdiction Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '12191e22-6c10-487f-8616-5bf65ae88e46').
narrative_ontology:cs_kernel_codification('12191e22-6c10-487f-8616-5bf65ae88e46', formalized).
narrative_ontology:cs_authority_grounding('12191e22-6c10-487f-8616-5bf65ae88e46', extraction).
narrative_ontology:cs_interpretation_layer_present('12191e22-6c10-487f-8616-5bf65ae88e46').
narrative_ontology:cs_reading_relation('12191e22-6c10-487f-8616-5bf65ae88e46', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('12191e22-6c10-487f-8616-5bf65ae88e46', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('12191e22-6c10-487f-8616-5bf65ae88e46', foundational, effects_principle_jurisdiction).
narrative_ontology:cs_axiom_status(effects_principle_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('12191e22-6c10-487f-8616-5bf65ae88e46', effects_principle_jurisdiction, deontological).
narrative_ontology:cs_axiom('12191e22-6c10-487f-8616-5bf65ae88e46', secondary, targeting_monitoring_operational_boundary).
narrative_ontology:cs_axiom_status(targeting_monitoring_operational_boundary, holdable).
narrative_ontology:cs_axiom_grounding('12191e22-6c10-487f-8616-5bf65ae88e46', targeting_monitoring_operational_boundary, empirically_contingent).
narrative_ontology:cs_reference_frame('12191e22-6c10-487f-8616-5bf65ae88e46', eu_regulatory_authority_extraterritorial_jurisdiction).
narrative_ontology:cs_drift_state('12191e22-6c10-487f-8616-5bf65ae88e46', contemporary_post_cjeu_schrems_ii_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('12191e22-6c10-487f-8616-5bf65ae88e46', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_regulatory_authority).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_digital_service_providers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, international_commerce_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-EU TECH SERVICE PROVIDER (SNARE) — Cannot exit extraterritorial jurisdiction without abandoning EU market access. Trapped by the targeting/monitoring test: any processing of EU residents' data triggers GDPR compliance regardless of server location or corporate residence. Suppression is structural: compliance costs are mandatory (security measures, DPA appointment, breach notification, subject rights fulfillment) with no meaningful exemption pathway. Extraction flows from regulatory authority to service provider in the form of operational burdens and fine risk.
constraint_indexing:constraint_classification(gdpr_article_3_scope__effects_jurisdiction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL COMMERCE COALITION (TANGLED ROPE) — Organized actors (non-EU tech companies, global supply chains, data-intensive service providers) experience both coordination and extraction. The effects-based jurisdiction rule coordinates a unified privacy protection standard across markets (genuine coordination function — companies can invest in one compliance system rather than jurisdictional patchwork). But the rule also extracts via: (a) asymmetric enforcement burden falling on non-EU providers while EU companies can claim territorial exemption more easily, (b) compliance costs layered atop market access, and (c) structural asymmetry in who sets standards (EU unilaterally, others must adapt). Constrained exit: leaving EU market is costly but possible; complying is also costly.
constraint_indexing:constraint_classification(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EU REGULATORY AUTHORITY (ROPE) — Net beneficiary of the effects-based jurisdiction reading. Experiences the constraint as pure coordination: extending jurisdiction via targeting/monitoring test enables coordinated privacy protection for the largest bloc of digitally-active residents and establishes EU regulatory authority over global data flows affecting EU markets. Arbitrage exit available (can choose to enforce or not in each case). Low experienced extraction — the regulatory machinery is designed to benefit this actor.
constraint_indexing:constraint_classification(gdpr_article_3_scope__effects_jurisdiction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EU-BASED TECH GIANTS (ROPE) — Established EU companies with existing compliance infrastructure experience the constraint as coordination with selective benefit. They have built GDPR compliance systems; extending those systems to global operations is operationally incremental. Mobile exit exists (they could relocate HQ, though costly). But more importantly, they benefit from competitor suppression: non-EU competitors face disproportionate compliance friction. Net: coordination that disadvantages rivals more than it constrains them.
constraint_indexing:constraint_classification(gdpr_article_3_scope__effects_jurisdiction_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EU DATA SUBJECTS (TANGLED ROPE) — Genuine coordination function: the effects-based jurisdiction ensures their personal data receives protection regardless of where processing occurs. This is a real coordination benefit — collective action to protect privacy across borders. But also embedded asymmetry: data subjects cannot exit the regime (trapped by geography), and the protection extends only outbound (their data gets protected when accessed by non-EU companies, but their access to non-EU services is not reciprocally constrained). Generational horizon captures the long-term entrenchment of this coordination structure in EU governance identity.
constraint_indexing:constraint_classification(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / REGULATORY INEVITABILITY VIEW (MOUNTAIN) — From a universal/civilizational perspective, this reading risks naturalization as inevitable regulatory necessity. The logic: digital markets operate globally; personal data flows across borders; absent regulatory coordination, privacy protection fragments; therefore extraterritorial jurisdiction is inherent to protecting privacy in a digital world. This perspective sees the effects-based jurisdiction as a natural law of digital governance. However, the structural data contradicts this: the beneficiary/victim structure, the asymmetric enforcement burden, and the contingent choice to use 'targeting/monitoring' as the jurisdictional hook all reveal the reading as a constructed regulatory framework, not a natural limit.
constraint_indexing:constraint_classification(gdpr_article_3_scope__effects_jurisdiction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gdpr_article_3_scope__effects_jurisdiction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gdpr_article_3_scope__effects_jurisdiction_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The effects-based jurisdiction rule imposes mandatory compliance costs on non-EU providers (technical safeguards, data protection officer appointment, breach notification, subject rights fulfillment, accountability mechanisms) and creates ongoing fine risk (up to 4% of global turnover for severe violations). These are direct extraction flows from non-EU providers to the EU regulatory system. The extractiveness is not maximum (0.80+) because: (a) some relief mechanisms exist (adequacy determinations, standard contractual clauses), (b) organized non-EU providers can achieve compliance economies of scale, (c) the rule coordinates a unified standard rather than forcing navigation of multiple jurisdictional regimes. Theater ratio (0.54): Moderate. The effects-based jurisdiction test itself contains ambiguity that generates theater. The 'targeting/monitoring' threshold is operationally vague — does algorithmic personalization count as 'targeting'? Does passive collection of IP geolocation count as 'monitoring'? This ambiguity creates extensive regulatory and legal theater around definitional disputes, safe harbor arguments, and adequacy determination procedures. However, the core GDPR compliance regime (technical safeguards, subject rights, accountability) is substantially functional — much theater is in the boundary-drawing, not in the core mechanism. Suppression (0.68): High. Non-EU providers face substantial structural barriers to exit: (a) market access suppression (leaving the EU market is costly for most large digital services), (b) technical suppression (GDPR compliance infrastructure becomes mandatory investment), (c) regulatory suppression (no meaningful exemption for minor controllers or limited processing). Suppression is higher than extractiveness because the barriers themselves (not just the extraction flowing through them) are the binding mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The effects-based jurisdiction reading produces sharp perspectival gaps across power levels. Non-EU powerless providers see a snare: mandatory jurisdiction with no exit, high suppression, pure extraction. International commerce coalitions see tangled rope: genuine coordination (unified standard) but asymmetric extraction (burden falls on non-EU actors). EU regulatory institutions see rope: coordination benefit with arbitrage exit (can enforce selectively). EU data subjects see tangled rope: coordination benefit (protection) with generational entrenchment (they cannot exit, but benefit from protection, so it's mixed). The analytical observer at civilizational scale risks seeing a mountain — the 'necessity' of effects-based jurisdiction for protecting privacy in global digital markets — but the structural data reveals this as a false summit: the effects test is a contingent jurisdictional choice, not a natural law, and it embeds power asymmetry that benefits the rule-maker (EU) more than the rule-subject (non-EU providers).
 *
 * DIRECTIONALITY LOGIC:
 *   The effects-based jurisdiction reading operates through an asymmetric power structure that directly determines directionality. Non-EU providers are identified as primary victims because they bear the compliance costs and fine risk without controlling the standard-setting process. They are trapped by the scope rule — they cannot opt out of GDPR by staying outside EU territory when their processing affects EU residents. The EU regulatory authority benefits from the coordination (unified privacy standard) and from the asymmetric enforcement capacity (can impose fines globally without reciprocal claims on EU companies). The direction of extraction is from non-EU providers (high d, high f(d)) toward EU regulatory authority (low d, low f(d)). EU data subjects benefit from the coordination but do not experience extraction because they are protected, not charged. The theater ratio reflects the operational ambiguity of the targeting/monitoring test, which generates regulatory and legal theater around boundary cases without substantial functional cost to the protection mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The effects-based jurisdiction reading resolves mandatrophy by showing that the constraint genuinely contains both coordination and extraction. The coordination function is real: a unified privacy standard across the largest bloc of digitally-active residents reduces transaction costs and enables innovation within a predictable compliance environment. The extraction is also real: non-EU providers bear disproportionate compliance costs and enforcement risk because they cannot control the rule-setting process. The snare perspective (non-EU providers) reflects genuine structural extraction; the rope perspective (EU regulatory authority) reflects genuine coordination benefit; the tangled rope perspectives (commerce coalitions, data subjects) reflect both. The constraint is not misclassified as rope when it should be snare — it is legitimately both, depending on structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effects_test_definitional_ambiguity,
    'What empirical threshold separates ''targeting/monitoring'' (triggering jurisdiction) from passive exposure (not triggering jurisdiction)?',
    'Case law evolution: analysis of CJEU decisions and national court rulings clarifying when incidental processing of EU resident data versus deliberate targeting distinction holds. Assessment of whether algorithmic targeting of regions counts as ''monitoring''.',
    'If threshold is strict (deliberate intent required): many non-EU providers escape jurisdiction via ''unintended exposure'' argument, reducing extraction experienced. If threshold is permissive (any processing of EU residents triggers jurisdiction): jurisdiction becomes quasi-universal for all data-intensive services, increasing suppression and extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effects_test_definitional_ambiguity, empirical, 'Empirical threshold for ''targeting/monitoring'' test in Article 3(2)').

omega_variable(
    adequacy_mechanism_effectiveness,
    'Do adequacy mechanisms (data transfer agreements, standard contractual clauses, binding corporate rules) genuinely reduce compliance burden for non-EU providers, or do they add bureaucratic overhead without substantive relief?',
    'Cost-benefit analysis: comparison of compliance expenses under adequacy mechanisms vs full GDPR compliance for non-EU controllers; measurement of time-to-adequacy determination and frequency of adequacy revocation; survey of non-EU provider perceptions of adequacy as relief vs additional regulatory navigation.',
    'If effective relief: suppression and extraction values lower; the constraint becomes more rope-like (genuine coordination with negotiated relief). If ineffective: suppression higher; adequacy becomes theatrical gateway, making suppression_requirement measurements rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_mechanism_effectiveness, empirical, 'Whether adequacy mechanisms provide meaningful compliance relief').

omega_variable(
    reading_contingency_on_territorial_interpretation,
    'Does the effects-based jurisdiction reading logically foreclose the territorial sovereignty reading (regulation applies only to establishments in EU territory), or can both readings coexist in different jurisdictional frameworks?',
    'Legal doctrine analysis: examination of whether accepting effects-based jurisdiction requires rejecting territorial control as a legitimate boundary, or whether both can be valid under different legitimacy frameworks (Brussels I-bis recast principles, WTO non-discrimination, GDPR as supranational law vs customary international law on jurisdiction).',
    'If foreclosure: the two readings are mutually exclusive; adoption of this reading structurally prevents the sibling reading. If coexistence: both readings remain live in different jurisdictional contexts or legal traditions. Affects cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contingency_on_territorial_interpretation, conceptual, 'Whether effects-based reading forecloses territorial sovereignty reading').

omega_variable(
    market_access_reading_enforcement_coupling,
    'Is the effects-based jurisdiction reading dependent on market access as enforcement mechanism (threat of EU market exclusion for non-compliance), or is it enforced through direct fines and jurisdiction independent of market access leverage?',
    'Empirical study of GDPR enforcement: analysis of penalty structures (fines vs market bans), frequency of actual market access denial vs fine imposition, effectiveness of adequacy threats in driving compliance, role of market access in behavioral compliance vs formal legal obligation.',
    'If market access is primary enforcement mechanism: the effects reading influences and depends on the market access reading; they are structurally coupled. If fines and direct jurisdiction are primary: the readings can operate independently. Affects network.affects_constraints and dual_formulation_note.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_access_reading_enforcement_coupling, empirical, 'Enforcement mechanism for effects-based jurisdiction: market access vs direct authority').

omega_variable(
    extraterritorial_enforcement_capacity,
    'What is the actual enforcement capacity of the EU regulatory authority over non-EU controllers outside EU territory?',
    'Analysis of enforcement actions: percentage of GDPR violations by non-EU providers that result in fines vs warnings vs technical compliance only; measurement of collection rates on fines imposed on non-EU entities; assessment of Europol/INTERPOL cooperation mechanisms for cross-border enforcement.',
    'If enforcement capacity is high: suppression and extraction values are accurately assessed. If enforcement capacity is low: measured suppression overstates the actual binding force of the jurisdiction; constraint is more aspirational than structural, potentially reclassifying toward scaffold (temporary enforcement mechanism) or piton (theatrical jurisdiction without teeth).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraterritorial_enforcement_capacity, empirical, 'Enforcement capacity of EU authority over non-EU controllers').

omega_variable(
    effects_reading_naturalizes_regulatory_asymmetry,
    'Is the effects-based jurisdiction reading naturalizing a contingent power asymmetry (large regulatory bloc imposing standards unilaterally) as inherent to digital governance, thereby obscuring the constructed nature of this particular boundary?',
    'Comparative regulatory analysis: examination of whether jurisdictional claims based on effects (rather than territory or nationality) are unique to GDPR or reflect broader patterns; assessment of whether non-EU jurisdictions (China, India, Russia) could claim equivalent effects-based jurisdiction over EU residents, and whether EU accepts such claims; analysis of whether ''effects'' is a natural threshold or a contingent choice embedding particular values.',
    'If naturalized: the mountain perspective misidentifies a contingent construction as inevitable law. The false summit detector should flag this. If contingent: the effects reading is a legitimate political choice, not a natural law, affecting how the mountain perspective should be weighted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effects_reading_naturalizes_regulatory_asymmetry, conceptual, 'Whether effects-based jurisdiction naturalizes regulatory power asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_a3_eff_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gdpr_a3_eff_tr_t2, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(gdpr_a3_eff_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.54).

% Extraction over time
narrative_ontology:measurement(gdpr_a3_eff_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(gdpr_a3_eff_be_t2, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(gdpr_a3_eff_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_a3_eff_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(gdpr_a3_eff_su_t2, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2, 0.63).
narrative_ontology:measurement(gdpr_a3_eff_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, transatlantic_data_transfer_adequacy).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, standard_contractual_clauses_enforcement).

% DUAL FORMULATION NOTE:
% The GDPR Article 3 scope question decomposes into three structurally distinct constraint stories, each representing a different reading of the authorization kernel. The effects-based reading (this story) has ε=0.62 (tangled rope with coordination and extraction); the territorial sovereignty reading has lower ε (treats extraterritoriality as overreach); the market access reading has different mechanism (power-based rather than effects-based). These are not variations of one constraint — they are fundamentally different interpretations of what the regulation authorizes. Network links show how the effects-based reading influences downstream constraints (adequacy determinations, data transfer mechanisms) and competes with sibling readings at the kernel level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__effects_jurisdiction_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
