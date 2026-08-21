% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Scope: Territorial Sovereignty Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'territorial sovereignty' reading of GDPR
 *   Article 3's scope, asserting that regulatory authority is fundamentally
 *   bounded by national borders and that extraterritorial application of laws
 *   like GDPR constitutes an overreach. This reading emphasizes the
 *   independence of non-EU states and data processors from direct EU
 *   regulatory control, often leading to data localization as a resistance
 *   mechanism and escalating jurisdictional conflicts. The claimed type is
 *   'tangled_rope' because it coordinates the principle of state sovereignty
 *   while extracting from EU regulators and citizens by limiting the reach of
 *   EU privacy protections.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.65).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.7).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Scope: Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, 'cb1da16a-a990-4e64-93bd-adc7e2b55ad9').
narrative_ontology:cs_kernel_codification('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', formalized).
narrative_ontology:cs_authority_grounding('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', lineage).
narrative_ontology:cs_interpretation_layer_present('cb1da16a-a990-4e64-93bd-adc7e2b55ad9').
narrative_ontology:cs_reading_relation('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', foundational, territorial_sovereignty_is_primary).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', territorial_sovereignty_is_primary, deontological).
narrative_ontology:cs_axiom('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', secondary, extraterritoriality_requires_explicit_consent).
narrative_ontology:cs_axiom_status(extraterritoriality_requires_explicit_consent, holdable).
narrative_ontology:cs_axiom_grounding('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', extraterritoriality_requires_explicit_consent, conventional).
narrative_ontology:cs_reference_frame('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', contemporary_digital_economy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('cb1da16a-a990-4e64-93bd-adc7e2b55ad9', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_processors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_citizens_abroad).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the principle that its regulatory authority over data processing within its borders is not superseded by foreign law. Views GDPR's extraterritorial claims as an overreach that infringes on its sovereignty, leading to data localization efforts and jurisdictional conflict.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states, beneficiary,
    institutional, generational, mobile, national).

% Benefits from resisting the direct application of GDPR to its operations outside the EU, arguing that compliance should be governed by its local jurisdiction. This reading provides a basis for challenging enforcement actions and advocating for data localization.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_processors, beneficiary,
    powerful, biographical, constrained, global).

% Bears the cost of jurisdictional challenges and enforcement difficulties when attempting to apply GDPR extraterritorially. This reading limits their effective reach and forces them to rely on international cooperation or indirect enforcement mechanisms.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators, payer,
    institutional, generational, constrained, regional).

% Experiences a reduction in effective privacy protection when their data is processed by non-EU entities that successfully resist GDPR application based on territorial sovereignty. Their privacy rights become contingent on the local laws of the non-EU state.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_citizens_abroad, payer,
    powerless, biographical, trapped, global).

% Analyze the tension between traditional territorial jurisdiction and modern regulatory challenges posed by digital data flows. This reading aligns with classical interpretations of state sovereignty in international law.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the principle of non-interference in the domestic regulatory affairs of sovereign states, preventing a 'race to the top' in privacy regulation from unilaterally imposing foreign law.
% TRANSFER_FUNCTION: Transfers regulatory authority and control over data processing from the EU to non-EU states for operations occurring outside EU territory, limiting the scope of EU privacy protections.
% ABSENT_VOICES: EU privacy advocates and data subjects who believe privacy is a universal human right would object, arguing that territorial limits undermine effective protection in a globalized digital economy. They are often excluded from the direct inter-state jurisdictional debates.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the default would shift towards broader extraterritorial application of privacy laws, leading to increased compliance burdens for non-EU entities, potential 'Brussels Effect' expansion, and a reduction in the regulatory independence of non-EU states regarding data processing.
% FOUNDING_PROBLEM: The problem of states asserting regulatory authority beyond their borders, leading to conflicts of law and infringements on national sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Non-EU governments and international law bodies consistently corroborate the ongoing relevance of territorial sovereignty in international law, citing numerous diplomatic protests and legal challenges against extraterritorial assertions by various states, not just the EU.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.65) is substantial because this reading effectively limits the scope of EU privacy law, reducing its protective power for EU citizens whose data is processed abroad, and forcing EU regulators to expend significant resources on jurisdictional disputes. Suppression (0.70) is high as it actively resists and challenges the extraterritorial claims of the GDPR, often through legal and diplomatic means, leading to data localization requirements. Resistance (0.80) is also high, reflecting the strong pushback from non-EU states and companies against perceived overreach. The theater ratio (0.20) is low, indicating that the arguments for territorial sovereignty are genuinely held and actively pursued, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of non-EU states, this reading is a 'rope' that upholds a fundamental principle of international law (sovereignty). From the perspective of EU regulators and citizens, it is a 'snare' that undermines privacy protections and creates enforcement gaps. The engine's classification as 'tangled_rope' reflects the hybrid nature: it coordinates state sovereignty but at the cost of asymmetric extraction from those seeking broader privacy protections.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-EU states and data processors are beneficiaries, as this reading protects their regulatory independence and reduces their compliance burden with foreign law. EU regulators and EU citizens abroad are victims, as their ability to enforce or benefit from GDPR's protections is curtailed. International law scholars act as observers, analyzing the structural tensions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_extraterritoriality,
    'Is the extraterritorial application of privacy laws a legitimate evolution of international law in the digital age, or an illegitimate overreach of state power?',
    'Emergence of new international treaties or customary international law explicitly addressing digital jurisdiction, or a landmark ruling by an international court that establishes a new precedent.',
    'If deemed legitimate, this reading''s claims of overreach would be weakened, potentially shifting its classification towards a ''piton'' or ''scaffold'' as its foundational premise erodes. If deemed illegitimate, this reading would gain strength, potentially solidifying its ''mountain'' aspects for non-EU states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_extraterritoriality, conceptual, 'The fundamental conceptual dispute over the boundaries of digital jurisdiction.').

omega_variable(
    effectiveness_of_data_localization,
    'How effective are data localization measures in truly preventing extraterritorial application of foreign law, given the global nature of data flows and cloud computing?',
    'Empirical studies tracking the actual impact of data localization mandates on data access by foreign authorities and the practical enforceability of local laws against global tech companies.',
    'If data localization is found to be largely ineffective, the ''resistance'' metric for non-EU entities would decrease, and the ''suppression'' metric for EU regulators might increase, as their efforts to resist are shown to be performative. This could shift the constraint towards a ''piton'' if the resistance becomes purely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_data_localization, empirical, 'The practical efficacy of data localization as a resistance mechanism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of EU regulatory reach structural (inherent limits of international law) or internalized (EU regulators'' reluctance to push boundaries due to diplomatic concerns)?',
    'Analysis of EU regulatory enforcement patterns across different non-EU jurisdictions, distinguishing between cases where legal challenges are mounted versus cases where enforcement is simply not attempted. Diplomatic correspondence analysis.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — EU regulators carry the suppression with them. If purely structural, the constraint is more ''mountain-like'' in its unchangeability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for EU regulatory reach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gdpr_tr_t5, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(gdpr_tr_t10, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(gdpr_tr_t15, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gdpr_be_t5, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(gdpr_be_t10, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(gdpr_be_t15, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(gdpr_su_t5, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(gdpr_su_t10, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(gdpr_su_t15, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, data_localization_mandates).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the GDPR Article 3 scope kernel. This reading emphasizes traditional territorial sovereignty, influencing data localization mandates and standing in tension with the effects-based and market-access readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
