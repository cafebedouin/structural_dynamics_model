% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-20
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: GDPR Article 3 Scope â Market Access Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the market_access_reading of the
 *   gdpr_article_3_scope kernel. Under this reading, GDPR Article 3(2) is not
 *   primarily an assertion of extraterritorial jurisdiction over foreign
 *   territory but a condition of market access: non-EU controllers and
 *   processors may reach EU residents only if they adhere to EU
 *   data-protection standards. The mechanism produces the Brussels Effect,
 *   diffusing EU regulatory norms globally. The EU regulatory apparatus
 *   benefits from standard-setting influence; non-EU firms bear compliance
 *   costs and strategic constraints; EU data subjects gain extended
 *   protection. Sibling readings frame the same legal text as effects-based
 *   jurisdictional extension or as territorial overreach.
 *
 * KEY AGENTS:
 *   - eu_regulatory_apparatus (agenda_setter/beneficiary, institutional/analytical) â sets the compliance condition and captures regulatory influence
 *   - major_non_eu_platforms (payer, powerful/constrained) â absorb high compliance costs to retain EU market access
 *   - small_non_eu_service_providers (payer, moderate/constrained) â face disproportionate barriers, often exit or rely on intermediaries
 *   - eu_data_subjects (beneficiary, organized/constrained) â receive extended privacy protection without administering the constraint
 *   - third_country_governments (excluded, institutional/analytical) â contest extraterritoriality but are outside the EU legislative process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.58).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.42).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Scope â Market Access Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, '2007cbd7-165c-4799-96c4-6473f0763b6b').
narrative_ontology:cs_kernel_codification('2007cbd7-165c-4799-96c4-6473f0763b6b', formalized).
narrative_ontology:cs_authority_grounding('2007cbd7-165c-4799-96c4-6473f0763b6b', lineage).
narrative_ontology:cs_interpretation_layer_present('2007cbd7-165c-4799-96c4-6473f0763b6b').
narrative_ontology:cs_reading_relation('2007cbd7-165c-4799-96c4-6473f0763b6b', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2007cbd7-165c-4799-96c4-6473f0763b6b', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('2007cbd7-165c-4799-96c4-6473f0763b6b', foundational, market_access_not_jurisdiction).
narrative_ontology:cs_axiom_status(market_access_not_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('2007cbd7-165c-4799-96c4-6473f0763b6b', market_access_not_jurisdiction, conventional).
narrative_ontology:cs_axiom('2007cbd7-165c-4799-96c4-6473f0763b6b', foundational, brussels_effect_legitimate_governance).
narrative_ontology:cs_axiom_status(brussels_effect_legitimate_governance, holdable).
narrative_ontology:cs_axiom_grounding('2007cbd7-165c-4799-96c4-6473f0763b6b', brussels_effect_legitimate_governance, instrumental).
narrative_ontology:cs_reference_frame('2007cbd7-165c-4799-96c4-6473f0763b6b', conditional_market_access_framework).
narrative_ontology:cs_drift_state('2007cbd7-165c-4799-96c4-6473f0763b6b', post_gdpr_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2007cbd7-165c-4799-96c4-6473f0763b6b', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, major_non_eu_platforms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, small_non_eu_service_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces GDPR compliance as a condition of EU market access. Administers the Article 3(2) threshold and benefits from global diffusion of EU data-protection standards, which amplifies regulatory influence beyond the EU's territorial borders.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, eu_regulatory_apparatus, beneficiary).

% Must architect global data infrastructure and legal processes to satisfy GDPR in order to retain EU market access. Bear high absolute compliance costs and face strategic constraints on data-centric business models, though they possess resources to negotiate or litigate.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, major_non_eu_platforms, payer,
    powerful, biographical, constrained, global).

% Lack dedicated legal and technical resources to navigate GDPR compliance independently. Frequently rely on third-party infrastructure, exit the EU market entirely, or absorb disproportionate per-revenue compliance costs.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, small_non_eu_service_providers, payer,
    moderate, biographical, constrained, global).

% Receive extended data-protection coverage for processing by foreign entities. Benefit from the global spillover of EU standards without administering the constraint or bearing its direct costs.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, continental).

% Assert that extraterritorial application infringes on their regulatory sovereignty and creates asymmetric trade burdens. Are structurally excluded from the EU legislative and standard-setting process that produces the constraint.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, third_country_governments, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__market_access_reading, eu_regulatory_apparatus).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified data-protection baseline for entities accessing the EU market, reducing regulatory fragmentation and creating a predictable compliance environment for global digital services.
% TRANSFER_FUNCTION: Moves compliance costs, legal-adaptation burdens, and strategic constraints from the EU legislative sphere to non-EU controllers and processors, while transferring standard-setting influence and regulatory prestige to the EU apparatus.
% ABSENT_VOICES: Third-country governments contesting extraterritorial overreach are excluded from the EU legislative process; SMEs from jurisdictions with limited data-protection infrastructure are underrepresented in the standard-setting conversation.
% DISAPPEARANCE_RATIONALE: If GDPR Article 3(2) vanished, global firms would lose a single reference standard and likely face fragmented national regimes; the EU would lose the primary engine of the Brussels Effect; alternative state-centric or sectoral models (e.g., US patchwork, Chinese state-control framework) would expand to fill the governance vacuum.
% FOUNDING_PROBLEM: Cross-border digital services processed EU residents' personal data without adequate safeguards, and divergent national laws within the EU created fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: EU data protection authorities and the Commission attest the problem remains live. Third-country governments and trade-policy scholars outside the benefiting parties contest that the market-access mechanism is proportionate to the problem, arguing it exceeds the EU's internal-market remit.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Base extractiveness (0.58) is moderate: compliance costs and loss of regulatory autonomy for non-EU actors are real, but market access is valuable and the standard reduces fragmentation. Suppression (0.42) is moderate because persistence depends on credible enforcement (fines, market exclusion) yet the framing as market strategy lowers enforcement tension relative to pure jurisdictional assertion. Theater ratio (0.30) reflects that the Brussels Effect is partially performative (signaling regulatory power) but underpinned by genuine enforcement. Accessibility collapse (0.65) is moderately high because global firms tend to adopt GDPR as a universal baseline once the condition is understood. Resistance (0.38) captures persistent transatlantic friction and litigation (Schrems I/II). The claim/metric independence is maintained: the reading is claimed as tangled_rope because the structure hybridizes coordination (global standard diffusion) with asymmetric extraction (unilateral EU influence and payer costs), regardless of whether the raw metrics could be read otherwise.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (EU regulatory apparatus) experiences the constraint as legitimate market governance and strategic standard-setting; its directionality is near the beneficiary pole. The payer seats (non-EU platforms and SMEs) experience the same legal text as a costly, asymmetric barrier to market entry and a loss of regulatory autonomy; their directionality is near the target pole. The beneficiary seat (EU data subjects) experiences extended protection with negligible personal cost. The engine computes these divergent seat classifications from the structural data without requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the EU regulatory apparatus (agenda_setter, collects regulatory influence) and EU data subjects (organized beneficiaries of extended protection). Victim/payer declarations map to non-EU commercial actors who bear compliance costs and strategic constraints. The power asymmetry between major platforms (powerful) and small providers (moderate) means effective extraction is moderated for the former and amplified for the latter, even under the same legal text. Third-country governments are excluded from the rule-making process, so their resistance manifests externally rather than through the constraint's internal directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling the constraint as pure extraction (Snare) by preserving the genuine coordination function: GDPR Article 3 creates a single, predictable standard that reduces regulatory fragmentation for firms operating across borders. It prevents mislabeling as pure coordination (Rope) by acknowledging the asymmetric beneficiary structure: the EU unilaterally sets the standard and captures diffuse regulatory influence, while non-EU parties pay without equivalent say. The active enforcement requirement (fines, market access denial) is structurally necessary to hold the hybrid together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_access_vs_jurisdiction_nature,
    'Is Article 3(2) truly a market-access mechanism, or is it a jurisdictional assertion disguised as market governance?',
    'Comparative enforcement-pattern analysis: if enforcement consistently targets foreign firms lacking a genuine EU market nexus, the jurisdictional reading is supported; if enforcement tracks market-access conditionality, the market-access reading holds.',
    'A jurisdictional nature would raise suppression and support reclassification toward snare; a genuine market-access nature keeps the coordination function intact and supports tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_access_vs_jurisdiction_nature, conceptual, 'Whether the legal mechanism is market access or jurisdictional assertion').

omega_variable(
    brussels_effect_intentionality,
    'Does the EU regulatory apparatus intentionally leverage market access for standard diffusion, or is the Brussels Effect an emergent byproduct of internal-market regulation?',
    'Internal EU legislative history, Commission impact assessments, and institutional communications.',
    'If intentional, the extraction is more targeted and the constraint leans snare-like; if emergent, the extraction is less directed and the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brussels_effect_intentionality, empirical, 'Whether Brussels Effect is intentional strategy or emergent outcome').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_ma_tr_t0, gdpr_article_3_scope__market_access_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gdpr_ma_tr_t1, gdpr_article_3_scope__market_access_reading, theater_ratio, 1, 0.25).
narrative_ontology:measurement(gdpr_ma_tr_t2, gdpr_article_3_scope__market_access_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(gdpr_ma_tr_t3, gdpr_article_3_scope__market_access_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(gdpr_ma_tr_t4, gdpr_article_3_scope__market_access_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(gdpr_ma_tr_t5, gdpr_article_3_scope__market_access_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(gdpr_ma_tr_t6, gdpr_article_3_scope__market_access_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement(gdpr_ma_tr_t7, gdpr_article_3_scope__market_access_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement(gdpr_ma_tr_t8, gdpr_article_3_scope__market_access_reading, theater_ratio, 8, 0.3).

% Extraction over time
narrative_ontology:measurement(gdpr_ma_be_t0, gdpr_article_3_scope__market_access_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gdpr_ma_be_t1, gdpr_article_3_scope__market_access_reading, base_extractiveness, 1, 0.4).
narrative_ontology:measurement(gdpr_ma_be_t2, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(gdpr_ma_be_t3, gdpr_article_3_scope__market_access_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(gdpr_ma_be_t4, gdpr_article_3_scope__market_access_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(gdpr_ma_be_t5, gdpr_article_3_scope__market_access_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(gdpr_ma_be_t6, gdpr_article_3_scope__market_access_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(gdpr_ma_be_t7, gdpr_article_3_scope__market_access_reading, base_extractiveness, 7, 0.57).
narrative_ontology:measurement(gdpr_ma_be_t8, gdpr_article_3_scope__market_access_reading, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_ma_su_t0, gdpr_article_3_scope__market_access_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gdpr_ma_su_t1, gdpr_article_3_scope__market_access_reading, suppression_requirement, 1, 0.25).
narrative_ontology:measurement(gdpr_ma_su_t2, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2, 0.35).
narrative_ontology:measurement(gdpr_ma_su_t3, gdpr_article_3_scope__market_access_reading, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(gdpr_ma_su_t4, gdpr_article_3_scope__market_access_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(gdpr_ma_su_t5, gdpr_article_3_scope__market_access_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(gdpr_ma_su_t6, gdpr_article_3_scope__market_access_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(gdpr_ma_su_t7, gdpr_article_3_scope__market_access_reading, suppression_requirement, 7, 0.46).
narrative_ontology:measurement(gdpr_ma_su_t8, gdpr_article_3_scope__market_access_reading, suppression_requirement, 8, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The gdpr_article_3_scope kernel decomposes into three structurally distinct readings. The market_access_reading (this file) frames extraterritoriality as conditional market access and Brussels Effect standard-setting. The effects_jurisdiction_reading frames it as protective jurisdiction following data-subject effects. The territorial_sovereignty_reading frames it as overreach. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
