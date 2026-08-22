% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: GDPR Article 3 Scope as Conditional Market Access (Market Access Reading)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint story captures the market access reading of GDPR Article
 *   3's extraterritorial scope. Under this reading, GDPR applies to non-EU
 *   controllers not because the EU asserts jurisdiction over their territory,
 *   but because access to the EU digital market is conditioned on compliance
 *   — a Brussels Effect mechanism where the EU's market power diffuses its
 *   regulatory standards globally. Compliance is a market entry strategy, not
 *   submission to extraterritorial jurisdiction. The constraint's extraction
 *   is low and declining (0.35→0.28) as compliance becomes standardized;
 *   suppression is minimal and falling (0.25→0.15) because enforcement
 *   follows market incentives rather than coercive territorial claims. The
 *   claimed type is rope: a coordination mechanism solving the problem of
 *   cross-border data flow standards with minimal coercive overhead, where
 *   participants (global companies seeking EU market access) are net
 *   beneficiaries of the standard.
 *
 * KEY AGENTS:
 *   - eu_regulatory_influence: Primary beneficiary (institutional/arbitrage) — gains global regulatory influence via standard diffusion
 *   - eu_data_subjects: Beneficiary (organized/constrained) — gains privacy protections that travel with their data
 *   - compliant_global_companies: Beneficiary (powerful/constrained) — gains EU market access and competitive advantage through adequacy
 *   - non_compliant_external_controllers: Victim (moderate/trapped) — bears compliance costs or loses EU market access
 *   - non_eu_regulators: Observer (institutional/analytical) — watches Brussels Effect diffusion; some adopt similar standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.28).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.15).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Scope as Conditional Market Access (Market Access Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology_governance/international_law/privacy_regulation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, '251f8cea-d50c-4979-88f8-c4ade7d74eed').
narrative_ontology:cs_kernel_codification('251f8cea-d50c-4979-88f8-c4ade7d74eed', formalized).
narrative_ontology:cs_authority_grounding('251f8cea-d50c-4979-88f8-c4ade7d74eed', lineage).
narrative_ontology:cs_interpretation_layer_present('251f8cea-d50c-4979-88f8-c4ade7d74eed').
narrative_ontology:cs_reading_relation('251f8cea-d50c-4979-88f8-c4ade7d74eed', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('251f8cea-d50c-4979-88f8-c4ade7d74eed', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('251f8cea-d50c-4979-88f8-c4ade7d74eed', foundational, market_access_conditionality_not_jurisdictional_claim).
narrative_ontology:cs_axiom_status(market_access_conditionality_not_jurisdictional_claim, holdable).
narrative_ontology:cs_axiom_grounding('251f8cea-d50c-4979-88f8-c4ade7d74eed', market_access_conditionality_not_jurisdictional_claim, conventional).
narrative_ontology:cs_axiom('251f8cea-d50c-4979-88f8-c4ade7d74eed', foundational, brussels_effect_standard_diffusion_as_legitimate_coordination).
narrative_ontology:cs_axiom_status(brussels_effect_standard_diffusion_as_legitimate_coordination, holdable).
narrative_ontology:cs_axiom_grounding('251f8cea-d50c-4979-88f8-c4ade7d74eed', brussels_effect_standard_diffusion_as_legitimate_coordination, instrumental).
narrative_ontology:cs_reference_frame('251f8cea-d50c-4979-88f8-c4ade7d74eed', market_access_conditionality_framework).
narrative_ontology:cs_drift_state('251f8cea-d50c-4979-88f8-c4ade7d74eed', post_schrems_ii_adequacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('251f8cea-d50c-4979-88f8-c4ade7d74eed', '2026-06-15T14:30:00Z').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_regulatory_influence).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, compliant_global_companies).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_compliant_external_controllers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, compliant_global_companies).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, brussels_effect_standard_diffusion).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, market_power_as_regulatory_leverage).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, adequacy_as_strategic_asset).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The EU's regulatory influence expands globally as companies adopt GDPR standards to access the EU market. This influence is exercised through adequacy decisions, standard contractual clauses, and the gravitational pull of the EU market — not through direct enforcement against non-EU entities. The EU can shape global privacy norms without bearing the enforcement costs of territorial jurisdiction.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_regulatory_influence, beneficiary,
    institutional, generational, arbitrage, global).

% EU residents gain privacy rights that follow their data globally when controllers target the EU market. Their protections travel with the data. However, they cannot easily exit the data ecosystem — digital participation requires data sharing — and their remedies depend on EU enforcement capacity against non-EU controllers.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, global).

% Large global companies (tech platforms, multinationals) invest in GDPR compliance as a market entry strategy. They gain access to the EU market, adequacy status as a competitive asset, and a unified global standard that reduces regulatory fragmentation costs. They pay substantial compliance costs but capture disproportionate market value. Their exit option (leaving the EU market) is theoretically available but commercially prohibitive.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, compliant_global_companies, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, compliant_global_companies, payer).

% Smaller non-EU companies that process EU resident data without targeting the EU market, or that target it without full compliance. They face the same compliance costs as large companies but lack the scale to amortize them. Their exit options are limited: withdraw from EU market (losing revenue), comply at disproportionate cost, or operate non-compliantly (enforcement risk). They are the primary extraction targets under this reading.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_compliant_external_controllers, payer,
    moderate, biographical, trapped, regional).

% Regulators in other jurisdictions observe the Brussels Effect. Some adopt GDPR-like standards (Brazil's LGPD, California's CCPA/CPRA, China's PIPL) — becoming diffusion beneficiaries. Others resist as sovereignty incursions. Their analytical seat tracks whether the market access mechanism produces genuine privacy gains or regulatory capture.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the cross-border data flow coordination problem: a single, high-standard privacy framework that travels with data, enabling global digital commerce without regulatory fragmentation. Companies comply once for global operations; data subjects get consistent protections; regulators get a reference standard.
% TRANSFER_FUNCTION: Moves compliance costs from global companies (especially non-EU controllers targeting the EU market) to the EU regulatory ecosystem in the form of standardized data practices. The EU gains global regulatory influence; compliant companies gain market access and adequacy premiums; non-compliant controllers bear costs without offsetting benefits.
% ABSENT_VOICES: Small non-EU businesses that process EU data incidentally (e.g., a US blog with EU readers) — they face GDPR compliance costs without targeting the EU market. They are structurally excluded from the 'market access' framing because they didn't choose market entry; the targeting test is supposed to exclude them but in practice captures them. Also absent: data subjects in non-adequacy jurisdictions whose data flows through GDPR-compliant pipes but who lack enforcement standing.
% DISAPPEARANCE_RATIONALE: If the market access condition vanished overnight, global companies would face regulatory fragmentation — each jurisdiction setting its own privacy standards. Compliance costs would rise from multiple conflicting standards. The EU would lose its primary lever for global privacy norm diffusion. Data subjects outside the EU would lose the traveling protections that GDPR compliance currently provides.
% FOUNDING_PROBLEM: Pre-GDPR: fragmented national privacy laws in the EU (Data Protection Directive 95/46/EC implemented differently across member states) and no global standard for cross-border data flows. Companies faced conflicting requirements; data subjects had inconsistent protections; the EU lacked a mechanism to project its privacy model globally.
% FOUNDING_PROBLEM_CORROBORATION: The EU Commission attests the fragmentation problem is substantially solved internally but the global projection problem remains live (adequacy decisions as ongoing work). Global companies attest the internal fragmentation is solved but argue the global projection has become a compliance burden disproportionate to the coordination benefit. Civil society groups (EDRi, Access Now) attest the founding problem is live globally — most of the world still lacks GDPR-level protections — but contest whether the market access mechanism delivers them. No single external corroborator agrees with the EU's self-assessment.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__market_access_reading_tests).
:- end_tests(gdpr_article_3_scope__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) reflects that compliance costs are real but proportional to the value of EU market access — they are the price of admission, not a transfer extracted by coercion. The decline from 0.35 (2018) to 0.28 (2024) shows standardization reducing marginal compliance costs. Suppression (0.15) is low because the constraint operates through market logic: non-compliant controllers can exit the EU market rather than submit. Theater ratio (0.22) captures that some enforcement activity performs 'privacy theater' (performative DPO appointments, boilerplate policies) but the core mechanism is functional standard-setting. Accessibility collapse (0.35) is moderate: alternatives exist (local EU representatives, adequacy decisions, standard contractual clauses) but are structured by the same standard. Resistance (0.25) is low and focused on compliance cost proportionality, not the standard's legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   The EU regulatory influence seat (institutional/arbitrage) is the structural beneficiary: it gains global norm-setting power without enforcement costs scaling with scope. EU data subjects (organized/constrained) benefit from traveling protections but have limited exit from the data ecosystem. Compliant global companies (powerful/constrained) sit near symmetric: they pay compliance costs but capture the EU market and adequacy premium. Non-compliant external controllers (moderate/trapped) are the targets: they face the full compliance cost with no offsetting benefit, and their exit (market withdrawal) is costly. Non-EU regulators (institutional/analytical) observe from analytical distance — some adopt GDPR-like standards (diffusion beneficiaries), others resist (sovereignty defenders). The market access reading's key structural claim: directionality derives from market power asymmetry, not jurisdictional assertion.
 *
 * MANDATROPHY ANALYSIS:
 *   The market access reading prevents misclassifying Brussels Effect standard diffusion as jurisdictional overreach. If GDPR Article 3 were read as effects jurisdiction (the sibling reading), the same compliance costs would appear as extraterritorial extraction — a tangled_rope or snare classification. The market access reading reframes the constraint as voluntary market participation conditioned on a standard, making it a rope. This matters because the mandatrophy risk is opposite: the effects jurisdiction reading risks labeling legitimate standard-setting as imperial overreach; the market access reading risks obscuring where market power makes 'voluntary' compliance effectively mandatory for controllers who cannot exit the EU market. The rope classification holds only while exit remains viable for significant controller classes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_access_vs_jurisdiction_boundary,
    'Where does the market access reading''s standard diffusion mechanism end and the effects jurisdiction reading''s territorial claim begin?',
    'Case law analysis of CJEU rulings on Article 3(2) targeting/monitoring test vs. market access conditionality; legislative history of GDPR recitals 23-25 vs. Article 45 adequacy decisions',
    'If the boundary is porous, the market access reading may be a strategic framing of the same jurisdictional reach; if distinct, they are genuinely different constraints with different extraction profiles',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_access_vs_jurisdiction_boundary, conceptual, 'Structural boundary between market access conditionality and effects-based jurisdiction').

omega_variable(
    extraction_from_non_compliant_controllers,
    'Does the cost of GDPR compliance for non-EU controllers constitute extraction or the price of market access?',
    'Comparative cost analysis: compliance costs for EU market access vs. revenue from EU market; exit option viability (market withdrawal vs. compliance)',
    'If costs exceed proportional market value for significant controller classes, extraction is higher than the market access framing suggests; if costs are proportional, the rope classification holds',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_from_non_compliant_controllers, empirical, 'Whether compliance costs function as market entry price or extractive transfer').

omega_variable(
    reading_commitment_kernel_gdpr_article_3,
    'This constraint is the market_access_reading of kernel gdpr_article_3_scope. How does this reading''s structural profile differ from the effects_jurisdiction_reading and territorial_sovereignty_reading?',
    'Comparative constraint story generation: instantiate all three readings as separate constraint stories with their own ε, beneficiaries, victims, and stakeholder surfaces; the engine computes per-seat classifications for each reading independently',
    'If readings produce divergent classifications (e.g., market_access_reading = rope, effects_jurisdiction_reading = tangled_rope, territorial_sovereignty_reading = snare), the kernel decomposition is validated; if they converge, the kernel label may be obscuring a single constraint',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commitment_kernel_gdpr_article_3, conceptual, 'Kernel-reading decomposition for GDPR Article 3 scope: market access vs. effects jurisdiction vs. territorial sovereignty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 2018, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_tr_t2018, gdpr_article_3_scope__market_access_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_tr_t2020, gdpr_article_3_scope__market_access_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_tr_t2022, gdpr_article_3_scope__market_access_reading, theater_ratio, 2022, 0.22).
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_tr_t2024, gdpr_article_3_scope__market_access_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_be_t2018, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_be_t2020, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2020, 0.3).
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_be_t2022, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2022, 0.28).
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_be_t2024, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_su_t2018, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2018, 0.25).
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_su_t2020, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2020, 0.18).
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_su_t2022, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2022, 0.15).
narrative_ontology:measurement(gdpr_article_3_scope__market_access_reading_su_t2024, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__market_access_reading, 0.02).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_adequacy_decisions).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, global_data_transfer_mechanisms).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, eu_digital_market_regulation).

% DUAL FORMULATION NOTE:
% This constraint (market_access_reading) and its siblings (effects_jurisdiction_reading, territorial_sovereignty_reading) form a constraint family decomposing the GDPR Article 3 scope kernel. Each reading instantiates a different constraint with distinct ε, beneficiaries, and classification. The market access reading has the lowest extraction (rope); effects jurisdiction reading likely shows higher extraction (tangled_rope) due to enforcement tension; territorial sovereignty reading likely shows highest extraction (snare) from the sovereignty-defender seat. The family is linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__market_access_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
