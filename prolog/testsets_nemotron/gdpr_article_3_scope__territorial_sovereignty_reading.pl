% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: GDPR Article 3 Scope - Territorial Sovereignty Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the territorial sovereignty reading of
 *   GDPR Article 3's jurisdictional scope. The reading holds that legitimate
 *   regulatory authority is bounded by territorial sovereignty, and that
 *   Article 3(2)'s extraterritorial reach (targeting/monitoring of EU
 *   residents by non-EU controllers) exceeds legitimate authority. The
 *   constraint operates as a resistance mechanism: non-EU states adopt data
 *   localization laws, blocking statutes, and adequacy negotiation tactics to
 *   contest EU jurisdictional claims. This creates a tangled rope — genuine
 *   coordination of national regulatory autonomy coexists with asymmetric
 *   extraction where EU data subjects abroad lose protection and cross-border
 *   service providers bear compliance fragmentation costs. The beneficiary is
 *   non-EU state regulatory independence; the victims are EU residents
 *   outside the EU and the operators caught in jurisdictional conflict.
 *
 * KEY AGENTS:
 *   - non_eu_state_regulators: Primary beneficiary (institutional/arbitrage) — gains regulatory autonomy from contesting extraterritorial reach
 *   - eu_data_subjects_abroad: Primary victim (powerless/trapped) — loses GDPR protection when outside EU territory
 *   - cross_border_service_providers: Primary victim (organized/constrained) — bears fragmentation costs of conflicting jurisdictional claims
 *   - national_security_agencies: Secondary beneficiary (institutional/identity_locked) — uses sovereignty argument to preserve surveillance access
 *   - eu_supervisory_authorities: Agenda setter (institutional/constrained) — enforces Article 3(2) but faces legitimacy contests
 *   - multinational_compliance_teams: Payer (organized/constrained) — implements contradictory compliance regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.35).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.58).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Scope - Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '1bc426e9-e033-42bd-9be4-4a4df59248e1').
narrative_ontology:cs_kernel_codification('1bc426e9-e033-42bd-9be4-4a4df59248e1', formalized).
narrative_ontology:cs_authority_grounding('1bc426e9-e033-42bd-9be4-4a4df59248e1', lineage).
narrative_ontology:cs_interpretation_layer_present('1bc426e9-e033-42bd-9be4-4a4df59248e1').
narrative_ontology:cs_reading_relation('1bc426e9-e033-42bd-9be4-4a4df59248e1', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bc426e9-e033-42bd-9be4-4a4df59248e1', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('1bc426e9-e033-42bd-9be4-4a4df59248e1', foundational, regulatory_authority_requires_territorial_nexus).
narrative_ontology:cs_axiom_status(regulatory_authority_requires_territorial_nexus, holdable).
narrative_ontology:cs_axiom_grounding('1bc426e9-e033-42bd-9be4-4a4df59248e1', regulatory_authority_requires_territorial_nexus, conventional).
narrative_ontology:cs_axiom('1bc426e9-e033-42bd-9be4-4a4df59248e1', foundational, extraterritorial_data_regulation_is_impermissible_overreach).
narrative_ontology:cs_axiom_status(extraterritorial_data_regulation_is_impermissible_overreach, holdable).
narrative_ontology:cs_axiom_grounding('1bc426e9-e033-42bd-9be4-4a4df59248e1', extraterritorial_data_regulation_is_impermissible_overreach, conventional).
narrative_ontology:cs_reference_frame('1bc426e9-e033-42bd-9be4-4a4df59248e1', westphalian_sovereignty_in_digital_domain).
narrative_ontology:cs_drift_state('1bc426e9-e033-42bd-9be4-4a4df59248e1', post_schrems_ii_adequacy_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1bc426e9-e033-42bd-9be4-4a4df59248e1', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, domestic_data_processors).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, national_security_agencies).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_subjects_abroad).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, cross_border_service_providers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, multinational_compliance_teams).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, territorial_sovereignty_principle).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, legitimate_regulatory_authority_bounded_by_territory).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, data_localization_as_sovereignty_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert regulatory sovereignty by contesting GDPR's extraterritorial reach. Enact data localization laws, blocking statutes, and negotiate adequacy on their own terms. Gain autonomy to set domestic data rules without EU oversight. Can shift between alignment and resistance depending on geopolitical leverage.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators, beneficiary,
    institutional, generational, arbitrage, national).

% Lose GDPR protections when physically outside EU territory. Non-EU states' sovereignty claims mean their data can be processed under lower standards. No practical exit: cannot carry EU jurisdiction with them, cannot easily avoid non-EU processing when living/working abroad.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_subjects_abroad, payer,
    powerless, biographical, trapped, global).

% Must comply with contradictory regimes: GDPR extraterritoriality vs non-EU data localization. Implement duplicate infrastructure, conflicting contractual clauses, and jurisdictional firewalls. Exit options constrained: leaving EU market loses revenue; leaving non-EU markets loses growth. Compliance cost scales with jurisdictional conflict intensity.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, cross_border_service_providers, payer,
    organized, biographical, constrained, global).

% Use territorial sovereignty arguments to preserve extraterritorial surveillance access (e.g., US CLOUD Act, Chinese NIL/CSL). Data localization laws often exempt government access. Their institutional identity fuses with the sovereignty claim — exit would mean abandoning the 'protect national security' mandate that constitutes their legitimacy.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, national_security_agencies, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, national_security_agencies, agenda_setter).

% Enforce Article 3(2) against non-EU controllers targeting EU residents. Issue fines, adequacy decisions, and guidance. Face legitimacy contests: non-EU states reject their jurisdiction, courts challenge extraterritorial reach. Constrained exit: mandate requires enforcement, but effectiveness depends on cooperation they cannot compel.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_supervisory_authorities, agenda_setter,
    institutional, generational, constrained, continental).

% Implement compliance programs that satisfy contradictory legal regimes. Build technical architectures for data localization, transfer mechanisms, and jurisdictional segmentation. Bear the operational cost of the sovereignty conflict. Professional identity tied to navigating complexity — exit means leaving the field.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, multinational_compliance_teams, payer,
    organized, biographical, constrained, global).

% Advocate for consistent high standards globally. Excluded from sovereignty negotiations between states. Would object to both the protection gap (territorial reading) and the regulatory imperialism charge (effects reading). Mobile exit: can shift advocacy focus but cannot change state sovereignty dynamics.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, digital_rights_ngos, excluded,
    organized, generational, mobile, global).

% Negotiate adequacy decisions that bridge the sovereignty gap. Analyze whether non-EU law provides 'essentially equivalent' protection. Their assessments determine whether data flows legally or requires supplementary measures. Analytical seat: they observe the conflict but their role is to resolve it technically.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, adequacy_negotiators, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legitimate regulatory diversity among sovereign states in the digital domain — each state sets rules for data processing within its territory without external imposition.
% TRANSFER_FUNCTION: Moves compliance burden and protection gaps from non-EU states (who avoid EU regulatory extension) onto EU data subjects abroad (who lose rights) and cross-border providers (who bear fragmentation costs). Non-EU states gain regulatory autonomy; EU residents and global operators pay.
% ABSENT_VOICES: EU data subjects physically outside the EU — they are the direct victims of the protection gap but have no representation in non-EU legislative processes that enact data localization or blocking statutes. Also excluded: small enterprises and civil society in non-EU states who might benefit from GDPR-level standards but have no voice in sovereignty assertions.
% DISAPPEARANCE_RATIONALE: If the territorial sovereignty reading vanished (i.e., non-EU states accepted Article 3(2) as legitimate), data localization laws enacted as resistance would lose their primary justification, adequacy negotiations would accelerate, and EU data subjects abroad would regain consistent protection. Cross-border providers would see compliance fragmentation collapse. The Brussels Effect would operate without sovereign contestation.
% FOUNDING_PROBLEM: The Westphalian sovereign state system assumes territorial jurisdiction; digital data flows do not respect territorial boundaries. The founding problem is how to reconcile sovereign regulatory authority with borderless data processing — specifically, whether the EU can legitimately regulate processing outside its territory that affects its residents.
% FOUNDING_PROBLEM_CORROBORATION: The territorial sovereignty reading is corroborated by non-EU state practice (Russia's localization law, China's CSL/PIPL, India's draft DPDP localization provisions, Brazil's LGPD territorial scope debates) and by international law scholars who argue extraterritorial jurisdiction requires a permissive rule (e.g., Lotus principle). The effects and market access readings are corroborated by EU legislative history (GDPR recitals 22-25), CJEU jurisprudence (Google Spain, Schrems), and the observable Brussels Effect where non-EU companies adopt GDPR standards globally. No single corroboration settles the dispute — the kernel is genuinely contested.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).
:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.35) reflects that the constraint's primary operation is jurisdictional contestation rather than direct value transfer — the extraction is the compliance cost imposed on cross-border operators and the protection gap for EU data subjects abroad. Suppression (0.58) is moderate-high: states enforce data localization and blocking statutes, but cannot fully prevent EU enforcement actions (fines, adequacy decisions). Theater ratio (0.42) is significant: many localization laws create performative data storage without preventing extraterritorial access (e.g., US CLOUD Act reaches data stored in EU). Accessibility collapse (0.45) is moderate — alternatives like Standard Contractual Clauses and Binding Corporate Rules exist but are under legal challenge. Resistance (0.72) is high: multiple states have enacted blocking statutes, adequacy negotiations are protracted, and the US-EU data transfer framework has been invalidated twice (Schrems I/II).
 *
 * PERSPECTIVAL GAP:
 *   From the non-EU regulator seat, the constraint is genuine coordination of sovereign authority against imperial overreach. From the EU data subject seat, it is extraction of their rights by their own government's inability to protect them abroad. From the cross-border provider seat, it is a snare — conflicting mandatory regimes with no compliant path. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-EU state regulators are structural beneficiaries (d ~ 0.15): they gain regulatory autonomy and avoid EU enforcement. EU data subjects abroad are structural targets (d ~ 0.85): they lose rights without gaining alternatives. Cross-border providers are targets (d ~ 0.75): they bear fragmentation costs with constrained exit (cannot serve EU market without compliance, cannot serve non-EU markets with EU compliance). National security agencies are beneficiaries (d ~ 0.20): sovereignty arguments protect surveillance prerogatives. EU supervisory authorities are near-symmetric (d ~ 0.50): they enforce but face legitimacy erosion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting EU residents' data) remains live, but the territorial sovereignty reading treats the extraterritorial mechanism as mandated beyond its legitimate function. The constraint persists because the sovereignty claim has no natural sunset — it is a structural feature of the Westphalian system confronting digital boundarylessness. Classification as tangled rope (not snare) captures that non-EU states genuinely coordinate regulatory diversity, but the asymmetric cost on data subjects and providers makes it extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Which specific structural element of GDPR Article 3 do the three readings disagree on: the text itself, the enforcement mechanism, or the legitimacy predicate?',
    'Comparative analysis of how each reading treats the ''establishment'' vs ''targeting'' criteria in Article 3(1) vs 3(2), and whether they treat the Brussels Effect as coordination or extraction.',
    'If disagreement is on text: irreducible conceptual split. If on enforcement: empirical question of compliance patterns. If on legitimacy: normative dispute about sovereign authority in digital space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Location of structural disagreement among the three kernel readings').

omega_variable(
    sovereignty_vs_protection_tradeoff,
    'Does the territorial sovereignty reading genuinely protect regulatory independence, or does it enable a race to the bottom in data protection standards?',
    'Longitudinal study of data protection law adoption in non-EU jurisdictions post-GDPR: strengthening vs weakening vs divergence.',
    'If race-to-bottom: sovereignty reading extracts from data subjects'' protection. If strengthening: sovereignty reading coordinates genuine regulatory diversity. If divergence: both coordination and extraction present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_protection_tradeoff, empirical, 'Whether territorial sovereignty generates regulatory race-to-bottom or genuine diversity').

omega_variable(
    data_localization_effectiveness,
    'Does data localization actually achieve the sovereignty objectives its proponents claim, or does it create performative compliance without substantive control?',
    'Technical assessment of whether localized data storage prevents extraterritorial access by foreign intelligence services and courts (e.g., US CLOUD Act vs EU data localization).',
    'If effective: localization is genuine coordination for sovereignty. If performative: localization is theater masking continued extraterritorial reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_localization_effectiveness, empirical, 'Whether data localization delivers substantive sovereignty or performative theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 2018, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement_basis(gdpr_tr_t2018, observed).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement_basis(gdpr_tr_t2020, observed).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement_basis(gdpr_tr_t2022, observed).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(gdpr_tr_t2024, observed).
narrative_ontology:measurement(gdpr_tr_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(gdpr_tr_t2026, observed).
narrative_ontology:measurement(gdpr_tr_t2028, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2028, 0.42).
narrative_ontology:measurement_basis(gdpr_tr_t2028, projected).
narrative_ontology:measurement(gdpr_tr_t2030, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2030, 0.42).
narrative_ontology:measurement_basis(gdpr_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2018, 0.22).
narrative_ontology:measurement_basis(gdpr_be_t2018, observed).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2020, 0.28).
narrative_ontology:measurement_basis(gdpr_be_t2020, observed).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2022, 0.31).
narrative_ontology:measurement_basis(gdpr_be_t2022, observed).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2024, 0.33).
narrative_ontology:measurement_basis(gdpr_be_t2024, observed).
narrative_ontology:measurement(gdpr_be_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2026, 0.34).
narrative_ontology:measurement_basis(gdpr_be_t2026, observed).
narrative_ontology:measurement(gdpr_be_t2028, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2028, 0.35).
narrative_ontology:measurement_basis(gdpr_be_t2028, projected).
narrative_ontology:measurement(gdpr_be_t2030, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2030, 0.35).
narrative_ontology:measurement_basis(gdpr_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement_basis(gdpr_su_t2018, observed).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement_basis(gdpr_su_t2020, observed).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement_basis(gdpr_su_t2022, observed).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2024, 0.56).
narrative_ontology:measurement_basis(gdpr_su_t2024, observed).
narrative_ontology:measurement(gdpr_su_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(gdpr_su_t2026, observed).
narrative_ontology:measurement(gdpr_su_t2028, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2028, 0.6).
narrative_ontology:measurement_basis(gdpr_su_t2028, projected).
narrative_ontology:measurement(gdpr_su_t2030, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2030, 0.62).
narrative_ontology:measurement_basis(gdpr_su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__territorial_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, eu_us_data_transfer_framework).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, national_data_localization_laws).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, cloud_act_vs_gdpr_conflict).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the GDPR Article 3 kernel family. Each has distinct ε: effects_jurisdiction_reading ε ≈ 0.15 (coordination-dominant), market_access_reading ε ≈ 0.25 (coordination with standard-setting rent), territorial_sovereignty_reading ε = 0.35 (contestation-dominant). The territorial reading's higher ε reflects the cost of jurisdictional conflict and protection gaps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__territorial_sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(gdpr_article_3_scope__territorial_sovereignty_reading, powerless, 0.85).
constraint_indexing:directionality_override(gdpr_article_3_scope__territorial_sovereignty_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
