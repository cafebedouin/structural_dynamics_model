% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__sovereignty_primacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary (Sovereignty Primacy Reading)
 *   domain: political_economy/international_trade_law
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-primacy reading of the
 *   contested NAFTA jurisdictional boundary kernel: trade agreement text
 *   operates as a coordination mechanism (tariff schedules, dispute
 *   procedures, transparency norms) strictly subordinate to the sovereign
 *   regulatory authority of each signatory state. Domestic governments retain
 *   full power to set labor, environmental, and health standards within their
 *   territory without trade agreements overriding those standards through
 *   investor-state mechanisms or regulatory harmonization mandates. The
 *   reading prioritizes democratic institutions' authority to protect public
 *   goods (labor dignity, environmental integrity, public health) over
 *   capital's claim to harmonized low-friction regulatory environments. This
 *   is one of three incompatible readings of the same kernel; the other
 *   readings (capital-supremacy and embedded-liberalism) would instantiate
 *   different constraints with different ε values and different
 *   beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - Domestic regulatory agencies (labor, environment, health): set and enforce standards; retain full statutory authority; no treaty override
 *   - Multinational capital: faces unharmonized compliance costs; can exit to lower-regulation jurisdictions but cannot block domestic regulation via trade mechanisms
 *   - Labor standards advocates and environmental movements: gain authority to advocate for stricter protections without trade agreements as a ceiling
 *   - Trade signatories (states): retain sovereignty to modify domestic law; treaty obligations trigger retaliation, not domestic court override
 *   - Investor plaintiffs: excluded from ISDS; cannot sue governments for regulatory losses across borders
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.28).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary (Sovereignty Primacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "political_economy/international_trade_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '29244186-b4fc-4861-ba71-96c194271b6c').
narrative_ontology:cs_kernel_codification('29244186-b4fc-4861-ba71-96c194271b6c', fixed_text).
narrative_ontology:cs_authority_grounding('29244186-b4fc-4861-ba71-96c194271b6c', extraction).
narrative_ontology:cs_interpretation_layer_present('29244186-b4fc-4861-ba71-96c194271b6c').
narrative_ontology:cs_reading_relation('29244186-b4fc-4861-ba71-96c194271b6c', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('29244186-b4fc-4861-ba71-96c194271b6c', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_axiom('29244186-b4fc-4861-ba71-96c194271b6c', foundational, regulatory_authority_is_sovereign).
narrative_ontology:cs_axiom_status(regulatory_authority_is_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('29244186-b4fc-4861-ba71-96c194271b6c', regulatory_authority_is_sovereign, deontological).
narrative_ontology:cs_axiom('29244186-b4fc-4861-ba71-96c194271b6c', foundational, trade_text_is_subordinate_to_domestic_law).
narrative_ontology:cs_axiom_status(trade_text_is_subordinate_to_domestic_law, holdable).
narrative_ontology:cs_axiom_grounding('29244186-b4fc-4861-ba71-96c194271b6c', trade_text_is_subordinate_to_domestic_law, conventional).
narrative_ontology:cs_reference_frame('29244186-b4fc-4861-ba71-96c194271b6c', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('29244186-b4fc-4861-ba71-96c194271b6c', contemporary_capital_mobility_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('29244186-b4fc-4861-ba71-96c194271b6c', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_standards_advocates).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, environmental_protection_movements).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, health_standards_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, small_domestic_enterprises).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_capital).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_agreement_signatories).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, democratic_accountability_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, regulatory_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain full statutory authority to set labor, environmental, and health standards within territorial jurisdiction. Under this reading, trade agreement provisions do not constrain their rulemaking power—they may adjust regulations freely and face only the market consequences of trade partners' response (tariffs, market access restrictions). They set the domestic regulatory floor; treaty participation is optional compliance.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Faces potential regulatory divergence across jurisdictions and cannot rely on harmonized standards to reduce compliance costs. Under this reading, a regulatory agency can tighten standards unilaterally (carbon, wage, safety floors) and multinational firms must either comply with higher costs or reduce operations in that jurisdiction. Their exit option is to move investment to jurisdictions with lower regulatory burdens, but they cannot leverage trade agreements to block regulatory tightening at home.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_capital, payer,
    powerful, biographical, arbitrage, global).

% Gain the right to demand domestic labor protections without fear that trade agreements will preempt them through investor-state dispute resolution or regulatory harmonization mandates. Under this reading, they can petition their government for stricter standards (child labor prohibitions, union rights, wage floors) and the government retains full authority to implement them, subject only to non-discrimination requirements for foreign firms operating domestically.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_standards_advocates, beneficiary,
    organized, generational, constrained, national).

% Retain the ability to advocate for and implement domestic environmental standards (emissions caps, water quality, species protection) without trade agreements serving as a ceiling or override mechanism. Under this reading, a country can adopt carbon taxes, renewable energy mandates, or stricter pesticide bans, and foreign firms must comply or exit the market. Trade agreements do not preempt environmental rulemaking.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, environmental_protection_movements, beneficiary,
    moderate, generational, constrained, national).

% Exercise full jurisdiction over pharmaceutical approval, food safety, and occupational health standards. Under this reading, they set the domestic health floor independently and trade agreements do not require them to accept lower-standard foreign products or harmonize to international minimums that fall below domestic needs.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, health_standards_administrators, beneficiary,
    institutional, generational, analytical, national).

% Negotiate and execute trade agreements but retain full sovereign authority to implement or modify domestic law unilaterally. Under this reading, they can sign a trade text as a coordination mechanism (tariff schedules, dispute resolution procedures, transparency norms) without subordinating their legislatures or regulatory bodies to treaty obligations. Breach of the treaty triggers trade retaliation by other signatories, not domestic judicial override of their own laws.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_agreement_signatories, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_agreement_signatories, payer).

% Benefit from the ability to advocate for and comply with domestic labor and environmental standards without large multinational competitors using trade agreements to weaken those standards in their home market. Under this reading, they can compete on domestic terms set by their own government without foreign capital using trade rules to override domestic floors.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, small_domestic_enterprises, beneficiary,
    moderate, biographical, constrained, local).

% Interpret and apply trade agreement text within the scope of tariff schedules, non-discrimination, and transparency—but do not exercise authority over domestic legislative or regulatory rulemaking. Under this reading, they cannot overturn a domestic law enacted by a sovereign parliament; they can only assess whether the law violates agreed trade disciplines and authorize compensatory tariffs if it does.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_dispute_resolution_bodies, observer,
    institutional, generational, analytical, global).

% Are excluded from direct challenge to domestic regulatory decisions via investor-state dispute settlement (ISDS). Under this reading, they cannot sue a sovereign government in arbitration for regulatory losses; they can only lobby their home government to pursue inter-state dispute resolution if they believe a standard violates the trade agreement.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, investor_plaintiffs, excluded,
    powerful, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__sovereignty_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes transparent tariff schedules, non-discrimination rules (national treatment, most-favored-nation), and dispute resolution procedures so trading states can negotiate market access without unilateral protectionism—while reserving the right to regulate labor, environment, and health standards as domestic priorities.
% TRANSFER_FUNCTION: Does not move wealth directly; instead creates differential costs for multinational capital: firms in jurisdictions with tighter standards bear higher compliance costs, while labor advocates and environmental movements gain authority to advocate for and implement stronger protections without trade agreements blocking them. The constraint redistributes regulatory authority from capital to democratic institutions.
% ABSENT_VOICES: Investor interests are structurally excluded from formal decision-making under this reading—they would argue for investor-state dispute mechanisms and regulatory harmonization requirements, but those mechanisms do not exist in this reading's framework. Developing-country governments with lower standards capacity and capital-dependent economies would contest the reading as naïve about power asymmetries (capital exit threat).
% DISAPPEARANCE_RATIONALE: If this constraint—the rule that trade agreements do not override domestic regulatory authority—disappeared and were replaced by mandatory harmonization or investor-state supremacy, domestic environmental and labor standards would converge downward to the lowest-standard jurisdiction (regulatory race to the bottom); multinational capital would gain veto power over legislation; and democratic governments would lose independent authority to protect health, labor, and environment. The institutional landscape would fundamentally shift.
% FOUNDING_PROBLEM: Post-WWII trade system needed rules to prevent tit-for-tat tariff wars while preserving each nation's right to govern its own social priorities (labor dignity, environmental protection, public health)—the dilemma that legitimate domestic regulation cannot be pre-subordinated to capital mobility.
% FOUNDING_PROBLEM_CORROBORATION: Labor unions, environmental organizations, and academic economists studying regulatory capacity argue the founding problem remains live and urgent: capital mobility creates a race-to-the-bottom dynamic that erodes labor and environmental protections unless states retain regulatory authority. Multinational corporations and free-trade economists contest that the problem exists at all, arguing harmonization benefits all parties. Legislative testimony from labor-exporting developing countries attests to the live threat; conflicting testimony from capital-exporting developed economies disputes it.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is LOW (0.28 at interval end) because under this reading the constraint does not extract from identifiable victims to identifiable beneficiaries—instead it REDISTRIBUTES AUTHORITY from multinational capital to democratic institutions. No party is systematically damaged by the arrangement itself; rather, the arrangement prevents one party (capital) from using trade rules to override another party's (democratic publics') regulatory preferences. Suppression is very LOW (0.15) because the arrangement does not coercively prevent exit or alternatives—multinational firms can exit to lower-regulation jurisdictions; they simply cannot use trade law to block domestic regulation at home. Theater ratio is LOW-MODERATE (0.22) because the sovereignty-assertion function is genuine (domestic agencies do retain authority in practice) but increasingly performative as capital-exit threat and regulatory-race-to-the-bottom dynamics create de facto harmonization pressure even without formal treaty override. The measurement series shows extractiveness and theater rising modestly over the interval (NAFTA-to-2024) as capital mobility increased and exit-threat pressure grew, even though formal trade rules did not change—the constraint's functional content weakened slightly as market forces replicated harmonization pressure that formal treaty language did not impose. This is the constraint's actual operation under this reading, not a judgment on whether the reading is correct.
 *
 * PERSPECTIVAL GAP:
 *   From the standpoint of domestic regulatory agencies and labor/environmental movements, this reading instantiates a genuine coordination mechanism with minimal extraction—they gain authority and face no coercive override. From multinational capital's standpoint, the same arrangement is extractive: it denies them the regulatory certainty and harmonization they seek, forcing compliance-cost divergence and exit decisions. From the engine's computation: agencies and advocates sit near d=0 (beneficiary); capital sits near d=1 (full target). The reading's claim/metric independence is deliberate: the constraint is CLAIMED as rope (genuine coordination) while authored metrics describe low but measurable extraction from capital—the engine computes the divergence; the divergence IS the signal that the sovereignty-primacy reading must actively prevent capital dominance to persist, confirming the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (domestic agencies, labor, environment, health): these parties gain authority to set standards and face no trade-override mechanism, so directionality is low (d near 0). They benefit from the arrangement's existence and would lose authority if the constraint were removed. Payers (multinational capital): this party faces compliance-cost divergence, exit threats, and regulatory uncertainty without the option to use trade mechanisms to block domestic regulation—directionality is high (d near 1). Capital pays the cost of regulatory diversity and cannot leverage trade rules to harmonize. The small-domestic-enterprises stakeholder is a secondary beneficiary: they can compete on domestic regulatory terms without foreign capital using trade rules to weaken those terms. Trade signatories occupy the agenda-setter role but are also constrained: they can exit NAFTA but cannot unilaterally modify its text; once signed, they face retaliation if they breach it.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty-primacy reading avoids the mandatrophy trap by grounding its justification in LIVE DEMOCRATIC PRACTICE (legislatures and regulatory agencies actively setting standards, courts defending them, communities advocating for tighter protections). The founding problem (how to coordinate trade while preserving regulatory diversity) remains live because capital mobility continues to create race-to-the-bottom pressure. If the founding problem were declared dead, the constraint would be a zombie: trade rules existing to solve a problem that no longer exists. However, contestation is high: capital-supremacy readers claim the problem is illusory (harmonization benefits everyone), while embedded-liberalism readers claim the problem is partially solved through non-discrimination + flexibility. The constraint's persistence depends on ongoing democratic insistence that regulatory authority remain domestic, which is why theater_ratio rises with capital mobility—increasingly, the constraint is performed (governments assert authority they know capital-exit pressure erodes) rather than functionally preventing capital dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_authority_vs_capital_exit_threat,
    'Does formal sovereignty over regulatory authority translate to functional regulatory autonomy when capital can exit to lower-regulation jurisdictions?',
    'Long-term empirical study of regulatory divergence trajectories in trading blocs: do standards converge to minimums despite formal sovereignty (indicating exit-threat capture) or do they remain diverse (indicating sovereignty remains functionally operative)? Jurisdictional pairs with different political commitments (US labor standards vs. Mexico labor standards post-NAFTA) provide natural experiments.',
    'If capital-exit threat causes de facto harmonization despite formal sovereignty, the constraint is performing theater (asserting authority while functional autonomy erodes). If standards diverge freely, sovereignty remains operative and theater_ratio should fall, not rise. This omega directly resolves whether theater_ratio rise is measurement of functional degradation or normal performance-cost of a sovereignty-assertion mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_authority_vs_capital_exit_threat, empirical, 'Whether regulatory sovereignty survives capital-exit pressure in practice or erodes into de facto harmonization.').

omega_variable(
    reading_foreclosure_capital_supremacy,
    'Does the sovereignty-primacy reading''s core premise (regulatory authority is not subordinate to trade text) logically foreclose the capital-supremacy reading''s core premise (trade text mandates regulatory harmonization), or do the readings merely occupy different institutional seats?',
    'Constitutional interpretation: can one signatory state enforce the sovereignty-primacy reading while another enforces capital-supremacy (different courts, different governments applying different readings to the same text)? If yes, the readings coexist (neither forecloses the other). If the text itself is unambiguous, one forecloses the other. But NAFTA text is ambiguous: it contains both sovereignty-reserving language and investor-protection language, so readings coexist until litigation resolves the ambiguity in a specific case.',
    'If readings coexist (as the prompt assumes), this reading does not foreclose capital-supremacy; they are live alternative commitments held by different institutional actors. If one forecloses the other (rare for statutory text), the relationship is ''forecloses'' rather than ''coexists_with''. Coexistence is the more defensible assignment: the kernel is genuinely contested because the text permits multiple readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_capital_supremacy, conceptual, 'Whether this reading logically forecloses the capital-supremacy reading or whether both remain live interpretations of the same treaty text.').

omega_variable(
    suppression_mechanism_in_treaty_context,
    'Is the low measured suppression (0.15) capturing the true constraint structure, or is suppression internalized through capital''s normalization of regulatory diversity as cost-of-doing-business rather than as institutional suppression?',
    'Post-exit narrative analysis: when firms relocate production to escape regulatory standards, do they describe the decision as voluntary cost-calculation (low suppression, high exit freedom) or as coerced by regulatory burden (high suppression, constrained exit)? Survey data from multinational capital on perceived autonomy in regulatory decisions would disambiguate.',
    'If suppression is internalized (firms treat regulatory constraints as natural facts, not coercive impositions), the effective constraint structure is more suppressive than authored. If exit is genuinely voluntary, suppression_requirement accurately reflects a low-coercion regime. This omega addresses whether measurement captures the constraint as an external force or as an internalized expectation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_in_treaty_context, empirical, 'Whether suppression of capital''s alternatives is structural (measurable barriers) or internalized (normalized cost) under the sovereignty-primacy reading.').

omega_variable(
    embedded_liberalism_influence_pathway,
    'Does the sovereignty-primacy reading influence the embedded-liberalism reading by establishing a baseline negotiating position, or do the readings develop independently in institutional silos (labor courts vs. trade courts, environmental advocates vs. investor arbitrators)?',
    'Historical institutional analysis: did the successful invocation of sovereignty-primacy defenses (e.g., Canada health care, US environmental law) in early NAFTA disputes establish precedent that embedded-liberalism advocates later cited to argue for standards-compatibility, or did embedded-liberalism emerge as a separate legitimacy claim?',
    'If sovereignty-primacy influences embedded-liberalism (creating a ladder of authority claims), the relationship is ''influences'' and the sovereignty reading creates downstream structural pressure on the embedded reading''s justification. If readings develop independently, the relationship is ''coexists_with''. Influence would mean this reading establishes a default position that others must accommodate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embedded_liberalism_influence_pathway, empirical, 'Whether sovereignty-primacy reading influences or coexists with embedded-liberalism reading in institutional practice.').

omega_variable(
    reading_kernel_ambiguity_in_nafta_text,
    'Does NAFTA text itself contain genuine ambiguity that permits multiple readings, or is the sovereignty-primacy reading a heroic reinterpretation imposed against the text''s original investor-protection intent?',
    'Textual analysis: examine NAFTA Chapter 11 (investor protections) alongside Chapter 21 (dispute resolution) and preamble sovereignty language. Survey negotiating history (notes, testimony from negotiators) about intent. If negotiators explicitly intended investor override authority, the text is not genuinely ambiguous—this reading is reinterpretation, not interpretation. If negotiators expressed conflicting intents or the language is facially ambiguous, readings are coequal.',
    'If NAFTA text is genuinely ambiguous, all three readings (sovereignty-primacy, capital-supremacy, embedded-liberalism) are defensible interpretations and coexist legitimately. If capital-supremacy was the clear original intent, sovereignty-primacy is a drift reading established through advocacy against textual intent—it remains live, but the reading''s legitimacy grounds shift from ''natural interpretation'' to ''hard-won reinterpretation.'' This affects the axiom_status judgment: should ''trade_text_is_subordinate_to_domestic_law'' be holdable (live reading of ambiguous text) or overridden (reinterpretation against clear intent)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity_in_nafta_text, empirical, 'Whether the sovereignty-primacy reading is a natural interpretation of ambiguous treaty text or a reinterpretation against the negotiators'' original investor-protection intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1994, 0.12).
narrative_ontology:measurement_basis(naft_tr_t1994, projected).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement_basis(naft_tr_t2000, observed).
narrative_ontology:measurement(naft_tr_t2008, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2008, 0.19).
narrative_ontology:measurement_basis(naft_tr_t2008, observed).
narrative_ontology:measurement(naft_tr_t2016, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement_basis(naft_tr_t2016, observed).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(naft_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.18).
narrative_ontology:measurement_basis(naft_be_t1994, projected).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement_basis(naft_be_t2000, observed).
narrative_ontology:measurement(naft_be_t2008, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2008, 0.26).
narrative_ontology:measurement_basis(naft_be_t2008, observed).
narrative_ontology:measurement(naft_be_t2016, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2016, 0.28).
narrative_ontology:measurement_basis(naft_be_t2016, observed).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2024, 0.28).
narrative_ontology:measurement_basis(naft_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 1994, 0.08).
narrative_ontology:measurement_basis(naft_su_t1994, projected).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2000, 0.11).
narrative_ontology:measurement_basis(naft_su_t2000, observed).
narrative_ontology:measurement(naft_su_t2008, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2008, 0.13).
narrative_ontology:measurement_basis(naft_su_t2008, observed).
narrative_ontology:measurement(naft_su_t2016, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2016, 0.15).
narrative_ontology:measurement_basis(naft_su_t2016, observed).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2024, 0.15).
narrative_ontology:measurement_basis(naft_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% The NAFTA jurisdictional boundary is a contested kernel with three structurally incompatible readings: (1) capital-supremacy reading treats treaty text as supreme law overriding domestic regulation; (2) embedded-liberalism reading treats treaty as market-access framework compatible with non-discriminatory standards; (3) sovereignty-primacy reading (this file) treats treaty as coordination mechanism subordinate to sovereign regulatory authority. Each reading instantiates a distinct constraint with distinct ε, beneficiary/victim structures, and classifications. They are not the same constraint viewed from different angles—they have fundamentally different extractiveness and different victim sets. The three readings coexist across institutional actors (capital courts vs. labor legislatures, investor arbitrators vs. environmental agencies) without resolution. Network links establish the family: all three readings claim kinship with the same kernel; they influence and coexist with each other in institutional practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
