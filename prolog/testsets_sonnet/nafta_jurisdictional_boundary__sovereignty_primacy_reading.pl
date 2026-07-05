% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA/USMCA Jurisdictional Boundary — Sovereignty Primacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-primacy reading of the
 *   NAFTA/USMCA jurisdictional boundary kernel: the treaty text functions as
 *   a coordination mechanism that domestic legal systems treat as subordinate
 *   to sovereign lawmaking authority. Under this reading, treaty obligations
 *   enter a state's compliance-cost calculus but do not override domestic
 *   regulatory choices on labor, environmental, or health standards —
 *   implementing legislation, not the treaty text itself, is what creates
 *   domestic legal effect, and legislatures retain full authority to exceed,
 *   amend, or decline to harmonize with treaty floors. This is a genuinely
 *   different constraint from the capital-supremacy reading (where treaty
 *   text is asserted as overriding domestic law) and the embedded-liberalism
 *   reading (where trade obligations and domestic policy space are balanced
 *   as co-equal, mutually conditioning commitments) — each reading has its
 *   own epsilon, its own beneficiary/victim structure, and is authored as a
 *   separate story per the epsilon-invariance principle. This reading is the
 *   one most protective of regulatory autonomy and correspondingly shows the
 *   lowest extraction and suppression of the three.
 *
 * KEY AGENTS:
 *   - domestic_regulatory_agencies: agenda_setter (institutional/analytical) — sets and enforces standards, treats treaty as non-overriding reference
 *   - national_legislatures: beneficiary (institutional/arbitrage) — retains full lawmaking authority
 *   - labor_and_environmental_advocacy_groups: beneficiary/observer (organized/mobile) — relies on domestic channels being primary
 *   - exporting_firms: payer (powerful/constrained) — bears voluntary compliance costs across heterogeneous domestic regimes
 *   - investor_state_dispute_tribunals: excluded (institutional/analytical) — sidelined from domestically-binding enforcement under this reading
 *   - trading_partner_governments: observer (institutional/analytical) — monitors compliance without domestic enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA/USMCA Jurisdictional Boundary — Sovereignty Primacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'c7df13f6-91d2-49bb-9c2f-c8c534b725f0').
narrative_ontology:cs_kernel_codification('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', fixed_text).
narrative_ontology:cs_authority_grounding('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', practice).
narrative_ontology:cs_interpretation_layer_present('c7df13f6-91d2-49bb-9c2f-c8c534b725f0').
narrative_ontology:cs_reading_relation('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', foundational, domestic_implementing_legislation_is_sole_binding_instrument).
narrative_ontology:cs_axiom_status(domestic_implementing_legislation_is_sole_binding_instrument, holdable).
narrative_ontology:cs_axiom_grounding('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', domestic_implementing_legislation_is_sole_binding_instrument, conventional).
narrative_ontology:cs_axiom('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', foundational, treaty_text_cannot_preempt_undelegated_domestic_regulatory_authority).
narrative_ontology:cs_axiom_status(treaty_text_cannot_preempt_undelegated_domestic_regulatory_authority, holdable).
narrative_ontology:cs_axiom_grounding('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', treaty_text_cannot_preempt_undelegated_domestic_regulatory_authority, deontological).
narrative_ontology:cs_reference_frame('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', dualist_constitutional_incorporation_baseline).
narrative_ontology:cs_drift_state('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', post_usmca_dispute_panel_expansion, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c7df13f6-91d2-49bb-9c2f-c8c534b725f0', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, national_legislatures).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_and_environmental_advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, exporting_firms).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, state_regulatory_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, dualist_treaty_incorporation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces labor, environmental, and health standards within its territory. Treats treaty text as a coordination reference that must be domestically implemented through legislation before it has any binding force; can raise standards above treaty floors at any time without triggering automatic treaty override. Experiences the trade agreement as one input among many into rulemaking, not as supreme law.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Retains full authority to pass, amend, or repeal domestic law regardless of treaty commitments; ratification and implementing statutes remain the mechanism by which treaty text acquires domestic legal effect. Benefits from the coordination gains of predictable market access without ceding lawmaking authority.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, national_legislatures, beneficiary,
    institutional, generational, arbitrage, national).

% Lobbies domestic legislatures and agencies directly, secure in the premise that treaty text cannot be invoked to strike down or preempt domestic protective standards. Uses domestic political and judicial channels rather than international dispute panels as the primary venue for contesting regulatory outcomes.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_and_environmental_advocacy_groups, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_and_environmental_advocacy_groups, observer).

% Bears the voluntary compliance costs of meeting whatever domestic standard applies in each jurisdiction it sells into; cannot use treaty text to compel harmonization downward or to force a change in domestic law. Views this reading as commercially inconvenient because it preserves regulatory heterogeneity as a cost of doing cross-border business.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, exporting_firms, payer,
    powerful, biographical, constrained, continental).

% Under this reading, tribunal findings on regulatory takings or discriminatory treatment carry persuasive but not domestically overriding force; a state can decline to alter its law even after an adverse award, treating the award as a trade-relations cost rather than a legal command. Tribunals would object that this collapses the deterrent function of investor protections, but their voice is structurally external to how domestic courts characterize treaty status.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, investor_state_dispute_tribunals, excluded,
    institutional, biographical, analytical, continental).

% Negotiates and monitors treaty compliance but has no domestic enforcement power inside the other party's legal system; can pursue state-to-state consultation or retaliation but cannot compel a change in the other state's internal law. Watches domestic implementation choices as the real site of treaty effect.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trading_partner_governments, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared reference framework — tariff schedules, market-access commitments, dispute consultation procedures — that lets trading states coordinate expectations about cross-border commerce without requiring any state to cede its domestic lawmaking authority.
% TRANSFER_FUNCTION: Moves predictability and market-access assurance between trading partners' firms and governments; does not move regulatory authority itself, which this reading holds remains entirely domestic. Compliance costs are borne voluntarily by firms seeking to trade, not extracted by treaty compulsion.
% ABSENT_VOICES: Investor-state dispute tribunals and the capital interests that rely on supremacy-style enforcement are structurally sidelined in this reading — they would argue that treating awards as non-binding guts the deterrent function that makes the treaty valuable to investors at all, but domestic courts adjudicating under a dualist framework are not required to hear that objection as controlling.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primacy characterization of the treaty vanished overnight and were replaced by a supremacy reading, domestic regulatory agencies would face a fundamentally different legal environment — implementing legislation could be preempted, and standards could be challenged as treaty violations with binding domestic effect. Whether this counts as 'the world rearranging' is itself contested between the readings: supremacy-reading proponents say almost nothing changes because the treaty's substantive obligations are unchanged; sovereignty-primacy proponents say everything changes because the enforceability chain is different.
% FOUNDING_PROBLEM: Trading states needed a way to reduce tariff and non-tariff barriers and coordinate market access without any party being required to surrender the constitutional allocation of lawmaking authority between international commitments and domestic legislatures.
% FOUNDING_PROBLEM_CORROBORATION: Domestic constitutional scholars and dualist-tradition jurists (outside the treaty's own text and outside investor-side legal counsel) attest that most signatory states' own constitutional doctrine requires implementing legislation for treaty obligations to bind domestically, corroborating that the founding problem — coordination without loss of sovereignty — remains the operative legal premise in domestic courts, even though investor-state tribunals and capital-mobility advocates contest this characterization from outside domestic courts.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, contested).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is low (0.22) because under this reading no party can compel a change in another state's domestic law through the treaty mechanism itself — the only 'extraction' is the voluntary compliance cost firms bear to access foreign markets, which is a cost of trade, not of subordination. Suppression is low (0.18) because states retain unimpaired exit from any specific standard the treaty might otherwise have implied; nothing forces harmonization. Theater ratio is moderate and rising slowly (0.20 to 0.30) because dispute-panel processes and consultation mechanisms increasingly perform the appearance of binding adjudication even as domestic courts continue to treat findings as persuasive rather than controlling — a genuine but modest theatrical drift as institutional dispute machinery accretes without corresponding domestic bindingness. Accessibility collapse is low (0.25): states retain real, exercised alternatives to treaty-implied standards. Resistance is moderate-high (0.55) because capital interests and investor-state tribunal proponents actively contest this reading in academic, diplomatic, and litigation arenas — the sovereignty-primacy characterization is not passively accepted, it is actively defended against rival readings.
 *
 * PERSPECTIVAL GAP:
 *   Domestic regulatory agencies and legislatures compute this constraint as low-extraction coordination because their lived experience is retained authority. Exporting firms compute a more burdensome experience because the same heterogeneity that preserves state sovereignty is, from their seat, an unrationalized patchwork of compliance costs. Investor-state tribunals and capital-mobility advocates would compute this reading itself as illegitimate or as failing to capture the treaty's real operation — but that disagreement is precisely the kernel contest this story is one reading of, not a perspectival gap within a single constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Domestic regulatory agencies and national legislatures sit near the beneficiary end: the coordination reading preserves their authority intact while still capturing trade-facilitation gains. Labor and environmental advocacy groups also benefit because their preferred venue (domestic legislative and judicial process) remains primary and is not preemptable by treaty claims. Exporting firms are the nearest thing to a payer, but the cost is voluntary compliance with market-access conditions, not coerced extraction — their d sits closer to symmetric than to full-target. Investor-state tribunals are excluded rather than victimized: their institutional function is diminished under this reading, but no party is extracting from them, so they do not qualify as victims under base_properties despite bearing structural disadvantage relative to the sibling readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating trade without surrendering domestic lawmaking authority — remains live in the sense that states continue to legislate independently of treaty text and treat implementing statutes as the operative legal instrument. This prevents mislabeling the arrangement as pure extraction (a capital-supremacy critique) because the structural data show states genuinely retaining and exercising divergent regulatory choices post-ratification. It equally prevents mislabeling it as costless pure coordination, since real compliance costs and real contestation (resistance = 0.55) persist — this is a Rope with active, unresolved contest over its own characterization, not a settled Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dualist_vs_monist_incorporation_ambiguity,
    'Does the treaty''s actual legal status depend on each signatory''s domestic constitutional tradition (dualist vs. monist incorporation), making ''the'' jurisdictional boundary genuinely different across signatory states rather than a single fact about the treaty?',
    'Comparative constitutional analysis of how each USMCA signatory''s domestic courts have actually treated treaty-implementing statutes versus treaty text directly in adjudicated disputes.',
    'If jurisdictional status varies materially by signatory, this reading may only hold cleanly for dualist-tradition states (e.g., historically for Canada and the US in certain domains) and not uniformly across the agreement, which would argue for further decomposition rather than a single sovereignty-primacy story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dualist_vs_monist_incorporation_ambiguity, conceptual, 'Whether dualist incorporation doctrine is uniform enough across signatories to support one unified sovereignty-primacy reading.').

omega_variable(
    investor_state_award_compliance_pattern,
    'When investor-state tribunals have issued awards against a signatory, has domestic law actually been altered in practice, or has the state treated the award as non-binding as this reading predicts?',
    'Empirical review of post-award domestic legislative and regulatory responses across USMCA''s investor-state dispute history.',
    'A pattern of domestic law changing in response to awards would undercut this reading''s core claim that treaty findings are merely persuasive; a pattern of non-compliance or negotiated settlement without domestic law change would corroborate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investor_state_award_compliance_pattern, empirical, 'Whether real-world award compliance behavior matches the sovereignty-primacy prediction of non-binding domestic effect.').

omega_variable(
    kernel_framing_selection_rationale,
    'Is the sovereignty-primacy characterization the dominant judicial reading, or is it one of three contested framings with no settled majority position among domestic courts and international law scholars?',
    'Systematic review of domestic appellate court decisions and leading international trade law scholarship characterizing treaty-domestic law relationships under USMCA/NAFTA.',
    'If sovereignty-primacy is a minority or declining position relative to embedded-liberalism or capital-supremacy readings in actual adjudication, this story''s claimed_type and metrics describe a contested aspirational reading rather than the dominant operative one — this would not change this story''s own epsilon (each reading keeps its own epsilon per the invariance principle) but would inform how much interpretive weight this reading should carry in downstream network analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection_rationale, conceptual, 'How dominant this reading actually is among the three contested framings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1994, 0.2).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2006, 0.24).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2012, 0.26).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.15).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2000, 0.17).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2006, 0.19).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2012, 0.2).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2018, 0.21).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2024, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nafta_jurisdictional_boundary__sovereignty_primacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'NAFTA/USMCA jurisdictional boundary' per the epsilon-invariance principle. The three readings — sovereignty_primacy_reading (this story, epsilon=0.22), capital_supremacy_reading (expected substantially higher epsilon, treaty as overriding law), and embedded_liberalism_reading (expected intermediate epsilon, balanced framework) — are linked as a kernel family via affects_constraints. Each carries its own claimed_type, metrics, stakeholders, and beneficiary/victim structure; none is a measurement of the others under a different observable. The kernel itself (nafta_jurisdictional_boundary) is the contested commitment; these three files are the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
