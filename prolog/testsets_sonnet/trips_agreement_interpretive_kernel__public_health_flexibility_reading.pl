% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__public_health_flexibility_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibility Reading (Compulsory Licensing / Parallel Import)
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the public-health-flexibility reading of the
 *   TRIPS interpretive kernel: the claim that the TRIPS text embeds broad
 *   compulsory licensing (Article 31) and parallel import (Article 6
 *   exhaustion) flexibilities intended to protect public health access, as
 *   affirmed and clarified by the 2001 Doha Declaration and the 2003
 *   Paragraph 6 waiver mechanism. This is a distinct constraint from the
 *   sibling strong-exclusivity reading, which holds that TRIPS mandates high
 *   uniform patent protection with only narrow flexibilities — the two
 *   readings produce different beneficiary/victim sets and different epsilon
 *   values from the same underlying text, so they are authored as separate
 *   stories per the ε-invariance principle rather than as one story with a
 *   measurement parameter. The dispute-settlement-interpretive-authority
 *   reading is a third sibling addressing who adjudicates, orthogonal to what
 *   the substantive content is.
 *
 * KEY AGENTS:
 *   - national_health_ministries: agenda_setter (institutional/constrained) — invoke and administer the flexibility
 *   - generic_pharmaceutical_manufacturers: beneficiary (organized/mobile) — gain production and export rights
 *   - low_income_patients: beneficiary (powerless/trapped) — gain medicine access, no direct voice
 *   - originator_pharmaceutical_patent_holders: payer (powerful/constrained) — lose exclusivity and pricing power
 *   - home_state_trade_representatives: excluded/payer (institutional/arbitrage) — advocate for the sibling reading from outside this constraint
 *   - wto_dispute_settlement_body: observer (institutional/analytical) — polices the boundary dispute by dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.28).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.35).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Reading (Compulsory Licensing / Parallel Import)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '708583ca-dc23-48a6-a76b-a068548fef37').
narrative_ontology:cs_kernel_codification('708583ca-dc23-48a6-a76b-a068548fef37', fixed_text).
narrative_ontology:cs_authority_grounding('708583ca-dc23-48a6-a76b-a068548fef37', distributed).
narrative_ontology:cs_reading_relation('708583ca-dc23-48a6-a76b-a068548fef37', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('708583ca-dc23-48a6-a76b-a068548fef37', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('708583ca-dc23-48a6-a76b-a068548fef37', foundational, public_health_necessity_overrides_patent_exclusivity).
narrative_ontology:cs_axiom_status(public_health_necessity_overrides_patent_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('708583ca-dc23-48a6-a76b-a068548fef37', public_health_necessity_overrides_patent_exclusivity, deontological).
narrative_ontology:cs_axiom('708583ca-dc23-48a6-a76b-a068548fef37', secondary, doha_declaration_clarifies_rather_than_amends_original_text).
narrative_ontology:cs_axiom_status(doha_declaration_clarifies_rather_than_amends_original_text, holdable).
narrative_ontology:cs_axiom_grounding('708583ca-dc23-48a6-a76b-a068548fef37', doha_declaration_clarifies_rather_than_amends_original_text, conventional).
narrative_ontology:cs_reference_frame('708583ca-dc23-48a6-a76b-a068548fef37', doha_declaration_clarified_flexibility_baseline).
narrative_ontology:cs_drift_state('708583ca-dc23-48a6-a76b-a068548fef37', post_covid19_vaccine_access_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('708583ca-dc23-48a6-a76b-a068548fef37', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, national_health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_income_patients).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, home_state_trade_representatives).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, doha_declaration_public_health_primacy).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, sovereign_regulatory_flexibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke Article 31 compulsory licensing and Article 6 parallel import exhaustion provisions to authorize domestic or imported generic production during health emergencies or for essential medicines. They administer the notification and negotiation process with patent holders, set the terms of 'adequate remuneration,' and face diplomatic and trade pressure when they invoke these flexibilities aggressively.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, national_health_ministries, agenda_setter,
    institutional, generational, constrained, national).

% Gain legal cover and export markets under compulsory licenses and the Paragraph 6 mechanism, producing lower-cost versions of patented drugs for domestic use or export to countries lacking manufacturing capacity. Their business model depends on the flexibility reading remaining the operative interpretation; they lobby WTO members and file amicus positions in disputes to keep the reading broad.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Gain access to essential medicines (antiretrovirals, cancer therapies, vaccines) at fractions of originator prices when compulsory licenses are issued. They have no direct voice in TRIPS negotiations or dispute panels; their access depends entirely on whether their government successfully invokes and defends the flexibility.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_income_patients, beneficiary,
    powerless, biographical, trapped, national).

% Hold the patents subject to compulsory licensing and see market exclusivity eroded and prices compressed in jurisdictions that invoke the flexibility. They lobby for the narrow (strong exclusivity) reading through bilateral trade agreements with TRIPS-plus provisions, and litigate or pressure via their home governments' trade representatives when licenses are issued against their portfolios.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders, payer,
    powerful, biographical, constrained, global).

% Represent originator-holding states' commercial interests in bilateral and WTO fora, pushing back against broad compulsory licensing through trade pressure and TRIPS-plus bilateral agreements. Their preferred (strong exclusivity) reading is a sibling constraint, not this one — they are structurally positioned against this reading's operation without being a formal party inside it.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, home_state_trade_representatives, excluded,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, home_state_trade_representatives, payer).

% Adjudicates disputes over whether specific invocations of Article 31 or parallel import exhaustion comply with TRIPS text, drawing on the Doha Declaration's affirmation of public health primacy. Its rulings determine, dispute by dispute, whether this reading holds or narrows in practice — it does not originate the reading but polices its boundaries.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_settlement_body, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared understanding among WTO members that intellectual property protection is subordinate to public health necessity in defined emergency and access contexts, allowing states to authorize generic production or importation without first securing patent holder consent, provided procedural safeguards (notification, remuneration, predominant domestic-market supply or Paragraph 6 export licensing) are followed.
% TRANSFER_FUNCTION: Moves manufacturing and market access rights from patent holders to generic producers and the governments authorizing them, and moves consumer surplus from originator pricing structures to patients and health systems, in exchange for a remuneration payment set unilaterally by the licensing state rather than negotiated at market rates.
% ABSENT_VOICES: Patients in the poorest countries with the least domestic pharmaceutical manufacturing capacity are the intended beneficiaries but have essentially no voice in whether their government successfully invokes the flexibility, negotiates favorable license terms, or withstands trade retaliation threats; they are also invisible in the WTO dispute process itself, which runs state-to-state.
% DISAPPEARANCE_RATIONALE: If this reading collapsed and the strong exclusivity reading became the operative interpretation across the WTO membership, compulsory licensing would become legally fraught or foreclosed in practice, generic manufacturers exporting under Paragraph 6 would lose their legal basis, and access programs built around emergency licensing (HIV/AIDS antiretroviral rollouts, COVID-19 vaccine manufacturing waivers) would have to be renegotiated from a much weaker position or abandoned; health ministries would lose a primary lever for price negotiation with originators.
% FOUNDING_PROBLEM: The 1994 TRIPS Agreement's uniform 20-year patent minimum threatened to block generic production of essential medicines in developing countries at the height of the HIV/AIDS crisis, prompting the 2001 Doha Declaration to affirm that TRIPS 'can and should be interpreted and implemented in a manner supportive of WTO members' right to protect public health' and to clarify the scope of Article 31 compulsory licensing.
% FOUNDING_PROBLEM_CORROBORATION: UNAIDS, WHO, and Médecins Sans Frontières — none of whom hold patents or manufacture generics — have documented that the founding public-health-access problem remains substantially live (persistent gaps in vaccine and treatment access, most visibly during COVID-19); originator pharmaceutical trade associations and several home-state trade representatives dispute this, arguing that voluntary licensing and tiered pricing have superseded the need for the flexibility's broad construction — that dispute is corroborated from outside the beneficiary set by academic health-law scholarship (e.g. published TRIPS Council submissions from non-implicated WTO observer delegations) documenting continued reliance on the flexibility as recently as 2020-2022 vaccine access negotiations.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).
:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28) because the flexibility reading, when successfully invoked, transfers value from patent holders to generic producers and patients through a legally sanctioned mechanism with remuneration built in — it is not costless to patent holders but it is bounded and procedurally constrained, unlike outright infringement. Suppression is moderate (0.35) reflecting the real diplomatic and trade pressure originator-holding states apply against ministries that invoke the flexibility aggressively (e.g. Section 301 threats), which functions as an active deterrent even though the legal right exists on paper. Theater ratio is low-moderate (0.22) — the flexibility is genuinely exercised (South Africa, Thailand, Brazil, India's Paragraph 6 exports, COVID-era TRIPS waiver debates), not merely symbolic, though rhetorical invocation of the Doha Declaration sometimes exceeds actual licensing activity. Accessibility collapse is low (0.30): alternative interpretive readings remain fully live and contested, which is precisely why this is a kernel-reading story rather than a settled fact. Resistance is substantial (0.55) because originator interests and their home governments actively contest every invocation.
 *
 * PERSPECTIVAL GAP:
 *   From the health ministry and generic manufacturer seats, this reading operates as a rope-like coordination mechanism solving a genuine access problem with a legally bounded remuneration mechanism. From the originator patent holder seat, the same structure operates as an enforced transfer that erodes the value of a granted property right through unilateral government action. The engine should compute this divergence from the beneficiary/victim/enforcement structure rather than from any story-level reconciliation — that is the seat divergence the tangled_rope classification is meant to register: genuine coordination (public health access) and asymmetric extraction (patent value erosion) coexisting in the same textual provision, held in place by active enforcement (dispute panels, WTO Council notification requirements, TRIPS-plus bilateral counter-pressure).
 *
 * DIRECTIONALITY LOGIC:
 *   Health ministries sit as agenda_setters who administer the mechanism but do not personally capture value from it — the gains flow through to generic manufacturers (who profit from expanded market access) and to patients (who benefit from lower prices, though as powerless/trapped agents they cannot advocate for themselves). Originator patent holders are the clear structural target: their exclusivity and pricing power are the thing being eroded, and their exit options are constrained because they cannot simply withdraw a drug from a market invoking compulsory licensing without reputational and regulatory cost elsewhere. Home-state trade representatives are excluded from this constraint's internal operation — they are the seat structurally aligned with the sibling reading, present in the broader kernel contest but not a party to this reading's coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — patent-driven barriers to essential medicine access during the HIV/AIDS crisis — remains partially live (COVID-19 vaccine access gaps corroborate this from outside the beneficiary set) even as originator interests argue voluntary licensing has superseded the need for broad compulsory licensing. This is authored as contested rather than resolved: the flexibility reading is not a zombie mandate propped up by inertia, but neither is it uncontested settled law. Classifying it as tangled_rope rather than snare or pure rope avoids two mislabeling errors: treating it as pure extraction (ignoring the genuine, still-operative public health coordination function) or treating it as costless pure coordination (ignoring that originator patent holders bear real, identifiable, enforced costs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_flexibility_vs_exclusivity,
    'Is the public-health-flexibility reading or the strong-exclusivity reading the operative interpretation of TRIPS Articles 31 and 6 across the WTO membership, and does that operative status vary by member state and by era (pre- vs. post-Doha, pre- vs. post-COVID)?',
    'Track the pattern of WTO dispute panel rulings and TRIPS Council decisions over time: a rising rate of panels upholding broad compulsory licensing invocations without penalty would indicate the flexibility reading is becoming dominant; a rising rate of TRIPS-plus bilateral agreements narrowing flexibility in practice would indicate the exclusivity reading is displacing it de facto even where not de jure.',
    'If the exclusivity reading is displacing this one in practice (via bilateral TRIPS-plus agreements bypassing the multilateral text), the effective extraction protection this reading provides to beneficiaries is lower than the formal legal text suggests, and the constraint should be understood as eroding rather than stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_flexibility_vs_exclusivity, empirical, 'Whether the flexibility reading or its exclusivity sibling is the operative interpretation in practice, and how that balance is shifting.').

omega_variable(
    remuneration_adequacy_ambiguity,
    'Is the ''adequate remuneration'' that licensing states set for compulsory licenses genuinely calibrated to compensate patent holders'' foregone value, or is it systematically set low enough that the flexibility functions as a disguised expropriation rather than a bounded, compensated transfer?',
    'Comparative analysis of remuneration rates set across multiple compulsory licensing episodes (Thailand''s 2006-2008 licenses, Brazil''s 2007 efavirenz license, India''s Paragraph 6 exports) against counterfactual market-negotiated royalty rates for comparable drugs.',
    'If remuneration is systematically low, the extractiveness score understates the actual transfer from patent holders and the tangled_rope classification''s extraction component is stronger than currently authored; if remuneration approximates market rates, the coordination framing is more fully justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remuneration_adequacy_ambiguity, empirical, 'Whether compulsory license remuneration terms are genuinely compensatory or systematically below market value.').

omega_variable(
    reading_selection_signal,
    'The choice to author this reading (rather than the strong-exclusivity reading) as the primary story for the ''TRIPS embeds broad flexibilities'' claim was guided by the Doha Declaration''s explicit textual affirmation and by documented WTO Council practice since 2001 — but the underlying treaty text itself is genuinely ambiguous on the scope of Article 31''s grounds for compulsory licensing (''national emergency or other circumstances of extreme urgency'' vs. general public interest). Does the textual ambiguity itself favor one reading over the other absent the Doha gloss?',
    'Textual and drafting-history analysis of Article 31 independent of the 2001 Doha Declaration''s interpretive gloss, comparing to the negotiating record (TRIPS Uruguay Round travaux préparatoires).',
    'If the pre-Doha text is genuinely closer to the strong-exclusivity reading and Doha represents a subsequent political reinterpretation rather than a clarification of original intent, this reading''s claim to represent ''what TRIPS embeds'' is weaker than authored, and the constraint is better understood as a post-hoc political achievement layered onto a more restrictive original text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_signal, conceptual, 'Whether the flexibility reading reflects original treaty intent or a subsequent political reinterpretation (Doha Declaration) layered onto more restrictive original text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2001, 0.12).
narrative_ontology:measurement(trip_tr_t2003, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2003, 0.14).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(trip_tr_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2001, 0.15).
narrative_ontology:measurement(trip_be_t2003, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2003, 0.18).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2010, 0.2).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2020, 0.26).
narrative_ontology:measurement(trip_be_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(trip_su_t2003, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2003, 0.32).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2010, 0.33).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(trip_su_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the trips_agreement_interpretive_kernel. strong_exclusivity_reading claims TRIPS mandates high uniform patent protection with narrow flexibilities (originator-favorable reading, different beneficiary/victim sets, different epsilon). dispute_settlement_interpretive_authority addresses a structurally distinct question — who holds binding interpretive authority over the text — and is largely orthogonal to the substantive content dispute between this reading and strong_exclusivity_reading, though WTO panel rulings under that authority structure determine which substantive reading prevails in specific disputes. All three should be read as a family; none is authored as an average or hedge over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
