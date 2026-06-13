% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Interpretive Authority over TRIPS
 *   domain: international_trade_law
 *
 * SUMMARY:
 *   The WTO's Dispute Settlement Understanding (DSU) grants panels binding
 *   interpretive authority over TRIPS text. This story instantiates the
 *   reading that privileges this institutional authority: panels are the
 *   final interpreters of what TRIPS permits, and their rulings are enforced
 *   through trade retaliation mechanisms (suspension of concessions under DSU
 *   Article 22). This reading does NOT claim TRIPS text mandates high uniform
 *   IP protections, NOR does it claim TRIPS embeds broad public health
 *   flexibilities—those are sibling readings. THIS reading is about WHO
 *   DECIDES what TRIPS means, and answers: the dispute panels do, with
 *   enforcement muscle through trade retaliation. The constraint's effect is
 *   to lock in panel interpretations (which have trended toward narrow public
 *   health flexibilities) as binding precedent, making reversal costly. The
 *   Appellate Body collapse (2017–present) has amplified this effect by
 *   removing appellate review, shifting resolution power toward developed
 *   countries with litigation capacity. The measured extractiveness rise from
 *   0.38 (1995, early TRIPS, broad text interpretation) to 0.68 (2025,
 *   accumulated panel precedent narrowing flexibilities) tracks the
 *   institutional hardening of the dispute settlement reading over time.
 *
 * KEY AGENTS:
 *   - WTO dispute panels: institutional agenda-setter holding binding interpretive authority
 *   - Pharmaceutical patent holders: beneficiaries capturing the constraint's enforcement benefit
 *   - Low-income developing countries: structural victims whose public health measures face retaliation risk
 *   - Generic drug manufacturers: constrained payers under narrowed Article 31bis interpretation
 *   - Public health advocacy networks: observers with limited standing, counterweight to pharma lobbying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.72).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Interpretive Authority over TRIPS").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'a4106533-dd41-4752-86d9-6949060f8b31').
narrative_ontology:cs_kernel_codification('a4106533-dd41-4752-86d9-6949060f8b31', formalized).
narrative_ontology:cs_authority_grounding('a4106533-dd41-4752-86d9-6949060f8b31', lineage).
narrative_ontology:cs_interpretation_layer_present('a4106533-dd41-4752-86d9-6949060f8b31').
narrative_ontology:cs_reading_relation('a4106533-dd41-4752-86d9-6949060f8b31', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4106533-dd41-4752-86d9-6949060f8b31', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_axiom('a4106533-dd41-4752-86d9-6949060f8b31', foundational, panel_adjudication_binding_over_unilateral_interpretation).
narrative_ontology:cs_axiom_status(panel_adjudication_binding_over_unilateral_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('a4106533-dd41-4752-86d9-6949060f8b31', panel_adjudication_binding_over_unilateral_interpretation, conventional).
narrative_ontology:cs_axiom('a4106533-dd41-4752-86d9-6949060f8b31', foundational, institutional_authority_precedes_substantive_flexibility).
narrative_ontology:cs_axiom_status(institutional_authority_precedes_substantive_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('a4106533-dd41-4752-86d9-6949060f8b31', institutional_authority_precedes_substantive_flexibility, instrumental).
narrative_ontology:cs_reference_frame('a4106533-dd41-4752-86d9-6949060f8b31', panel_neutral_institutional_authority).
narrative_ontology:cs_drift_state('a4106533-dd41-4752-86d9-6949060f8b31', contemporary_post_appellate_body_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a4106533-dd41-4752-86d9-6949060f8b31', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, dispute_settlement_institutional_authority).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, low_income_developing_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, hiv_aids_treatment_programs).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_drug_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_governments).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_institutional_legitimacy_through_binding_adjudication).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, intellectual_property_enforcement_through_trade_coercion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret TRIPS text in the formal dispute settlement process; their rulings are binding on complaining and defending parties. They have authority to declare a country's IP laws inconsistent with TRIPS and trigger enforcement mechanisms. Their interpretations accumulate as precedent, effectively authoring the constraint's meaning over time. The institutional position depends on the perception that panels are neutral adjudicators, not strategic actors favoring high-IP regimes.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from panel rulings that narrow compulsory licensing and parallel import exceptions, protecting market exclusivity and pricing power in high-value jurisdictions. They lobby for stricter interpretations and finance amicus curiae briefs in disputes. Their exit option is relocating R&D and manufacturing to jurisdictions with stronger enforcement; most prefer the global regime standardization that panel authority delivers.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_patent_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Face binding panel rulings that restrict their ability to use TRIPS Article 31 (compulsory licensing) and Article 31bis (parallel importation) flexibilities to access affordable generic medicines. When they attempt to issue compulsory licenses for TB, malaria, or HIV treatment, they risk retaliation through trade sanctions if a dispute panel rules against them. Their exit is to accept higher medicine prices, let diseases spread, or withdraw from the WTO—all costly or politically infeasible.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, low_income_developing_countries, payer,
    powerless, generational, trapped, national).

% Depend on affordable generic antiretroviral medications supplied through compulsory licensing and parallel importation. They absorb the cost when panel interpretations narrow these flexibilities: they either pay higher prices to patent-holding manufacturers, ration treatment to the wealthiest patients, or reduce coverage. Their advocacy power is real but unequal to pharmaceutical lobbying in panel processes.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, hiv_aids_treatment_programs, payer,
    organized, biographical, constrained, regional).

% Operate under panel-mediated interpretations of TRIPS Article 31, which restricts their ability to manufacture under compulsory license for export to other developing countries. Strict panel interpretations limit their market and reduce investment in production capacity for generic versions of expensive drugs. Their constraint is: panels narrowly read Article 31bis, requiring products made under license to be used primarily in the issuing country, crippling bulk generic manufacturing for regional supply.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_drug_manufacturers, payer,
    moderate, biographical, constrained, regional).

% Home jurisdictions for most pharmaceutical patent holders. They use panel authority as a tool to enforce IP regimes favorable to their domestic industries. They file complaints on behalf of industry (formal or informal influence) when developing countries attempt flexibility measures. They defend panel authority as neutral rule of law and resist demands for explicit public health carve-outs.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_governments, beneficiary,
    institutional, generational, analytical, national).

% Have minimal voice in formal dispute proceedings despite bearing disproportionate health burden from expensive medicines. They cannot afford to bring complaints or defend against challenges to their own IP measures due to cost and lack of legal capacity. Their exclusion from the adjudicatory process means panel rulings reflect bilateral negotiations between developed and developing nations, not the interests of the most vulnerable.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, small_island_developing_states, excluded,
    powerless, biographical, trapped, local).

% The WTO Appellate Body effectively collapsed after 2017 due to U.S. obstruction of appointments. This absence shifts dispute resolution toward ad-hoc panel interpretation with reduced appellate review, increasing unilateral discretion in binding interpretations. Were the Appellate Body functional, it could serve as a check on panel overreach; its dysfunction is itself part of the constraint's enforcement structure.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, appellate_body_consensus_coalition, excluded,
    institutional, generational, trapped, global).

% Monitor disputes and mount public campaigns to pressure panel interpretation. They lack formal standing in the process but testify through amicus curiae briefs where permitted. Their analytical seat gives them visibility to the structure but limited enforcement power; they are a counterweight to pharmaceutical lobbying but one on an asymmetric playing field.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_advocacy_networks, observer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_patent_holders).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a global dispute resolution mechanism that enforces a uniform intellectual property standard across 164 WTO members, preventing regulatory races-to-the-bottom where countries would slash IP protection to attract generic drug manufacturing. The coordination solves the multi-party standard-setting problem by substituting panel adjudication for bilateral negotiation, reducing transaction costs and uncertainty for multinational pharmaceutical firms.
% TRANSFER_FUNCTION: Transfers authority to interpret TRIPS obligations from national legislatures to WTO panels, and transfers the benefit of IP exclusivity from the public domain (where Article 31 compulsory licensing would permit generic production) to pharmaceutical patent holders (whose exclusivity is protected when panels narrowly read Article 31). Also transfers retaliation risk from developed countries to developing countries through the enforcement mechanism (DSU Article 22 suspension of concessions).
% ABSENT_VOICES: Least-developed countries, small island states, and patient advocacy organizations are structurally excluded from formal dispute standing. They lack resources to mount complaints, cannot afford to defend against challenges to their own IP measures, and have no formal role in panel composition. Pharmaceutical regulatory agencies in poor countries cannot participate in dispute briefs unless their government files, and governments often lack capacity or political will. The process privileges industrial-nation litigants and organized industry groups (through amicus curiae access) over health ministries and patient representatives.
% DISAPPEARANCE_RATIONALE: If WTO dispute settlement authority over TRIPS disappeared, countries would revert to unilateral IP lawmaking and bilateral negotiation. Developing countries would immediately broaden compulsory licensing and parallel importation; generic drug production would surge in countries no longer bound by panel precedent; pharmaceutical prices would fall sharply in low-income regions. The global IP regime would fragment into regional regimes (EU high-IP, developing-country generic-friendly, China-hybrid). Pharmaceutical firms would face inconsistent regulation and reduced pricing power in poor markets.
% FOUNDING_PROBLEM: TRIPS agreement (1995) was created to establish minimum IP standards across trading partners and prevent regulatory arbitrage. The founding problem: countries had divergent interests—developed countries wanted strong patent protection for pharmaceutical R&D, while developing countries wanted flexibility to access medicines and preserve traditional knowledge. TRIPS text attempted to balance these by mandating patent terms (Article 33) while embedding public health flexibilities (Articles 31, 31bis, 6). The question of HOW TO ENFORCE the balance, and who decides its meaning, was delegated to dispute panels without explicit rules for interpreting the flexibilities.
% FOUNDING_PROBLEM_CORROBORATION: Pharmaceutical industry and developed-country governments attest the problem is live—without binding panel authority, countries would circumvent TRIPS flexibilities and weaken IP globally, deterring drug development. Developing countries, WHO, MSF, and UNAIDS attest the problem was solved (TRIPS created the minimum standard) but panels are now re-solving it in the direction of IP strength, overriding the balance TRIPS embedded. UN Special Rapporteur on health attests panels have adopted narrow readings of Article 31 unsupported by text. UN fact-finding missions on access to medicines (2017, 2020) corroborate that panel precedent, not new law, has narrowed developing-country flexibility. Academic economic analysis (Bolton, Maskus) shows measured narrowing of Article 31 scope through case law vs. text language.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 1995–2020 because panel precedent progressively narrowed Article 31 and Article 31bis flexibility, despite TRIPS text language permitting these flexibilities. The rate plateaued 2020–2025 as the narrow interpretation became institutionalized and major disputes (India generic exports, Thailand compulsory licensing) settled into expected panel outcomes. Theater rises sharply 2001–2015 (justifications shifted from 'coordination' to 'rule of law and neutrality') and plateaus at 0.41, indicating that ~41% of enforcement activity is now defensive (panels justify narrow readings through formalist interpretation, not functional argument). Suppression is high and stable (0.52→0.72) because enforcement is structural: countries choosing flexibilities face retaliation risk that grows with dispute panel precedent, not new legal language—the suppression is the accumulated precedent binding future behavior. Accessibility collapse is moderate (0.64) because alternative IP regimes (bilateral FTAs, national law) remain formally available but practically costly to adopt, especially for countries wanting WTO access. Resistance (0.58) is real: developing countries mount formal defenses in disputes, public health advocates pressure panels through amicus briefs, and countries test flexibility boundaries, but they lose leverage as precedent accumulates.
 *
 * PERSPECTIVAL GAP:
 *   From the panels' and developed-country seats, the constraint is legitimate institutional coordination: binding interpretation prevents regulatory chaos and protects innovation incentives. From the low-income country seat, the same constraint is enforced extraction: panels interpret flexibilities narrowly, developing countries cannot legally use them without risking retaliation, and pharmaceutical prices remain unaffordable. From the generic manufacturer's seat, the constraint is a dual role (payer + beneficiary): narrowed Article 31bis hurts their compulsory-license market but stable IP enforcement protects their existing operations in high-IP jurisdictions. The engine computes divergent classifications from these structural differences: panels see coordination; victims see extraction. The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical patent holders have low directionality (~0.15): they benefit directly from panel rulings and have arbitrage exit (can relocate R&D to high-IP jurisdictions; the regime favors them). Low-income countries have high directionality (~0.85): they are constrained targets whose compulsory licensing rights are narrowed by panels, with trapped exit (cannot withdraw from TRIPS without WTO exit). Panels themselves sit at analytical directionality (0.5) in structural terms but derive their actual power from developed-country backing—the institutional seat is symmetric in the rules but asymmetric in state support. Public health advocates are excluded rather than codirected; their resistance exists but they lack standing, so directionality is not a clean metric—they are observers with organized power but analytical status.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mislabeling by declaring its dual nature: it is a Tangled Rope, not pure extraction (Snare), because it solves a genuine coordination problem (uniform IP standards across 164 parties) while extracting asymmetrically (panels narrow flexibilities, benefiting patent holders at developing-country expense). The founding problem is live (TRIPS did set out to coordinate) but the reading of its solution has shifted: from 'balanced coordination with flexibilities' (1995) to 'strict IP enforcement with minimal exceptions' (2025, in practice). Mandatrophy in this reading is incomplete: the institutional authority (panels) would reject the extraction framing and assert neutrality; the constraint does not yet meet full mandatrophy because the legitimacy claim (panels are neutral interpreters) remains contested, not collapsed. IF the Appellate Body were restored, the configuration might reclassify toward rope; the collapse of appellate review (itself a constraint meta-layer) has amplified extractiveness by removing institutional checks on panel discretion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    panel_neutrality_vs_capture,
    'Are WTO dispute panels genuinely neutral adjudicators applying TRIPS text, or are they structurally captured by developed-country interests through litigation resources and institutional socialization?',
    'Comparative analysis of panel composition (nationality, prior employment), dispute outcomes by claimant-defendant power asymmetry, and qualitative interviews with panelists about their decision-making process. Statistical test: does the probability of ruling for the claimant differ when claimant is developed vs. developing country, controlling for case strength?',
    'If captured, the ''institutional authority'' framing is false legitimation, and the constraint reclassifies from tangled_rope (coordination + asymmetric extraction) to snare (pure extraction with coordination cover). If neutral, the constraint remains tangled_rope but theater_ratio should decline and accessibility_collapse should rise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(panel_neutrality_vs_capture, empirical, 'Whether panels function as neutral interpreters or institutional agents of developed-country pharmaceutical interests.').

omega_variable(
    article_31_plain_language_scope,
    'Does the plain language of TRIPS Article 31 (compulsory licensing for any reason, without prior negotiation) support the narrow interpretations panels have adopted (emergency-only, domestic use, export-restricted via Article 31bis), or do panels enforce interpretations that contradict the text?',
    'Statutory textual analysis by comparative IP scholars, UN fact-finding missions, and formal linguistics. Compare the text''s actual scope (which Article 31 permits) to panel rulings (which narrow the scope through added conditions).',
    'If panels contradict plain language, the constraint becomes pure extraction (Snare): panels use the authority delegated by TRIPS to rewrite TRIPS against its stated terms, suppressed by their institutional position. If panels correctly read Article 31 as narrow, the constraint remains tangled_rope with legitimate coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_31_plain_language_scope, empirical, 'Whether panel interpretations of Article 31 match or contradict TRIPS text''s explicit language.').

omega_variable(
    appellate_body_functional_role,
    'Would restoration of the Appellate Body reduce the extractiveness of this constraint by creating appellate review of panel interpretations, or is the Appellate Body collapse itself an enforcement feature sustaining the constraint?',
    'Historical analysis of Appellate Body decisions 1995–2017 (did it reverse or narrow panel IP-restrictive rulings?). Counterfactual: if the Appellate Body were functional, would pending disputes (India pharmaceutical exports, Thailand generics) likely resolve differently?',
    'If the Appellate Body would constrain panels, its collapse is a meta-constraint enabling the dispute-settlement reading''s extractiveness. Restoration might reclassify the constraint toward rope. If the Appellate Body would maintain panel interpretations, it is irrelevant to extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_functional_role, empirical, 'Whether Appellate Body functionality would reduce panel discretion and narrow IP-favorable interpretations.').

omega_variable(
    reading_coexistence_or_foreclosure,
    'Does the dispute_settlement_interpretive_authority reading logically foreclose the public_health_flexibility_reading, or can both coexist as live positions held by different coalitions?',
    'Formal logic check: can a party simultaneously hold ''panels have binding authority'' AND ''panels must interpret TRIPS to preserve public health flexibilities'' without contradiction? The answer depends on whether PANEL AUTHORITY is the claim (then both coexist if panels adopt the public-health reading) or whether NARROW INTERPRETATION is the claim (then they foreclose each other).',
    'If they foreclose, one reading will eventually displace the other and reclassify out. If they coexist, both remain live and institutional contest continues. The answer determines whether this constraint is a temporary meta-layer (dispute settlement authority) or a stable institutional trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_or_foreclosure, conceptual, 'Whether the dispute-settlement authority claim is compatible with public-health-flexible interpretation.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.72) structural (retaliation threat from developed countries, legal costs of disputes) or internalized (developing countries have internalized the belief that they cannot use Article 31 even when legally permitted)?',
    'Post-Appellate-Body natural experiment: if panel authority authority remains but one major developing country tests compulsory licensing and faces no retaliation (because dispute parties resolve bilaterally), does suppression persist among other countries? Internalized suppression would persist; structural suppression would decline.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—countries carry the suppression with them even if formal retaliation risk declined. This indicates the constraint has achieved cultural lock-in (identity-fusion with ''law-abiding IP respector''). If structural, removing the retaliation threat would unlock behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression is externally enforced (retaliation threat) or internally maintained (belief constraint).').

omega_variable(
    kernel_reading_committer_frame,
    'This story instantiates the dispute_settlement_interpretive_authority reading of the TRIPS kernel. Are there configurations of panel authority and interpretation that would reclassify this constraint as rope (pure coordination) rather than tangled_rope (coordination + extraction), or is asymmetric extraction inherent to binding dispute panels?',
    'Counterfactual design: suppose panels interpreted TRIPS to maximize both IP enforcement AND public health access (e.g., green-light Article 31 for epidemics, narrow only frivolous claims). Would that configuration be stable, or would patent-holder pressure force interpretive drift back toward narrowing? If stable, extraction is not inherent; if drift is inevitable, extraction is structural.',
    'If extraction is inherent to institutional authority, this reading remains tangled_rope or degrades to snare. If extraction is contingent on panel composition and pressure, panels could reclassify toward rope through institutional reform (diverse panelist origins, explicit public health criteria, appellate review).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether the dispute-settlement reading''s extraction is structurally necessary or contingent on panel pressure asymmetries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.08).
narrative_ontology:measurement_basis(trip_tr_t1995, observed).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2001, 0.12).
narrative_ontology:measurement_basis(trip_tr_t2001, observed).
narrative_ontology:measurement(trip_tr_t2008, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2008, 0.25).
narrative_ontology:measurement_basis(trip_tr_t2008, observed).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2015, 0.35).
narrative_ontology:measurement_basis(trip_tr_t2015, observed).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(trip_tr_t2020, observed).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2025, 0.41).
narrative_ontology:measurement_basis(trip_tr_t2025, projected).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement_basis(trip_be_t1995, observed).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement_basis(trip_be_t2001, observed).
narrative_ontology:measurement(trip_be_t2008, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement_basis(trip_be_t2008, observed).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement_basis(trip_be_t2015, observed).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(trip_be_t2020, observed).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(trip_be_t2025, projected).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement_basis(trip_su_t1995, observed).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement_basis(trip_su_t2001, observed).
narrative_ontology:measurement(trip_su_t2008, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2008, 0.64).
narrative_ontology:measurement_basis(trip_su_t2008, observed).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement_basis(trip_su_t2015, observed).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement_basis(trip_su_t2020, observed).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(trip_su_t2025, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, global_infrastructure).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.22).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_compulsory_licensing_article_31).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_parallel_importation_article_6).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_appellate_body_institutional_review).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TRIPS kernel. The kernel is the TRIPS agreement text itself (fixed, formalized). This reading privileges WTO dispute panels as the institutional interpreters of what TRIPS permits. Sibling readings—public_health_flexibility_reading (TRIPS embeds broad flexibilities) and strong_exclusivity_reading (TRIPS mandates high uniform protection)—instantiate alternative interpretive authorities and different substantive emphases. All three readings reference the same kernel text; they differ on who decides interpretation and what that interpretation prioritizes. Each reading is a separate constraint with its own ε, beneficiaries, victims, and structural dynamics. The network edges trace how one reading's institutional hardening (dispute panels locking in narrow interpretations through precedent) influences the feasibility and operation of the other readings (flexibilities become paper rights; exclusivity becomes enforced practice).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
