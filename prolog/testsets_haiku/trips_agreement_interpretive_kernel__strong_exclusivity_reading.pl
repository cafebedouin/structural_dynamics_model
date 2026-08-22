% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Exclusivity Reading: Patent Protection Mandate with Narrow Health Flexibilities
 *   domain: international_trade/intellectual_property/public_health
 *
 * SUMMARY:
 *   The TRIPS Agreement (1995) embeds a contested kernel: a formal text that
 *   can be read to prioritize either strong patent protection to incentivize
 *   pharmaceutical innovation OR public health flexibilities (compulsory
 *   licensing, parallel importation) to ensure access. This story
 *   instantiates the strong exclusivity reading — the interpretation that has
 *   dominated WTO dispute settlement since 2000 and shapes actual practice:
 *   TRIPS mandates high uniform patent protections across all member states
 *   with narrow, difficult-to-invoke flexibilities. Patent holders benefit
 *   directly; low-income countries and patients bear high prices and
 *   constrained generic access. The reading persists through active
 *   enforcement by WTO dispute panels (the agenda-setter) backed by
 *   high-income governments' trade retaliation threat. The sibling
 *   public_health_flexibility_reading would authorize broad compulsory
 *   licensing and parallel importation; it remains a live position held by
 *   low-income countries and NGOs but loses consistently in dispute panels.
 *   This story describes the strong reading's structure and operation; it
 *   does NOT describe a natural settlement of the TRIPS text, but rather ONE
 *   reading among interpretively defensible alternatives.
 *
 * KEY AGENTS:
 *   - Multinational pharmaceutical corporations (beneficiaries, institutional power, global scope) — extract rents through patent exclusivity; their licensing revenue is globally enforceable
 *   - Patent holder institutions (beneficiaries, institutional power) — universities and biotech firms licensing to manufacturers, receiving reliable royalty flows
 *   - Low-income countries (payers, moderate-to-powerless power, constrained exit) — obligated to enforce the same patent rules as high-income countries; compulsory licensing narrowly construed and practically inaccessible
 *   - Patients in developing regions (payers, powerless, trapped exit, immediate time horizon) — face monopoly prices; generics unavailable during patent term; structurally excluded from TRIPS voice
 *   - WTO dispute panels (agenda-setter, institutional power) — operationalize the strong reading through binding interpretations that consistently narrow flexibilities and uphold patent enforcement
 *   - High-income governments (beneficiary + agenda-setter, powerful, arbitrage exit) — benefit through domestic pharma industries; use trade retaliation threat to enforce the strong reading globally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Exclusivity Reading: Patent Protection Mandate with Narrow Health Flexibilities").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade/intellectual_property/public_health").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7').
narrative_ontology:cs_kernel_codification('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7', fixed_text).
narrative_ontology:cs_authority_grounding('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7', extraction).
narrative_ontology:cs_interpretation_layer_present('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7').
narrative_ontology:cs_reading_relation('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_axiom('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7', foundational, patent_protection_globally_uniform_and_strong).
narrative_ontology:cs_axiom_status(patent_protection_globally_uniform_and_strong, holdable).
narrative_ontology:cs_axiom_grounding('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7', patent_protection_globally_uniform_and_strong, empirically_contingent).
narrative_ontology:cs_axiom('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7', foundational, compulsory_licensing_exception_not_the_rule).
narrative_ontology:cs_axiom_status(compulsory_licensing_exception_not_the_rule, holdable).
narrative_ontology:cs_axiom_grounding('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7', compulsory_licensing_exception_not_the_rule, deontological).
narrative_ontology:cs_reference_frame('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7', patent_holder_property_rights_framework).
narrative_ontology:cs_drift_state('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd77d8a6-4bc9-4ce5-8e3c-fd7cd96df9a7', '2026-06-13T14:32:00Z').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_corporations).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patent_holder_institutions).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_regions).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain exclusive market rights under TRIPS to recover R&D investment across developed and developing markets simultaneously. Can charge uniform high prices globally during patent term because generic competitors are barred by the strong exclusivity interpretation. Extract substantial economic rents during the protection period; the interpretation secures their ability to use high-income market profits to cross-subsidize R&D that serves primarily high-income populations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Universities, biotech firms, and research institutes that license patents to manufacturers. Benefit from strong enforcement mechanisms that guarantee licensing fees and royalties flow reliably from manufacturers across all jurisdictions. The strong reading makes the licensing income stream predictable and globally enforceable.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patent_holder_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Obligated by TRIPS to enforce the same patent protections as high-income countries despite vastly lower per-capita ability to pay and acute public health need for affordable medicines. The strong exclusivity reading narrows their compulsory licensing options through dispute panel interpretation that treats Article 31 as an exception to be narrowly construed. Cannot exit TRIPS without cascading trade sanctions and loss of market access for their own goods.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_countries, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_countries, excluded).

% Face patent-protected drug prices set for global markets (anchored by high-income demand and ability to pay) rather than local ability to pay. Generic alternatives that would cost 90% less are unavailable during patent term. Health systems cannot budget for the patented versions. No mechanism exists to voice their preference for access over innovation-incentive doctrine; their need is structurally invisible in TRIPS interpretation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_regions, payer,
    powerless, immediate, trapped, global).

% Barred from manufacturing low-cost versions during patent term under the strong exclusivity reading. Can manufacture only after patent expiry. Their productive capacity and willingness to supply low-cost medicines (typically 90% cheaper than originals) is activated only when the strong reading no longer applies — they are structurally excluded from the most critical period (patent term) when prices are highest and access is most constrained.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers, payer,
    organized, biographical, constrained, global).

% Interpret TRIPS text through dispute settlement; their rulings bind member states and set precedent for how strongly the patent protections are enforced. Under this reading, panels consistently construe compulsory licensing narrowly (WT/DS114 India/US, WT/DS409 Brazil/US) and invalidate public-health-justified exceptions. They are the enforcement machinery that operationalizes the strong reading into actual constraints on generic manufacturing and pricing.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% Benefit indirectly through enhanced patent regimes that boost domestic pharmaceutical and biotech industries (major employers, innovation drivers, export revenue). Use WTO dispute machinery and bilateral trade pressure to enforce the strong reading globally. Their corporations dominate the beneficiary seats; their governments leverage trade rules to entrench the interpretation and threaten retaliation against countries that invoke compulsory licensing.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_governments, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_governments, agenda_setter).

% Are not formal parties to TRIPS interpretation; their voice appears in amicus briefs and political pressure outside dispute panels. Would argue that the strong exclusivity reading sacrifices millions of lives annually to pharmaceutical profits and that the text permits public-health-centered interpretation. Structurally excluded from the adjudication process; their evidence on mortality correlates with access constraints is inadmissible in the narrow dispute panel frame.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocates, excluded,
    moderate, biographical, constrained, global).

% Monitor the constraint's operation and document health outcomes. Produce evidence that high prices correlate with access loss and preventable mortality in low-income regions. Their analysis feeds into the founding_problem_status debate and strengthens the public_health_flexibility_reading position, but carries no formal weight in dispute resolution.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, civil_society_organizations, observer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_corporations).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes uniform global patent protection that signals to pharmaceutical corporations they can recover R&D investment across all markets simultaneously, reducing country-by-country licensing negotiation and enabling single global pricing strategies. Solves a coordination problem for multinational firms: without TRIPS, a firm investing in a new drug would face 195 separate negotiation contexts, each claiming inability to pay or threatening price controls; TRIPS unifies the institutional framework so the firm can price uniformly.
% TRANSFER_FUNCTION: Moves the economic surplus from drug sales from consumers and health systems in low-income regions (who face monopoly prices during patent term) to patent holders and high-income pharmaceutical shareholders. Transfers decision-making authority over essential medicine access from elected health officials in low-income countries to patent holders via the WTO dispute mechanism (when countries try to use compulsory licensing, disputes arise). Transfers time value of innovation costs from high-income populations (who can afford high prices) to low-income populations (who cannot, and so forgo treatment).
% ABSENT_VOICES: Patients in low-income countries cannot participate in TRIPS interpretation; they have no seat at the WTO table. Generic drug manufacturers from India and other developing regions have constrained voice (their governments can argue on their behalf but lack enforcement power). Public health ministries from low-income countries can formally participate in WTO but cannot match the legal resources of high-income trading blocs. Epidemiologists and global health researchers are excluded from dispute panels; their evidence on mortality correlates with access constraint is inadmissible in the narrow interpretive frame.
% DISAPPEARANCE_RATIONALE: If the strong exclusivity reading dissolved and the public-health-flexibility reading became binding (or if TRIPS itself dissolved), governments would immediately invoke compulsory licensing to manufacture generic antiretrovirals, antibiotics, and cancer drugs; prices would collapse 85–95% within months; pharmaceutical corporations would repricing strategies toward high-income markets and reduce R&D pipeline for diseases affecting only low-income populations; the geographic cost-segregation that currently exists would break, forcing unified global pricing or tiered access schemes. The world would rearrange around generic access, not because the coordination problem disappears but because the strong reading's enforcement mechanism would no longer hold it in place.
% FOUNDING_PROBLEM: Pharmaceutical R&D is capital-intensive, high-risk, and requires recovery of sunk costs plus profit across all addressable markets. Without patent protection, a firm that invests in a new drug faces generic competition immediately upon entry, eliminating the pricing power needed to amortize R&D. Low-income countries have strong incentives to copy successful drugs at marginal cost once the original developer has borne the risk. Patent protection, applied globally and uniformly, solves the innovator's coordination problem: secure market exclusivity worldwide, enabling unified pricing and global profit aggregation.
% FOUNDING_PROBLEM_CORROBORATION: Pharmaceutical industry and USTR attest the founding problem remains live: generic entry upon patent expiry demonstrates the ongoing need for exclusivity; they cite R&D productivity metrics and pipeline data. Public health economists (Médecins Sans Frontières, Harvard, Oxford) and WTO critics attest the founding problem is partially solved (drug development has not ceased) and the strong reading persists because it serves extractive interests, not innovation incentives. Independent analysis from WHO and academic literature documents that price elasticity in low-income markets means high prices do not recover R&D meaningfully there; the strong reading persists because high-income markets anchor profits, not because low-income access drives innovation.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the strong reading enables patent holders to charge monopoly prices in markets where ability to pay is vastly constrained, and low-income countries cannot use compulsory licensing to override these prices without facing WTO disputes. The gap between actual cost and patent-protected price is the extraction; it is sustained by the reading's narrow construction of flexibilities (compulsory licensing for emergencies only, strict royalty requirements, long patent terms). Suppression is high (0.72) because the enforcement mechanism — WTO dispute panels with retaliation authority — deters low-income countries from invoking their formal flexibilities; India's compulsory license for Sofosbuvir (hepatitis C) faced legal challenge; Pakistan's generic ARV manufacturing faced trade pressure. Theater ratio is moderate (0.28) because the innovation-incentive justification is partially real (R&D does respond to patent protection) but an increasing share of enforcement effort goes to defending rents rather than funding new research (as drug prices rise faster than R&D productivity). The measurement series show extractiveness rising from 1995–2020 as dispute panels narrowed flexibilities and as pharmaceutical firms consolidated market power; suppression requirement rose as enforcement capacity was tested and hardened (India's generics challenged; countries learned they cannot safely invoke compulsory licensing without retaliation). Theater ratio rose because the constraint's stated function (reward innovation) is decoupling from its operation (defending existing monopolies on off-patent conditions).
 *
 * PERSPECTIVAL GAP:
 *   From the patent holder and high-income government seats, this reading appears as legitimate property protection and innovation incentive: the text says what it says; TRIPS is a contract freely entered; pharma R&D requires exclusivity; prices reflect value creation. From the low-income country and patient seats, the same reading appears as extractive colonialism: their governments did not freely choose TRIPS (it was a condition of WTO membership during asymmetric power-imbalance negotiation in 1995); the reading's narrow flexibilities were promised but made practically unusable through dispute panel interpretation; prices do not reflect what their economies can bear; the reading persists because high-income trading powers enforce it, not because it is legitimate. The WTO panels (analytical seat) compute a narrow legal reading of the TRIPS text as written, but do not address the distributional questions about who benefits and who bears costs — the panels' technical legal authority masks the underlying asymmetry of power and interest. The engine computes per-seat types from these structural relationships: from the beneficiary seat, the reading may compute as coordination or rope (genuine innovation incentive, parties participate); from the victim seat, as tangled_rope or snare (coordinating innovation for some while extracting from others).
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational pharma corporations (institutional power, arbitrage exit) have d near 0.0 (full beneficiaries): they set prices, enforce exclusivity globally, and can exit through alternative markets or licensing arrangements. Patent holder institutions (institutional power, arbitrage) have d near 0.1 (strong beneficiaries): they collect reliable royalties. Low-income countries (moderate power, constrained exit) have d near 0.85 (targets): they are obligated to enforce the same rules as high-income countries despite different capacity, and their attempt to use flexibilities faces dispute panel retaliation. Patients in developing regions (powerless, trapped) have d near 0.95 (full targets): they bear the cost (high prices, no generics, forgoing treatment) with no exit and no voice. Generic manufacturers (organized power, constrained exit) have d near 0.75 (targets): they are barred from operating during patent term, the highest-value period. High-income governments (powerful, arbitrage) have d near 0.2 (beneficiaries): they benefit indirectly through pharma industry advantage but do not directly pay the constraint's cost. The spread in d values across seats reflects the fundamental asymmetry: some seats benefit from the exact mechanism others are trapped in.
 *
 * MANDATROPHY ANALYSIS:
 *   The strong exclusivity reading shows signs of mandate drift: the founding problem (incentivizing R&D in a world where generics would undermine profits) was live in 1995 when TRIPS was negotiated. By 2020, pharmaceutical R&D has been historically productive (over 450 new drugs approved annually in the U.S.), suggesting the founding problem no longer requires the level of protection being enforced. Yet the constraint persists and intensifies (extractiveness rising, enforcement hardening) — the reading is increasingly about defending profits on existing drugs rather than funding new research. The theater ratio rise (0.12 to 0.30) indicates performative maintenance: dispute panels invoke innovation-incentive language while consistently ruling against compulsory licensing even in emergencies (e.g., Thailand's generic ARV manufacturing for domestic public health faces legal challenge). The constraint shows piton-adjacent dynamics: administered by institutional machinery (WTO), persists partly by institutional inertia (dispute panels follow precedent), but is still actively enforced (not yet theatrical skeleton). The mandatrophy question is whether the strong reading persists because innovation incentive is genuine and necessary, or because it serves extractive interests independent of innovation benefits. The founding_problem_status (contested) and the divergence between stated function and measured operation support the mandatrophy frame: this is a reading that began as coordination (genuine problem-solving) and has drifted toward extraction (defending monopoly rents).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_empirical_necessity,
    'Does pharmaceutical R&D productivity and investment genuinely require the level of patent protection mandated under the strong exclusivity reading, or would a moderate/shorter-term protection regime still generate sufficient innovation?',
    'Comparative analysis of R&D productivity across patent regimes; historical trend analysis of drug approval rates vs. patent strength; natural experiments from jurisdictions that weakened patent protection (e.g., India''s pre-2005 patent law, Taiwan''s tiered approach); econometric modeling of R&D response to patent term changes.',
    'If moderate protection suffices for innovation, the strong reading''s justification is substantially weakened and the constraint reclassifies as pure extraction (snare) rather than coordination with extraction (tangled_rope). If strong protection is empirically necessary, the tangled_rope classification holds and the innovation-incentive frame is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_empirical_necessity, empirical, 'Whether the strong reading''s patent requirements are empirically necessary for pharmaceutical R&D or excess beyond innovation incentive.').

omega_variable(
    compulsory_licensing_practical_accessibility,
    'Are the Article 31 compulsory licensing flexibilities practically accessible to low-income countries, or have dispute panel interpretations narrowed them to unusability?',
    'Audit of compulsory licensing attempts post-1995: count of invocations, success rate, time-to-resolution in dispute panels, de facto chilling effect on future attempts (measured by stated intent vs. actual filing). Post-COVID analysis of COVID-vaccine licensing negotiations (which bypassed dispute panel but revealed political barriers).',
    'If flexibilities are practically inaccessible, the strong reading operationalizes as pure extraction without realistic escape valve; if accessible, it operationalizes as tangled_rope with genuinely available (if costly) remedies. This affects whether the constraint''s enforcement appears as suppression or as structural coercion with negotiable boundaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compulsory_licensing_practical_accessibility, empirical, 'Practical operability of TRIPS Article 31 compulsory licensing under the strong exclusivity reading.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the strong exclusivity reading logically foreclose the public health flexibility reading, or do both readings remain live interpretive options of the same text?',
    'Textual analysis: Are there TRIPS provisions that are literally incompatible with one reading or the other? (e.g., does Article 28 necessarily imply Article 31 is inaccessible, or are they logically separable?). Jurisprudential history: Have dispute panels treated the public health reading as logically impossible, or merely as disfavored and losing?',
    'If the strong reading forecloses the sibling, the kernel has one defensible interpretation and the constraint''s reading classification is determinate. If both remain live, the dispute is fundamentally about institutional power over interpretation, not textual meaning, and the engine''s dispute-settlement-authority mechanism becomes the principal driver of the strong reading''s persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Logical relationship between strong exclusivity and public health flexibility readings of the TRIPS text.').

omega_variable(
    patient_benefit_vs_high_income_profit_orientation,
    'Does pharmaceutical innovation under the strong reading produce therapeutics that address low-income disease burden (e.g., TB, malaria, parasitic infections) proportional to that burden, or is innovation pipeline oriented toward high-income prevalence (e.g., oncology, cardiovascular)?',
    'Drug approval audit: count of new drugs by indication; cross-index with disease burden by income level (WHO Global Burden of Disease); analyze licensing/pricing decisions by indication. If pipeline orientation is demonstrably toward high-income diseases despite global disease burden, innovation incentive is not equitably serving the populations bearing the extraction cost.',
    'If innovation pipeline is demonstrably skewed away from low-income disease burden, the founding_problem frame (incentivizing research for global health) is contradicted by the constraint''s actual operation — it incentivizes research for profitable (high-income) indications, and the strong reading''s extraction falls on populations whose diseases are not being researched. This supports mandatrophy and snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patient_benefit_vs_high_income_profit_orientation, empirical, 'Alignment between pharmaceutical innovation pipeline and low-income disease burden under the strong reading''s incentive structure.').

omega_variable(
    suppression_mechanism_enforcement_vs_internalized_acceptance,
    'Is suppression of low-income countries'' compulsory licensing efforts structural (fear of WTO retaliation, legal costs, dispute resolution uncertainty) or internalized (low-income governments have accepted the TRIPS regime as legitimate and don''t challenge it)?',
    'Interviews and documentary evidence from low-income country health ministries and trade negotiators; analysis of stated reasons for not invoking compulsory licensing (legal risk, retaliation fear, or normative acceptance); natural experiment of countries that have challenged (India''s generics, Thailand''s licenses) vs. those that haven''t; post-COVID policy shifts (did compulsory licensing become less suppressed after public health mobilization?).',
    'If suppression is structural (external threat), the constraint''s persistence depends on enforcement capability and could be disrupted by coalition-building among low-income countries or by shifting dispute panel composition. If suppression is internalized, the constraint carries its suppression with it even if external enforcement weakens — low-income countries will voluntarily comply. This affects whether the piton/snare classification is stable or vulnerable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_enforcement_vs_internalized_acceptance, empirical, 'Structural vs. internalized suppression of low-income countries'' access to TRIPS Article 31 compulsory licensing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(trip_tr_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(trip_be_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2005, 0.71).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(trip_su_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.18).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_term_extension_mechanisms).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, hiv_aids_drug_access_constraint_sub_saharan_africa).

% DUAL FORMULATION NOTE:
% The TRIPS Agreement is a contested kernel with two structural interpretations: the strong_exclusivity_reading (this story) and the public_health_flexibility_reading (sibling). They read the same text but operationalize different constraint structures with different beneficiary/victim sets, different extractiveness profiles, and different dispute-panel precedent histories. Neither reading is textually foreclosed; the dispute is institutional (WTO panels have consistently adopted the strong reading since 2000). Both stories describe binding constraints on actual state behavior, but the public_health_flexibility_reading remains less operationalized in practice (dominated in disputes, less invoked by low-income countries due to suppression mechanisms). The two stories are linked via network.affects_constraints to flag their common kernel and their institutional competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
