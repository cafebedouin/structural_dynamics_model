% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Extraterritorial Trigger — Effects Jurisdiction Reading
 *   domain: technology governance/international law
 *
 * SUMMARY:
 *   A non-EU company that offers goods or services to people in the EU, or
 *   monitors their behaviour, falls under the Union's data protection regime
 *   regardless of where it is headquartered: that is the operation of Article
 *   3(2) under the effects-jurisdiction reading, which holds that regulatory
 *   authority legitimately follows the effects of processing on EU residents.
 *   The trigger is enforced through supervisory-authority investigation,
 *   fines scaled to worldwide turnover, and the adequacy mechanism governing
 *   data flows. The arrangement solves a real cross-border protection problem
 *   and simultaneously concentrates recurring costs on parties who had no
 *   vote in creating it; a service industry has grown up around the
 *   obligations and receives a large share of the spend. This file
 *   instantiates one reading of the Article 3(2) kernel; the sibling readings
 *   are separate constraints linked in the network section.
 *
 * KEY AGENTS:
 *   - eu_supervisory_authorities: Agenda-setter and institutional beneficiary (institutional/constrained) — administers enforcement, accumulates fines and precedent
 *   - eu_data_subjects: Primary intended beneficiary (organized/constrained) — receive enforceable rights and a reachable complaint channel
 *   - non_eu_controllers: Primary target (powerful/constrained) — bear compliance costs and turnover-scaled fine exposure
 *   - small_non_eu_exporters: Secondary target (moderate/constrained) — bear regressive fixed compliance costs against thin margins
 *   - privacy_compliance_industry: Receipt-side beneficiary (organized/mobile) — invoices the mandated advisory, audit, and consent-tooling spend
 *   - adequacy_jurisdiction_firms: Conditionally cushioned payers (organized/constrained) — lighter treatment contingent on their country's adequacy status
 *   - non_eu_governments: Excluded objectors (institutional/trapped) — their firms are bound, but they hold no seat in rule-making
 *   - eu_based_businesses: Competitive beneficiary (organized/mobile) — gained a level playing field against offshore rivals
 *   - international_law_scholars: Analytical observer (analytical/analytical) — map the jurisdictional dispute the other seats argue within
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.62).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.58).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritorial Trigger — Effects Jurisdiction Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology governance/international law").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '6f38e1ed-04ac-46e2-8d38-92ce9055dc81').
narrative_ontology:cs_kernel_codification('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', fixed_text).
narrative_ontology:cs_authority_grounding('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', lineage).
narrative_ontology:cs_interpretation_layer_present('6f38e1ed-04ac-46e2-8d38-92ce9055dc81').
narrative_ontology:cs_reading_relation('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', foundational, jurisdiction_extends_to_effects_on_residents).
narrative_ontology:cs_axiom_status(jurisdiction_extends_to_effects_on_residents, holdable).
narrative_ontology:cs_axiom_grounding('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', jurisdiction_extends_to_effects_on_residents, deontological).
narrative_ontology:cs_axiom('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', secondary, targeting_or_monitoring_confers_nexus).
narrative_ontology:cs_axiom_status(targeting_or_monitoring_confers_nexus, holdable).
narrative_ontology:cs_axiom_grounding('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', targeting_or_monitoring_confers_nexus, conventional).
narrative_ontology:cs_reference_frame('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', effects_based_jurisdiction_baseline).
narrative_ontology:cs_drift_state('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', contemporary_enforcement_maturation, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('6f38e1ed-04ac-46e2-8d38-92ce9055dc81', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_based_businesses).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, privacy_compliance_industry).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, adequacy_jurisdiction_firms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_exporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, adequacy_jurisdiction_firms).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, charter_fundamental_rights_article_8).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, effects_principle_of_prescriptive_jurisdiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National data protection authorities (such as Ireland's DPC and France's CNIL) investigate complaints, issue fines, and coordinate through the European Data Protection Board; any one of them can open a case against a controller with no office in its territory if EU residents are targeted or monitored. Fine revenue flows to member-state budgets and investigative precedents accumulate to the authorities. Their discretion is bounded by board coordination, court challenge, and the politics of their host governments; they cannot withdraw from the enforcement role without abdicating their mandate.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, eu_supervisory_authorities, beneficiary).

% Residents of EU member states whose personal data is collected, profiled, and monetized by services operated from outside the Union. They gain enforceable rights — access, erasure, objection, portability — and a complaint channel that reaches controllers they could never sue abroad. Individually they hold little leverage; their collective weight arrives through test-case litigants and civil-society organizations that file on their behalf. They cannot opt out of being processed by services they use; their practical choice is which services to use at all.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, continental).

% Large technology and consumer companies headquartered in the United States, Asia, and elsewhere that offer services into the EU or track EU visitors. They maintain EU representative offices, appoint data protection officers, run consent infrastructure, and re-engineer products to satisfy EU rules, at costs their home jurisdictions do not impose. Exiting the EU market is technically available — some firms have geo-blocked EU users — but for platform businesses the installed user base and advertising reach make exit a strategic last resort rather than a live option.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_controllers, payer,
    powerful, generational, constrained, global).

% Small and mid-sized merchants, publishers, and software vendors outside the EU that sell into the Union through websites and app stores. The same legal obligations arrive at them as at the largest firms, but as a fixed cost against thin margins: privacy policies, cookie-consent tooling, records of processing, and breach procedures consume staff time they barely have. Many comply minimally, some ignore the rules and accept the risk of complaint, and a few quietly block EU traffic; none of these paths is costless.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, small_non_eu_exporters, payer,
    moderate, immediate, constrained, global).

% Consultancies, law firms, audit practices, and consent-management software vendors that sell the services and tooling the obligations require — impact assessments, representative appointments, records systems, consent banners. Demand for their work scales with the reach and stringency of the rules, and a large share of the money spent meeting them passes through their invoices. They operate across jurisdictions and would sell equivalent services under any comparably stringent regime.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, privacy_compliance_industry, beneficiary,
    organized, biographical, mobile, global).

% Companies based in countries the Commission has certified as adequate (Japan, the United Kingdom, Switzerland, and others under active decisions). Their transfers and operations face streamlined treatment compared with firms in non-adequate countries, though they remain bound by the underlying obligations. Their commercial position depends on the durability of their country's adequacy status, which the Commission reviews and can suspend.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, adequacy_jurisdiction_firms, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, adequacy_jurisdiction_firms, payer).

% Foreign ministries, trade negotiators, and data protection regulators in non-EU states whose firms fall under the EU trigger. They object diplomatically to the reach of the obligations into their economies, negotiate adequacy recognition to soften the burden for their industries, and in some cases legislate counter-measures. They were not participants in designing the rules their firms must meet, and their main instruments — negotiation and retaliation — operate entirely outside the EU legislative process.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments, excluded,
    institutional, generational, trapped, global).

% Companies established inside the Union that already carried the compliance burden under national law before the unified rules applied to anyone else. Extending the same obligations to foreign competitors removed a cost asymmetry that had favoured offshore rivals. They press for vigorous enforcement and against dilution, and they compete in the same markets as the foreign firms now bearing equivalent costs.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_based_businesses, beneficiary,
    organized, biographical, mobile, continental).

% Academic specialists in transnational legal process, jurisdiction doctrine, and data governance who analyse whether regulating foreign conduct on effects grounds is continuous with established practice (antitrust, securities, sanctions) or a novel expansion. They publish the mapping the other seats argue within, testify to legislatures, and hold no stake in the outcome beyond professional standing.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__effects_jurisdiction_reading, privacy_compliance_industry).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__effects_jurisdiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the cross-border protection gap that fragmented national laws could not: personal-data processing aimed at EU residents from outside any member state previously escaped effective oversight. Article 3(2) gives the protection regime a single trigger (targeting or monitoring of EU residents) and a single enforcement channel (any supervisory authority can act via the one-stop-shop), replacing twenty-seven uncoordinated local chases of offshore processors with one uniform standard.
% TRANSFER_FUNCTION: Moves compliance expenditure, legal exposure, and decision rights over data practices from non-EU controllers toward EU data subjects (who receive enforceable rights and remedies), EU supervisory authorities (who gain caseload, precedent, and fine revenue routed to member budgets), the privacy compliance service sector (which sells the mandated advisory, auditing, and consent tooling), and EU policymakers (who gain standard-setting leverage as foreign firms adopt EU norms globally).
% ABSENT_VOICES: Non-EU governments and their regulators are absent from the rule-making conversation: the obligations were legislated by EU institutions, yet they bind firms under foreign sovereigns who had no vote. Non-EU small exporters likewise had no seat. They surface only reactively — through trade negotiations, adequacy talks, and diplomatic objection — never as participants in setting the terms they must meet.
% DISAPPEARANCE_RATIONALE: If the extraterritorial trigger vanished overnight, offshore processing of EU residents' data would revert to the pre-2016 pattern: enforcement only where servers or establishments sat, forum shopping toward lenient jurisdictions, and rapid divergence between the protections EU residents enjoy and those the same firms offer elsewhere. Cross-border data contracts, adequacy arrangements, and the compliance industry built around the trigger would all unwind.
% FOUNDING_PROBLEM: Before 2016, data protection enforcement stopped at borders: a processor with no EU establishment could target EU residents at scale while each member state's authority lacked reach, and the 1995 Directive left a patchwork of national implementations with no unified trigger for foreign-directed processing.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: multinational controllers' own compliance documentation and industry submissions describe the pre-2016 multi-jurisdiction patchwork as the problem the unified trigger addressed; international-law scholarship independently documents the enforcement gap the Directive-era regime could not close; and non-EU governments implicitly attest the arrangement's reality by negotiating adequacy accords rather than ignoring it. No party disputes that the founding problem existed; the dispute is over whether the cure's costs are proportionate and who should bear them.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.62 for the standing arrangement as this reading assesses it: the trigger imposes substantial, recurring costs on parties outside the rule-making constituency, and the reading itself treats those costs as the justified price of resident protection rather than as an illegitimate taking — the number records the size of the transfer, not a verdict on its legitimacy. Suppression (0.58) is a raw structural property, unscaled by power or scope: enforcement rests on fines scaled to global turnover, order powers, and the adequacy lever, against which the realistic alternatives (comply, geo-block, restructure around the targeting test) are costly but not absent. Theater (0.32) reflects the consent-layer problem: banner interactions and box-ticking documentation are widespread, while the enforcement core — investigation, fining, adequacy review — remains functional. Accessibility collapse (0.45): understanding the trigger does not eliminate alternatives, since market exit and restructuring remain nominally open; resistance (0.50) registers sustained industry lobbying, diplomatic objection, and scholarly contestation. The three temporal series share one grid (yearly, t=0-8 from the May 2018 application date) so no metric borrows another's end-state; the suppression_requirement series is authored deliberately because this story tracks enforcement-capacity buildup — the guidance-and-grace era gave way to landmark fining and adequacy leverage, a hardening the scalar alone cannot show. Enforcement is episodic (waves following rulings and scandals) but the trend across the grid is monotonic, so no cyclical series is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the engine computes each from the structural data. From the supervisory-authority seat the arrangement is a protective jurisdiction successfully extended; from the multinational-controller seat it is a foreign obligation with constrained exit and severe fine exposure; from the SME seat the same rule is a regressive fixed cost pushing toward quiet exit; from the compliance-industry seat it is demand. Two same-power institutional actors — EU authorities and non-EU governments — face opposite directionalities with no overlapping exit option, which is the inter-institutional signature this story exists to register. The claimed type (tangled_rope) is authored from the structural middle: a genuine cross-border protection function and a real, concentrated transfer operating through the same machinery. Whether the engine's per-seat computations reproduce that middle, or pull toward the payer seats' harsher reading given the named receipt seat and prohibitive fixing cost, is precisely the divergence the corpus measures; the claim is not tuned to anticipate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: eu_data_subjects, eu_based_businesses, and privacy_compliance_industry sit near the beneficiary pole (low d) — the first two receive protection and a leveled field, the third receives the compliance spend itself; eu_supervisory_authorities derive low effective extraction as administering beneficiaries. non_eu_controllers and small_non_eu_exporters sit near the target pole (high d), with exit constrained rather than arbitrage-grade, so their effective extraction is amplified rather than damped; the SME seat bears the sharper per-unit burden because fixed costs are regressive. adequacy_jurisdiction_firms occupy an intermediate position — nominally targeted, materially cushioned — which the derivation approximates from their dual beneficiary/payer declaration. non_eu_governments are excluded rather than seated: their objection is recorded as absence (Q4), not as a classified relationship. Gain receipt is concentrated enough to name: the monetary form of the imposed costs passes disproportionately through the compliance sector's invoices, so gain_flow names privacy_compliance_industry rather than asserting diffuse dispersal; fine revenue to member budgets and protection value to residents are real but secondary and unquantified flows. fixing_cost is prohibitive for the only actor that could remove the trigger — the EU co-legislature — for whom removal would forfeit standard-setting leverage and domestic coalition support worth far more than the relief foreign controllers would gain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — border-blind enforcement against offshore processing — is attested live by parties outside the beneficiary set, so the arrangement is not mandatrophy-resolved and carries no sunset. The drift risk runs the other way: theater_ratio climbing from 0.18 to 0.32 marks consent-layer ritual displacing protective substance, and the compliance sector's receipt position gives the machinery a constituency that profits from stringency itself. If protection outcomes flattened while enforcement and compliance spend kept growing, the arrangement would be drifting toward maintenance of the apparatus rather than the mission — the mismatch the lifecycle detectors exist to catch. Classification discipline cuts both ways here: naming the coordination function keeps the arrangement from being misread as pure extraction despite the named receipt seat, and naming the receipt seat keeps it from being misread as pure coordination despite the vendor constituency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_3_kernel_reading_indexicality,
    'Which reading of the Article 3(2) kernel does a given assessment instantiate — effects-based jurisdiction (this file), conditional market access, or territorial bound — and does the assessor''s choice change the constraint''s victim set and epsilon?',
    'Conceptual: fix the normative frame before scoring — ask whether the burden is treated as jurisdiction properly asserted over effects (this reading), as a voluntary price of market entry (market_access_reading), or as an authority excess (territorial_sovereignty_reading); the compiled family permits per-reading comparison.',
    'Under the market-access sibling the burden is self-selected and the victim set thins toward firms that chose entry; under the territorial sibling the entire arrangement lacks legitimacy and epsilon is read against an illegitimate imposition; this file''s numbers hold only for the effects reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_3_kernel_reading_indexicality, conceptual, 'Committer-frame omega: this constraint is one reading of the gdpr_article_3_scope kernel; sibling readings restructure the victim set and epsilon.').

omega_variable(
    targeting_monitoring_test_boundary,
    'Where does the targeting/monitoring test actually bite — does incidental accessibility to EU users count, or only intentional offering and systematic behavioural monitoring?',
    'EDPB guidance and accumulating case law (website-targeting decisions, cross-border behavioural advertising cases) progressively fix the test''s perimeter; a structured sample of enforcement decisions would map the operative boundary.',
    'A wide boundary pulls large numbers of marginal non-EU sites into scope and raises effective extraction sharply; a narrow boundary confines the burden to deliberate entrants and lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_monitoring_test_boundary, empirical, 'Empirical ambiguity in the trigger test''s perimeter drives how many foreign controllers are actually captured.').

omega_variable(
    brussels_effect_convergence_trajectory,
    'Does extraterritorial application converge global practice onto EU norms (turning the differential burden into a sunk universal cost), or do rival regimes entrench divergence?',
    'Track adoption of GDPR-analogous statutes and the geographic distribution of compliance spend over the next decade; convergence shows up as non-EU domestic laws mirroring the EU trigger.',
    'Full convergence would shrink the EU-specific differential and migrate the arrangement toward a common standard with residual EU enforcement advantage; durable divergence preserves the differential burden indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brussels_effect_convergence_trajectory, empirical, 'Whether the reading''s extraterritorial reach globalizes the standard or provokes durable rival regimes.').

omega_variable(
    adequacy_mechanism_dual_character,
    'Is the adequacy mechanism a proportionate facilitator lowering costs for well-regulated third countries, or a coercive bargaining lever whose suspension threat extracts concessions beyond the statute''s text?',
    'Compare compliance-cost trajectories for firms under stable adequacy decisions versus firms in suspended or contested-status countries (the post-Schrems II US experience); suspension episodes reveal the lever''s coercive range.',
    'If predominantly coercive, measured suppression understates the arrangement''s hold and the adequacy seat''s cushioning is contingent hostage-taking; if predominantly facilitative, the mechanism belongs to the coordination side of the ledger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_mechanism_dual_character, empirical, 'Dual character of the adequacy lever: facilitation versus conditionality-backed leverage.').

omega_variable(
    representation_deficit_inherent_or_remediable,
    'Is the absence of non-EU parties from rule-making an inherent property of any effects-based jurisdiction (every such doctrine regulates foreigners without enfranchising them), or a remediable design gap solvable by consultation and mutual-recognition structures?',
    'Comparative doctrine: examine whether established effects doctrines (antitrust, securities extraterritoriality) developed voice mechanisms, and whether proposed consultation channels for affected third-country stakeholders change burden distribution.',
    'If inherent, the transfer this reading counts is constitutive of effects jurisdiction as such and cannot be designed away without abandoning the reading; if remediable, part of the measured extraction is a policy choice separable from the reading''s core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representation_deficit_inherent_or_remediable, conceptual, 'Whether the no-obligation-without-participation objection is intrinsic to effects jurisdiction or a fixable institutional deficit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gdpr_tr_t0, observed).
narrative_ontology:measurement(gdpr_tr_t1, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 1, 0.21).
narrative_ontology:measurement_basis(gdpr_tr_t1, observed).
narrative_ontology:measurement(gdpr_tr_t2, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2, 0.24).
narrative_ontology:measurement_basis(gdpr_tr_t2, observed).
narrative_ontology:measurement(gdpr_tr_t3, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 3, 0.26).
narrative_ontology:measurement_basis(gdpr_tr_t3, observed).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(gdpr_tr_t4, observed).
narrative_ontology:measurement(gdpr_tr_t5, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement_basis(gdpr_tr_t5, observed).
narrative_ontology:measurement(gdpr_tr_t6, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement_basis(gdpr_tr_t6, observed).
narrative_ontology:measurement(gdpr_tr_t7, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 7, 0.31).
narrative_ontology:measurement_basis(gdpr_tr_t7, observed).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(gdpr_tr_t8, observed).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(gdpr_be_t0, observed).
narrative_ontology:measurement(gdpr_be_t1, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 1, 0.47).
narrative_ontology:measurement_basis(gdpr_be_t1, observed).
narrative_ontology:measurement(gdpr_be_t2, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2, 0.51).
narrative_ontology:measurement_basis(gdpr_be_t2, observed).
narrative_ontology:measurement(gdpr_be_t3, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement_basis(gdpr_be_t3, observed).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.56).
narrative_ontology:measurement_basis(gdpr_be_t4, observed).
narrative_ontology:measurement(gdpr_be_t5, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(gdpr_be_t5, observed).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement_basis(gdpr_be_t6, observed).
narrative_ontology:measurement(gdpr_be_t7, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 7, 0.61).
narrative_ontology:measurement_basis(gdpr_be_t7, observed).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(gdpr_be_t8, observed).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(gdpr_su_t0, observed).
narrative_ontology:measurement(gdpr_su_t1, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 1, 0.39).
narrative_ontology:measurement_basis(gdpr_su_t1, observed).
narrative_ontology:measurement(gdpr_su_t2, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2, 0.43).
narrative_ontology:measurement_basis(gdpr_su_t2, observed).
narrative_ontology:measurement(gdpr_su_t3, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 3, 0.47).
narrative_ontology:measurement_basis(gdpr_su_t3, observed).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement_basis(gdpr_su_t4, observed).
narrative_ontology:measurement(gdpr_su_t5, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(gdpr_su_t5, observed).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement_basis(gdpr_su_t6, observed).
narrative_ontology:measurement(gdpr_su_t7, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 7, 0.56).
narrative_ontology:measurement_basis(gdpr_su_t7, observed).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(gdpr_su_t8, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_chapter_v_transfer_restrictions).

% DUAL FORMULATION NOTE:
% The colloquial label 'GDPR extraterritoriality' decomposes into three structurally distinct claims about the same Article 3(2) text: whether the authority asserted is effects-based jurisdiction (this file), conditional market access (market_access_reading), or an exceedance of territorial sovereignty (territorial_sovereignty_reading). Each carries its own epsilon, victim set, and classification; they are linked here as one constraint family. The effects reading stands upstream of the market-access reading in discourse: as enforcement intensifies, the market-access description gains descriptive accuracy without resolving the doctrinal dispute. The chapter V transfer restrictions are causally coupled — adequacy decisions and transfer tools are the machinery through which the extraterritorial trigger reaches data flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
