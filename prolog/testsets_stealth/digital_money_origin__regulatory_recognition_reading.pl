% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Monetary Recognition Perimeter (Regulatory-Incorporation Origin Reading)
 *   domain: economic/institutional/technological
 *
 * SUMMARY:
 *   Under the regulatory-recognition reading, digital money's emergence is
 *   dated to the moment monetary authorities formally incorporated
 *   non-physical instruments into statistical aggregates and regulatory
 *   frameworks — the drawing of a codified monetary perimeter. The standing
 *   arrangement under contest is that perimeter itself: the operative rule
 *   that a monetary instrument counts, for statistics, policy, and legality,
 *   only once incorporated. The reading's own lights treat recognition as the
 *   constitutive act; the authored metrics describe the perimeter's actual
 *   operation — a genuine, heavily-used measurement and supervision function,
 *   actively enforced, with asymmetric incidence falling on unincorporated
 *   issuers. The claim and the metrics are authored independently: the engine
 *   computes per-seat classifications from the structural data, and any
 *   divergence between the claimed type and computed seat-level types is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - - central_banking_authorities: Agenda-setter (institutional/arbitrage) — draws and administers the recognition boundary; collects informational control over the money stock
 *   - - incumbent_chartered_banks: Primary beneficiary (powerful/identity_locked) — charter franchise protected by the boundary; defends its shape
 *   - - licensed_fintech_emoney_institutions: Incorporated beneficiary-payer (organized/constrained) — purchases legitimacy at compliance rates sized for larger balance sheets
 *   - - unregulated_emoney_issuers: Primary target (moderate/trapped) — issues money-like balances without recognition; denied durable banking access
 *   - - decentralized_crypto_protocol_developers: Secondary target (moderate/constrained) — builds off-perimeter issuance; chokepointed at regulated fiat interfaces
 *   - - community_alternative_currency_operators: Diffuse target (powerless/trapped) — local exchange schemes surviving by remaining small enough to be ignored
 *   - - retail_depositors: Protected beneficiary (powerless/constrained) — receives insurance and confidence; pays indirectly through fees and foregone innovation
 *   - - heterodox_free_banking_economists: Excluded critic (moderate/analytical) — denies the definitional premise itself; holds no seat in perimeter-setting
 *   - - international_standard_setting_bodies: Observer (institutional/analytical) — maps perimeter gaps across jurisdictions; recommends harmonization without granting recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.66).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.64).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Monetary Recognition Perimeter (Regulatory-Incorporation Origin Reading)").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "economic/institutional/technological").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '10b1b751-e8f0-48a4-8571-a7ba30a3e723').
narrative_ontology:cs_kernel_codification('10b1b751-e8f0-48a4-8571-a7ba30a3e723', formalized).
narrative_ontology:cs_authority_grounding('10b1b751-e8f0-48a4-8571-a7ba30a3e723', expertise).
narrative_ontology:cs_interpretation_layer_present('10b1b751-e8f0-48a4-8571-a7ba30a3e723').
narrative_ontology:cs_reading_relation('10b1b751-e8f0-48a4-8571-a7ba30a3e723', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('10b1b751-e8f0-48a4-8571-a7ba30a3e723', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('10b1b751-e8f0-48a4-8571-a7ba30a3e723', foundational, official_recognition_constitutes_money).
narrative_ontology:cs_axiom_status(official_recognition_constitutes_money, holdable).
narrative_ontology:cs_axiom_grounding('10b1b751-e8f0-48a4-8571-a7ba30a3e723', official_recognition_constitutes_money, conventional).
narrative_ontology:cs_axiom('10b1b751-e8f0-48a4-8571-a7ba30a3e723', secondary, statistical_visibility_precedes_monetary_existence).
narrative_ontology:cs_axiom_status(statistical_visibility_precedes_monetary_existence, holdable).
narrative_ontology:cs_axiom_grounding('10b1b751-e8f0-48a4-8571-a7ba30a3e723', statistical_visibility_precedes_monetary_existence, instrumental).
narrative_ontology:cs_reference_frame('10b1b751-e8f0-48a4-8571-a7ba30a3e723', codified_monetary_perimeter).
narrative_ontology:cs_drift_state('10b1b751-e8f0-48a4-8571-a7ba30a3e723', contemporary_stablecoin_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('10b1b751-e8f0-48a4-8571-a7ba30a3e723', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, central_banking_authorities).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_chartered_banks).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, licensed_fintech_emoney_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, retail_depositors).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_emoney_issuers).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, decentralized_crypto_protocol_developers).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, community_alternative_currency_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, licensed_fintech_emoney_institutions).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, monetary_statistics_completeness_doctrine).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, supervisory_perimeter_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and maintain the statistical aggregates and licensing categories through which non-physical instruments become official money. They publish the aggregates, grant or withhold recognition, and adjust category boundaries through consultation processes. They bear little direct cost from the boundary they administer and can redraw it by regulation; their measurement and policy functions depend on the boundary staying determinate.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, central_banking_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate inside the perimeter under charters that the recognition boundary protects: deposit franchises, settlement access, and a compliance burden that falls proportionally harder on would-be competitors. Compliance costs are familiar overhead absorbed at scale. Leaving the perimeter would mean surrendering the charter that constitutes their business, so they do not exit — they defend the boundary's shape through consultation participation and supervised relationships with entrants.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_chartered_banks, beneficiary,
    powerful, generational, identity_locked, continental).

% Chose incorporation: they hold e-money or payment licenses, submit to capital and safeguarding rules, and gain legal legitimacy and banking-sector partnerships in return. The license terms were written around incumbent balance sheets, so compliance consumes a disproportionate share of their revenue. Renouncing the license would forfeit the legitimacy they paid for, so they stay and lobby at the margin.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, licensed_fintech_emoney_institutions, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, licensed_fintech_emoney_institutions, payer).

% Issue stored-value or wallet balances without incorporation. They cannot open durable accounts with regulated banks, cannot present themselves as money in contracts or marketing, and face abrupt closure of banking relationships. Incorporation remains formally available but on terms sized for institutions far larger than they are, so the available exit would transform them into something else.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_emoney_issuers, payer,
    moderate, biographical, trapped, national).

% Build issuance and transfer systems that run globally without a licensing address. Their chokepoint is the fiat interface: on-ramps and off-ramps run through regulated institutions that sever ties under supervisory pressure. Relocating code is easy; relocating liquidity and users is not, so their practical freedom ends at the perimeter's edge.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, decentralized_crypto_protocol_developers, payer,
    moderate, biographical, constrained, global).

% Run local mutual-credit or time-banking schemes that function as media of exchange at neighborhood scale. They lack the resources to engage rulemaking, are occasionally threatened with action for unauthorized issuance, and survive by staying small enough to remain beneath supervisory attention.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, community_alternative_currency_operators, payer,
    powerless, biographical, trapped, local).

% Hold accounts inside the perimeter and receive deposit insurance, payment-protection rules, and confidence in the official money form. They pay indirectly through fees passed through and through innovations that never launch because compliance outweighs the addressable market. Opting out of the official money form entirely is impractical for ordinary economic life.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, retail_depositors, beneficiary,
    powerless, biographical, constrained, national).

% Study monetary systems in which money emerges from use rather than recognition, and argue the definitional premise of the perimeter is wrong. They publish, testify occasionally, and hold no decision seat in perimeter-setting; their objection is to the category boundary itself, not to any particular rule inside it.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, heterodox_free_banking_economists, excluded,
    moderate, generational, analytical, global).

% Compile cross-country comparisons of perimeter definitions, flag gaps where money-like instruments escape measurement, and recommend harmonization. They shape the climate in which national authorities redraw boundaries without themselves granting or withholding recognition.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, international_standard_setting_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__regulatory_recognition_reading, incumbent_chartered_banks).
narrative_ontology:fixing_cost_class(digital_money_origin__regulatory_recognition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces a single official answer to the question of what currently counts as money: statistical agencies can sum a money supply, central banks can target it, supervisors can bound who may issue claims redeemable in it, and payment systems share one settlement asset.
% TRANSFER_FUNCTION: Moves legal legitimacy, central-bank account access, and market credibility toward incorporated institutions; moves compliance costs, capital and AML burdens, and exclusion risk onto unincorporated issuers; concentrates informational control over the money stock in the statistical apparatus.
% ABSENT_VOICES: Free-banking and heterodox monetary economists, who deny that state recognition is constitutive of money and would contest the definitional premise itself, hold no seat in perimeter-setting consultations; users of informal community currencies and offshore issuers likewise have no domestic representation in the frameworks that classify them.
% DISAPPEARANCE_RATIONALE: If the recognition boundary vanished overnight, official money aggregates would lose determinate content, policy targets would lose their object, incumbents would lose the charter moat the perimeter maintains, and unincorporated instruments would compete openly for the money role — the monetary system would reorganize around whatever settlement conventions emerged from that competition.
% FOUNDING_PROBLEM: Late-twentieth-century monetary authorities confronted proliferating non-physical instruments — stored-value cards, early e-money, later crypto assets — that circulated as money-like claims while escaping aggregate measurement and supervisory reach, threatening both policy control and stability oversight.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic monetary-history scholarship documenting the recurring historical pattern of privately issued money prompting regulatory incorporation, and market data showing large stablecoin balances held outside banking perimeters; international-body reports acknowledge continuing measurement gaps. Incumbent institutions also attest the problem, but the external academic and market sources stand independently of them.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.66 at interval end: compliance obligations are sized to incumbent balance sheets rather than issuer size, licensing walls raise rivals' costs, and incorporation on offered terms frequently restructures the entrant's model. It is not higher because the measurement function is real and load-bearing — the aggregates are computed, published, and consumed by policy. Suppression is 0.64 as a raw structural property (licensing gates, AML/KYC obligations, and denial of banking access to non-incorporated issuers); it is deliberately unscaled — only extractiveness is scaled by directionality and scope in the engine's computation. Theater ratio is 0.29: consultation and impact-assessment rituals increasingly decorate decisions made elsewhere, but the statistical work is functional. Accessibility_collapse is 0.48 — off-perimeter issuance remains possible (crypto rails, offshore domiciles, informal schemes) but is costly and legally hazardous, so alternatives narrow without vanishing; this is nowhere near mountain-grade collapse. Resistance is 0.55 — sustained fintech and crypto advocacy, jurisdictional arbitrage, and litigation. All three tracked series run on one shared six-point grid (t=0,8,16,24,32,40) so every metric is authored at every examined time point; the rising suppression_requirement series is intentional, as the story specifically traces the build-out of enforcement capacity (licensing regimes, AML expansion, de-risking practice) across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the perimeter is the act that makes money governable — without it there is no money supply to target and no supervisor's map. From the incumbent beneficiary seat it is franchise protection: a boundary that converts supervisory necessity into a charter moat. From the licensed-fintech seat it is a toll worth paying for legitimacy. From the unincorporated payer seats it is a wall: the same rules that protect depositors exclude them from banking access entirely. From the excluded heterodox-economist seat it is a category error — money defined by recognition rather than use. The engine derives these divergences from power, exit, and directional position; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: central_banking_authorities and incumbent_chartered_banks sit near the beneficiary end (low d), with the banks' identity_locked exit pinning them inside the arrangement they defend; the three victim groups sit near the target end, with trapped exits (unregulated issuers, community operators) pushing them toward full-target and constrained exits (crypto developers) slightly less far. One override is declared: licensed_fintech_emoney_institutions at power atom 'organized' to d=0.42. The derivation from their beneficiary role would place them near d≈0.15–0.2, but their actual net position is close to symmetric — they pay heavy compliance costs calibrated to incumbent balance sheets and accept terms they did not set, against legitimacy gains they genuinely collect. Retail depositors' residual imprecision (real protection against indirect costs) is left to the derivation, which already separates them from identically-powered trapped victims via exit modulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — instruments circulating as money while escaping measurement and supervision — recurs with each technology wave (stored value, e-money, crypto, stablecoins), so the founding problem status is live and no mandatrophy resolution is declared. The classification discipline prevents two opposite mislabels: a pure-snare verdict would erase the genuine, heavily-consumed measurement function that policy cannot do without; a pure-rope verdict would erase the asymmetric incidence — compliance scaled to incumbents, exclusion enforced against non-payers. Tangled rope holds both halves. A watch condition: if a future wave (e.g., central-bank issuance of the very instrument class being measured) makes the perimeter self-referential, the coordination half could decay faster than the enforcement half, dating a transition toward theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_origin_date_indexicality,
    'This story instantiates the regulatory_recognition_reading of the digital_money_origin kernel: is ''emergence'' properly dated at formal incorporation into statistical aggregates and regulatory frameworks, at technical and institutional conceivability (became_thinkable_reading), or at first practical holding of non-physical instruments (first_held_reading)?',
    'Cross-story comparison of the three sibling constraint files: whichever reading''s barrier set best predicts the observed structure (legal/regulatory vs cognitive/conceptual vs custody/trust) and the observed beneficiary/victim alignment wins on fit; the corpus retains all three as rival indexicals rather than adjudicating by fiat.',
    'Adopting a sibling reading re-dates the origin, swaps the operative barrier set, and reassigns beneficiaries and victims — this file''s perimeter-extraction structure would be replaced by a conceivability-gate or custody-trust structure with a different epsilon and different stakeholder surface.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_origin_date_indexicality, conceptual, 'Indexical ambiguity of ''emergence'' across the three readings of the digital_money_origin kernel.').

omega_variable(
    constitutive_vs_epistemic_recognition,
    'Does formal recognition constitute money (an instrument is not money until incorporated) or merely register money that already functions (recognition as epistemic admission of pre-existing monetary fact)?',
    'Examine instruments that served as media of exchange, units of account, or stores of value before any incorporation event (stored-value cards, in-game currencies, early e-money ledgers): if they performed monetary functions at scale prior to recognition, recognition is epistemic rather than constitutive.',
    'Under the constitutive branch the perimeter creates the category and pre-boundary extraction is nil; under the epistemic branch the perimeter extracts from instruments that were already money, extending victimhood backward in time and raising effective epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_epistemic_recognition, conceptual, 'Whether the reading''s core premise is ontological (recognition constitutes money) or epistemic (recognition registers money).').

omega_variable(
    perimeter_capture_direction,
    'Was the perimeter drawn primarily to restore measurement and supervisory completeness (public function), or shaped by incumbent institutions converting supervisory goals into entry barriers (private benefit)?',
    'Rulemaking-trail analysis: compare comment letters from incumbents versus entrants on e-money, payment-services, and stablecoin consultations against the provisions actually adopted; measure adoption correlation by commenter class.',
    'High incumbent-adoption correlation pushes the arrangement toward snare (the coordination story as cover); low correlation supports the tangled_rope reading with the coordination function dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perimeter_capture_direction, empirical, 'Public-measurement versus incumbent-capture explanation of the perimeter''s shape.').

omega_variable(
    derisking_suppression_attribution,
    'Is the account-denial and correspondent-withdrawal pressure on unincorporated issuers authored by the regulatory constraint itself, or chosen by private banks under prudential discretion the constraint merely permits?',
    'Compare de-risking intensity across jurisdictions with materially identical incorporation rules but different supervisory guidance and examination practice; inspect supervisory letters for explicit or implied expectations.',
    'If supervisory expectation drives the denials, the suppression belongs to the constraint and effective suppression rises; if purely private, the constraint''s suppression is lower and the harm is a permitted externality rather than enforced exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derisking_suppression_attribution, empirical, 'Attribution of de-risking harms between the regulatory rule and private bank choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__regulatory_recognition_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(digi_tr_t0, observed).
narrative_ontology:measurement(digi_tr_t8, digital_money_origin__regulatory_recognition_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(digi_tr_t8, observed).
narrative_ontology:measurement(digi_tr_t16, digital_money_origin__regulatory_recognition_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(digi_tr_t16, observed).
narrative_ontology:measurement(digi_tr_t24, digital_money_origin__regulatory_recognition_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement_basis(digi_tr_t24, observed).
narrative_ontology:measurement(digi_tr_t32, digital_money_origin__regulatory_recognition_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement_basis(digi_tr_t32, observed).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__regulatory_recognition_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(digi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(digi_be_t0, observed).
narrative_ontology:measurement(digi_be_t8, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement_basis(digi_be_t8, observed).
narrative_ontology:measurement(digi_be_t16, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement_basis(digi_be_t16, observed).
narrative_ontology:measurement(digi_be_t24, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(digi_be_t24, observed).
narrative_ontology:measurement(digi_be_t32, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement_basis(digi_be_t32, observed).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(digi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(digi_su_t0, observed).
narrative_ontology:measurement(digi_su_t8, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement_basis(digi_su_t8, observed).
narrative_ontology:measurement(digi_su_t16, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement_basis(digi_su_t16, observed).
narrative_ontology:measurement(digi_su_t24, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(digi_su_t24, observed).
narrative_ontology:measurement(digi_su_t32, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement_basis(digi_su_t32, observed).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement_basis(digi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the emergence of digital money' per the epsilon-invariance principle. The label conflates three structurally distinct claims: (1) became_thinkable_reading — emergence at technical/institutional conceivability, earliest date, cognitive/conceptual barrier set; (2) first_held_reading — emergence at first practical holding of non-physical instruments, intermediate date, custody/trust barrier set; (3) this file, regulatory_recognition_reading — emergence at formal incorporation into aggregates and frameworks, latest date, legal/regulatory barrier set with incumbent beneficiaries and unregulated-innovator victims. Each story carries its own epsilon, stakeholders, and claimed type; the upstream readings are typically cited as background for the recognition claim, so this file links to both siblings. Measuring 'emergence' with different observables yields different epsilon values precisely because they are different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__regulatory_recognition_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
