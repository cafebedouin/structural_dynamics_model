% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__consumer_holdings_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Consumer-Holdings Boundary on Digital Money Emergence
 *   domain: economic/historiographical/regulatory
 *
 * SUMMARY:
 *   The consumer-holdings reading fixes the emergence of digital money at the
 *   point when individuals could directly hold and transact with digital
 *   instruments outside traditional bank accounts — the 1990s e-purse wave,
 *   consolidated legally by the 2000 Electronic Money Directive. As a
 *   constraint, the boundary operates on three surfaces at once: monetary
 *   statistics (it necessitates separating bank deposits from e-money in the
 *   aggregates), licensing law (it draws the perimeter within which non-bank
 *   issuance of individually held electronic value is permitted), and
 *   historiography (it dates the beginning, demoting everything earlier to
 *   prehistory). The epsilon referent is the standing arrangement under
 *   contest — the consumer-holdings boundary as it actually operates in
 *   statistical manuals, directives, and official histories — assessed by
 *   this reading's own lights: the reading endorses the criterion as the
 *   correct one, and still authors honest, non-zero extraction for how the
 *   boundary distributes authority and credit in operation. Per the
 *   epsilon-invariance principle, the colloquial label 'the emergence of
 *   digital money' decomposes into a three-story family: this file
 *   (holdability), the conceptualization reading (thinkability, 1960s-1985
 *   Chaum formalization), and the infrastructure reading (transfer
 *   capability, 1967 ATM / 1972 ACH / 1977 SWIFT). Each is a separate
 *   constraint with its own epsilon, beneficiaries, and victims; they are
 *   linked via network.affects_constraints. The claim/metric gap is
 *   deliberate: the boundary is CLAIMED as tangled_rope from its structure
 *   (real statistical coordination plus asymmetric, enforcement-backed credit
 *   and perimeter allocation), while the metrics independently describe its
 *   operation — the engine measures any divergence.
 *
 * KEY AGENTS:
 *   - emi_ecb_monetary_authorities: Agenda-setter and primary beneficiary (institutional/identity_locked) — administers the statistical and legal categories, publishes the histories in which the emergence date appears, collects definitional authority
 *   - fintech_emoney_issuers: Beneficiary (organized/mobile) — the category legitimizes their products as individually held electronic money under a lighter perimeter than deposit-taking
 *   - monetary_economists: Secondary beneficiary (moderate/mobile) — consume the deposit/e-money separation as a workable measurement scheme without administering it
 *   - payments_infrastructure_historians: Payer (moderate/constrained) — the ATM/ACH/SWIFT decades are refiled as infrastructure prehistory rather than monetary history
 *   - cryptographic_cash_pioneers: Payer (moderate/identity_locked) — 1980s blind-signature and ecash work is demoted from origin to anticipation
 *   - bank_deposit_account_holders: Payer (powerless/trapped) — decades of electronic banking register as use of bank money, not digital money; no account escapes the classification
 *   - unbanked_underbanked_populations: Excluded (powerless/trapped) — the boundary's defining clause is effectively written about them, yet they sit in none of the venues where the line is drawn or debated
 *   - bis_payment_statistics_community: Analytical observer (institutional/analytical) — compiles cross-country statistics on the categories, documents inconsistencies, pressures definitions through harmonization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.58).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.55).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Consumer-Holdings Boundary on Digital Money Emergence").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "economic/historiographical/regulatory").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, 'c7e72d80-9387-4e36-8a26-c469834c92d1').
narrative_ontology:cs_kernel_codification('c7e72d80-9387-4e36-8a26-c469834c92d1', distributed).
narrative_ontology:cs_authority_grounding('c7e72d80-9387-4e36-8a26-c469834c92d1', expertise).
narrative_ontology:cs_interpretation_layer_present('c7e72d80-9387-4e36-8a26-c469834c92d1').
narrative_ontology:cs_reading_relation('c7e72d80-9387-4e36-8a26-c469834c92d1', digital_money_emergence_boundary__conceptualization_reading, forecloses).
narrative_ontology:cs_reading_relation('c7e72d80-9387-4e36-8a26-c469834c92d1', digital_money_emergence_boundary__infrastructure_reading, forecloses).
narrative_ontology:cs_axiom('c7e72d80-9387-4e36-8a26-c469834c92d1', foundational, individual_holdability_constitutes_digital_money).
narrative_ontology:cs_axiom_status(individual_holdability_constitutes_digital_money, holdable).
narrative_ontology:cs_axiom_grounding('c7e72d80-9387-4e36-8a26-c469834c92d1', individual_holdability_constitutes_digital_money, conventional).
narrative_ontology:cs_axiom('c7e72d80-9387-4e36-8a26-c469834c92d1', secondary, bank_deposits_excluded_from_digital_money_category).
narrative_ontology:cs_axiom_status(bank_deposits_excluded_from_digital_money_category, holdable).
narrative_ontology:cs_axiom_grounding('c7e72d80-9387-4e36-8a26-c469834c92d1', bank_deposits_excluded_from_digital_money_category, instrumental).
narrative_ontology:cs_reference_frame('c7e72d80-9387-4e36-8a26-c469834c92d1', consumer_holdability_constitutes_money).
narrative_ontology:cs_drift_state('c7e72d80-9387-4e36-8a26-c469834c92d1', post_cryptocurrency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c7e72d80-9387-4e36-8a26-c469834c92d1', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, emi_ecb_monetary_authorities).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_emoney_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, payments_infrastructure_historians).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, cryptographic_cash_pioneers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, bank_deposit_account_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, monetary_economists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produced the 1994 report on stored-value cards, carried the e-money file into the 2000 Electronic Money Directive, and maintains the statistical separation between bank deposits and electronic money in its aggregates. Publishes the periodic histories in which the emergence date appears. Redrawing the line now would put its own multi-decade publication record in question, so the institution treats the category as settled method. Collects the authority to decide what counts as money for statistical and supervisory purposes across the euro area and, by example, beyond.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, emi_ecb_monetary_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, emi_ecb_monetary_authorities, beneficiary).

% Issue prepaid cards, wallets, and payment tokens under the e-money category the boundary defines. The category lets them offer individually held electronic value without a full banking license, and lets stablecoin projects seek the same designation. They fund industry associations and consultations defending the category's usefulness. If the line moved toward bank-transfer infrastructure or theoretical conception, their product class would lose its distinct legal identity and part of its market legitimacy.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_emoney_issuers, beneficiary,
    organized, biographical, mobile, global).

% Use the deposit-versus-e-money separation when constructing liquidity aggregates and studying substitution between bank money and electronic instruments. They receive a workable measurement scheme without administering it, and can switch to flow data or alternative aggregates if the separation dissolves.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_economists, beneficiary,
    moderate, biographical, mobile, global).

% Document the ATM, ACH, and SWIFT decades: network builds, clearing-house conversions, card rails. Under the operative dating, their material falls before digital money's beginning and is filed as infrastructure prehistory. Reframing their work as monetary history means arguing against the categories their sources are catalogued under; moving to a later period means abandoning accumulated archive expertise.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, payments_infrastructure_historians, payer,
    moderate, generational, constrained, global).

% Formalized blind signatures and ecash in the 1980s and built early anonymous digital cash prototypes. Their professional standing and priority claims rest on digital cash being conceived and first realized in that work. The operative dating places the beginning two decades later, casting their output as anticipation rather than origin. Leaving the field or renouncing the priority narrative is the only exit.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, cryptographic_cash_pioneers, payer,
    moderate, biographical, identity_locked, global).

% Hold balances at banks and move them by card, transfer, and app. Under the operative category those balances are bank liabilities, so their decades of electronic payment use register as use of bank money rather than digital money. No fee attaches to the classification and no alternative account escapes it, since every deposit account sits on the same side of the line.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, bank_deposit_account_holders, payer,
    powerless, biographical, trapped, global).

% Already hold value outside bank accounts — cash, informal savings, in some regions mobile wallets. The boundary's defining clause about holding outside traditional bank accounts is effectively written about people like them, yet they appear in none of the statistical, legal, or historiographical venues where the line is drawn, dated, or defended.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_underbanked_populations, excluded,
    powerless, immediate, trapped, global).

% Compiles cross-country payment and e-money statistics and publishes comparative red-book volumes. Takes the categories as inputs, documents inconsistencies between national implementations, and occasionally pressures definitions through harmonization exercises. Neither collects nor pays under the boundary; its seat is analytical.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, bis_payment_statistics_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__consumer_holdings_reading, emi_ecb_monetary_authorities).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__consumer_holdings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single consistent criterion for what counts as digital money, enabling comparable monetary statistics that separate bank deposits from e-money, a defined regulatory perimeter for non-bank issuance of individually held electronic value, and a common reference for consumer-protection scope.
% TRANSFER_FUNCTION: Moves definitional authority and historical narrative credit toward the monetary authorities who administer the categories and the issuers whose products the category legitimizes; moves discursive standing away from infrastructure-era actors, cryptographic-cash pioneers, and bank-deposit users whose electronic payment use is reclassified as not-digital-money.
% ABSENT_VOICES: Defunct e-purse operators and the unbanked populations the 'outside traditional bank accounts' clause implicitly references are not in the conversation; infrastructure-era engineers mostly retired without contesting the periodization; dissent survives mainly in specialist historiography rather than in the statistical and legal venues where the boundary is maintained.
% DISAPPEARANCE_RATIONALE: If the consumer-holdings boundary vanished overnight, e-money licensing would lose its conceptual anchor and the perimeter would need redrawing around function or risk instead; monetary aggregates would need redefinition; and the historiography of digital money would reopen, with infrastructure and conceptualization datings competing on equal footing again.
% FOUNDING_PROBLEM: Late-1980s and 1990s policymakers faced proliferating stored-value cards and electronic purses with no settled answer to whether issuing them constituted deposit-taking requiring a banking license; the boundary was built to give supervisors a workable line between bank money and new electronic instruments.
% FOUNDING_PROBLEM_CORROBORATION: Contemporaneous supervisory literature outside the benefiting parties — the BIS/CPSS 1996 report on security of electronic money, national supervisory consultation papers, and late-1990s legal scholarship on stored-value regulation — attests that the licensing-classification problem was real and pressing. No source outside the benefiting parties currently attests that the founding problem remains live; the corroborating record supports the dead status.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.58: the boundary moves no funds directly; what it moves is definitional authority, narrative credit, and perimeter control — real, career- and jurisdiction-bearing, but softer than fiscal extraction. The series accumulates from 1994 through the 2009 recast and the post-Libra scramble for e-money designations, then eases slightly as MiCA splits the perimeter and creates a parallel crypto-asset category. Suppression is 0.55 and is administrative-editorial rather than coercive: statistical manuals, licensing law, commissioned official histories, and venue gatekeeping; rival periodizations stay publishable in journals but not in the venues that allocate regulatory and statistical standing. The suppression_requirement series traces enforcement-capacity change — build-up through EMD transposition and the 2009 recast, plateau, then partial migration to MiCA — which is why it is authored despite a broadly static structural picture. Theater is 0.32 and rising: the deposit/e-money separation performed genuine statistical work for two decades, while a growing share of boundary-related activity is commemorative (anniversary histories, 'birth of digital money' narratives) even as several major central banks abandoned aggregate targeting altogether. Accessibility_collapse is 0.52 — alternatives collapse only inside official venues — and resistance is 0.55, sustained specialist contestation with no mass resistance because the harms are diffuse. All three tracked series run on one shared time grid (t=0..30 at steps of 5; t0 approximates 1994, tn approximates 2024), so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the boundary is methodology: a neutral line drawn for comparability, maintained by experts, revised only on evidence. From the payer seats the same structure is a credit-allocation machine: it dates their subject matter out of monetary existence, converts their priority claims into footnotes, and files four decades of depositor experience as pre-digital. The engine computes per-seat classifications from the structural data — the institutional seat with identity-locked exit and the trapped diffuse payer seat should classify differently, and that divergence is the measurement, not an inconsistency to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: fintech issuers (mobile exit, category legitimacy flows to them) derive low d; monetary economists (mobile, incidental analytic benefit) lower still. Payers sit near the target end: infrastructure historians (constrained exit — their archives bind them to the demoted period), cryptographic pioneers (identity_locked — exit means renouncing the priority narrative), and deposit account holders (trapped — every alternative account sits on the same side of the line, so the classification cannot be exited by switching). One override is authored: emi_ecb_monetary_authorities derives near-full-beneficiary d from its beneficiary declaration alone, but the seat also bears real enforcement costs, publication-record risk, and contestation management, so its net structural position is d=0.25 rather than the derived ~0.1. Unbanked populations are excluded rather than positioned: the clause that defines the boundary references holding outside bank accounts, yet they were never seated in the conversation that drew it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving supervisors a workable line for classifying 1990s stored-value and e-purse issuance against deposit-taking — is dead: the e-purse schemes largely failed commercially, the licensing question was resolved, and the current frontier (stablecoins, CBDC, crypto-assets) is governed by newer regimes such as MiCA. Yet the boundary persists and is actively maintained, with theater rising as the operative apparatus decays. The founding_problem_status x disappearance_verdict pair (dead + world_rearranges) is exactly the mismatch signature that flags zombie/capture tendency, and it is authored honestly rather than reconciled. The classification prevents mislabeling in both directions: calling this a rope ignores that the credit and perimeter allocation is asymmetric and enforcement-backed; calling it a snare ignores that the deposit/e-money separation delivered genuine statistical coordination for two decades and that one beneficiary seat (monetary economists) receives real analytic value without collecting rents. Tangled rope with rising theater tracks the mandatrophy drift the temporal series shows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is the consumer_holdings_reading of kernel digital_money_emergence_boundary — the claim that digital money''s emergence is fixed by direct individual holdability outside traditional bank accounts. Where exactly does the disagreement with the sibling readings sit?',
    'Locate the disputed element: the constitutive criterion for money-existence (theoretical thinkability vs. individual holdability vs. transfer-enabling infrastructure). Resolution requires the field to agree on what property of an instrument is constitutive of digital money, not on dates.',
    'Adopting a sibling''s criterion reshuffles the structural surface wholesale: under the infrastructure reading the beneficiary set shifts toward bank networks and payment processors and the payer set shifts toward consumer-side actors; under the conceptualization reading beneficiaries shift toward cryptographers and theory-building institutions. This story''s epsilon, beneficiaries, and victims are valid only under the holdability criterion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Committer structure: one reading of the emergence-boundary kernel; disagreement located in the constitutive criterion for money-existence.').

omega_variable(
    sibling_adoption_structural_delta,
    'What structurally changes if a sibling reading displaces this one as the operative boundary on digital money''s emergence?',
    'Counterfactual reclassification: re-run classification under each sibling''s criterion and diff the resulting beneficiary/victim/perimeter structures.',
    'Under the infrastructure reading, the deposit/e-money separation this reading necessitates becomes unnecessary (inter-bank electronic transfer already counts), collapsing the M4/M5 apparatus and converting this reading''s regulatory beneficiaries into bystanders. Under the conceptualization reading, the regulatory perimeter detaches from holdings entirely and e-money licensing loses its conceptual anchor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_adoption_structural_delta, conceptual, 'Structural delta if sibling readings displace this one as the operative boundary.').

omega_variable(
    constitutive_criterion_status,
    'Is the holdability criterion a discovered feature of what money is, or a stipulation adopted for supervisory and statistical convenience that was subsequently naturalized?',
    'Trace the criterion''s adoption history: if it entered through supervisory necessity (EMI 1994 stored-value report, EMD 2000) rather than prior monetary theory, the constructedness reading is supported; test whether monetary theory independently entails holdability as constitutive.',
    'If stipulated-then-naturalized, the boundary''s apparent analytic necessity drops and its concentration of definitional authority reads as constructed advantage rather than methodological requirement, pushing computed classification toward the extractive end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_criterion_status, conceptual, 'Constructed vs. discovered status of the holdability criterion.').

omega_variable(
    authority_expertise_extraction_gradient,
    'Does the adjudicating authority (central-bank statistical and supervisory apparatus) maintain the boundary for epistemic reasons, or does definitional stability deliver it institutional benefit — jurisdiction over what counts as money, publication authority, gatekeeping of the e-money perimeter?',
    'Compare the authority''s behavior on definitional disputes where it holds no institutional stake, and audit internal review records for interest-bearing reasoning in boundary maintenance decisions.',
    'If benefit-driven, the authority structure drifts from expertise toward extraction and the enforcement series reads as rent-defense rather than methodology maintenance, raising effective extraction for the agenda-setter seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_expertise_extraction_gradient, empirical, 'Expertise vs. extraction gradient in the adjudicating authority.').

omega_variable(
    aggregate_apparatus_obsolescence,
    'Many major central banks abandoned monetary-aggregate targeting after the 1990s; does the deposit/e-money separation this reading necessitates still bind any operative practice, or does the apparatus survive only ceremonially?',
    'Survey current central-bank statistical releases, legal texts, and supervisory filings for active operational reliance on the separation; measure citation and workflow dependence.',
    'If the apparatus is largely ceremonial, the constraint''s functional share falls and theater_ratio should be revised upward, weakening the coordination half of the structure and shifting computed classification toward piton-like inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aggregate_apparatus_obsolescence, empirical, 'Whether the necessitated aggregate-separation apparatus is operative or vestigial.').

omega_variable(
    victim_harm_materiality,
    'Are the harms borne by infrastructure historians, cryptographic-cash pioneers, and deposit account holders material (career, funding, regulatory-standing losses) or purely discursive (narrative placement), and does discursive harm alone sustain the asymmetric half of the structure?',
    'Track funding, citation, and regulatory-consultation access for the payer seats relative to beneficiary-aligned narratives across the interval.',
    'If harms are purely discursive and cheaply reversible, effective extraction falls and the structure reads closer to a coordination device with rhetorical asymmetry; if careers and perimeters materially track the boundary, the hybrid structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_harm_materiality, empirical, 'Materiality of the payer seats'' harms under the boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(digi_tr_t5, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(digi_tr_t10, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(digi_tr_t15, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(digi_tr_t20, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(digi_tr_t25, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(digi_tr_t30, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(digi_be_t5, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(digi_be_t10, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(digi_be_t15, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(digi_be_t20, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(digi_be_t25, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(digi_be_t30, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(digi_su_t5, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(digi_su_t10, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(digi_su_t15, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(digi_su_t20, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(digi_su_t25, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(digi_su_t30, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the emergence of digital money' covers three structurally distinct claims — thinkability (conceptualization_reading), transfer capability (infrastructure_reading), and individual holdability outside bank accounts (this file). Their epsilon values differ because their extraction surfaces differ: this reading concentrates extraction on narrative credit and regulatory perimeter around the latest boundary; the infrastructure reading would concentrate it on bank networks and card rails; the conceptualization reading on theory-building institutions and the cryptographic lineage. Authored as a three-story constraint family per the epsilon-invariance principle; every member links the others via network.affects_constraints, and upstream members typically get cited as evidence for downstream ones.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__consumer_holdings_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
