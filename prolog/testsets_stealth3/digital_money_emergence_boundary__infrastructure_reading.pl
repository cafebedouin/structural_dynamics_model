% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence Boundary — Infrastructure Reading (Rail-Transfer Origin)
 *   domain: economic/monetary-history/technology-governance
 *
 * SUMMARY:
 *   Between 1967 and 1977 the banking system acquired the machinery to move
 *   money electronically — cash dispensers (1967), automated clearing (1972),
 *   cross-border messaging (1977) — and the operative convention adopted by
 *   statisticians, supervisors, and industry historians dates digital money's
 *   origin to exactly that window. This story instantiates the
 *   infrastructure_reading of the contested kernel
 *   digital_money_emergence_boundary: the claim that money becomes digital
 *   when the banking system can transfer it, whether or not any consumer can
 *   yet hold a digital instrument directly. The convention performs real
 *   coordination — monetary statistics and supervisory perimeters need an
 *   operational date-line — while allocating definitional privilege to the
 *   institutions that own the rails and imposing novelty costs on non-bank
 *   issuers, whose products must enter through purpose-built licence
 *   categories rather than counting as money. Epsilon's referent is the
 *   standing arrangement under contest — the incumbent rail-dated taxonomy as
 *   actually compiled and enforced — assessed by this reading's own lights;
 *   the reading regards its criterion as correct, so it authors moderate-low
 *   extraction concentrated on the challenger seat. Claimed type and metrics
 *   were authored independently: the claim states my structural judgment, the
 *   metrics describe observed operation, and any divergence the engine
 *   computes is the datum the corpus exists to take.
 *
 * KEY AGENTS:
 *   - KEY AGENTS (by structural relationship):
 *   - - central_banks: Agenda setter (institutional / identity_locked) — compiles the aggregates, operates the settlement rails, writes the official dating
 *   - - banking_rail_operators: Primary beneficiary (institutional / arbitrage) — SWIFT, ACH and card-network cooperatives controlling the rails the definition celebrates
 *   - - retail_commercial_banks: Secondary beneficiary (powerful / constrained) — deposit franchises counted as money proper under the frame
 *   - - non_bank_digital_issuers: Primary target (moderate / constrained) — e-money, e-purse and stablecoin issuers gated into bespoke authorization categories
 *   - - monetary_statisticians: Analytical observer (analytical / analytical) — maintains the boundary methodologically, collects no rents
 *   - - retail_depositors: Incidental beneficiary (powerless / mobile) — gained ATM and payroll-deposit access without bearing the convention's costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.32).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.28).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence Boundary — Infrastructure Reading (Rail-Transfer Origin)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "economic/monetary-history/technology-governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '409fd4a4-472d-4e86-b72d-896164651d64').
narrative_ontology:cs_kernel_codification('409fd4a4-472d-4e86-b72d-896164651d64', distributed).
narrative_ontology:cs_authority_grounding('409fd4a4-472d-4e86-b72d-896164651d64', expertise).
narrative_ontology:cs_reading_relation('409fd4a4-472d-4e86-b72d-896164651d64', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_reading_relation('409fd4a4-472d-4e86-b72d-896164651d64', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('409fd4a4-472d-4e86-b72d-896164651d64', foundational, electronic_transferability_constitutes_digitality).
narrative_ontology:cs_axiom_status(electronic_transferability_constitutes_digitality, holdable).
narrative_ontology:cs_axiom_grounding('409fd4a4-472d-4e86-b72d-896164651d64', electronic_transferability_constitutes_digitality, conventional).
narrative_ontology:cs_axiom('409fd4a4-472d-4e86-b72d-896164651d64', secondary, operational_classifiability_precedes_held_experience).
narrative_ontology:cs_axiom_status(operational_classifiability_precedes_held_experience, holdable).
narrative_ontology:cs_axiom_grounding('409fd4a4-472d-4e86-b72d-896164651d64', operational_classifiability_precedes_held_experience, instrumental).
narrative_ontology:cs_reference_frame('409fd4a4-472d-4e86-b72d-896164651d64', rail_transfer_constitutes_digitality).
narrative_ontology:cs_drift_state('409fd4a4-472d-4e86-b72d-896164651d64', post_stablecoin_cbdc_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('409fd4a4-472d-4e86-b72d-896164651d64', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, central_banks).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_rail_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, retail_commercial_banks).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, retail_depositors).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, non_bank_digital_issuers).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, operationalist_definition_of_money).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue the currency, operate wholesale settlement systems (Fedwire-style RTGS, ACH oversight), compile the monetary aggregates, and publish the official histories that date electronic money to their own infrastructure builds. Enforce the bank-centered taxonomy through supervisory perimeters and legal definitions of deposit money. Leaving the frame would mean surrendering the definitional authority that constitutes the institution's core function, so departure is unavailable regardless of preference.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, central_banks, beneficiary).

% Run the shared messaging and clearing utilities — SWIFT-style cooperatives, ACH operators, card and ATM networks — collecting membership, message, and interchange fees on traffic that official statistics treat as digital money movement. Set message standards, host the anniversary commemorations, and hold permanent seats in the forums where payment taxonomy is reviewed. Their installed assets appreciate under the canonical dating and would need repositioning under any rival dating.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_rail_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Hold the deposit liabilities that the canonical definition counts as money itself. Electronic payroll credit, ATM access, and card settlement deepen their deposit franchise, and rival instruments being classed outside 'money' shields that franchise from reclassification pressure. Shrinking the deposit book would dissolve the balance sheet that defines them, so they defend the frame through association positions and compliance investment rather than exit.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, retail_commercial_banks, beneficiary,
    powerful, biographical, constrained, national).

% Offer stored-value wallets, e-money accounts, and later tokenized balances that behave like money for users but sit outside the canonical definition. To operate they must obtain purpose-built authorizations (e-money institution licences, money-transmitter permits) that bank deposits never needed, absorb the associated compliance cost, and accept a legitimacy discount under which their product is described as an adjunct to bank money. Building wholly parallel settlement rails is possible but expensive, so most work within the licensing system.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_bank_digital_issuers, payer,
    moderate, biographical, constrained, global).

% Compile the aggregates and reconcile the boundary whenever instruments blur categories; several jurisdictions discontinued their broadest aggregates after electronic balances collapsed the narrow/broad distinction the framework was built to track. Adjudicate dating disputes through methodological papers and handbook revisions. Collect no fee income from any dating and can adopt a rival criterion by revising a manual.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_statisticians, observer,
    analytical, generational, analytical, global).

% Received ATM cash access and electronic salary credit as the rails spread, gaining convenience without participating in or paying for the classification convention. Switch providers freely and bear no barrier attached to the boundary itself; which year is canonical is a matter of indifference to them.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, retail_depositors, beneficiary,
    powerless, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, banking_rail_operators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives statisticians, supervisors, and historians a single operational date-line and criterion — digitally transferable by the banking system — so instruments classify consistently across jurisdictions and decades; anchors the treatment of electronic bank deposits as money proper and the progressive blurring of narrow and broad aggregates.
% TRANSFER_FUNCTION: Moves first-mover legitimacy and heritage authority to banking infrastructure providers; moves regulatory-novelty and legitimacy costs onto non-bank digital issuers, who must enter money-adjacent markets through bespoke authorization categories instead of inheriting money status; moves citation priority in the historical record.
% ABSENT_VOICES: Cryptographic-cash researchers (the Chaum lineage), stored-value and e-purse entrepreneurs, mobile-money builders, and heterodox monetary historians had no seat when statistical manuals and official anniversaries canonized the rail dating; they register objections through journal literature, standards-body comments, and trade associations rather than inside the compiling committees.
% DISAPPEARANCE_RATIONALE: Monetary statistics manuals, supervisory perimeter documents, payment-industry heritage claims, and the aggregate-narrative architecture would all need immediate re-dating; the three sibling readings would compete to fill the vacated origin slot; rail operators would lose the founding claim that anchors their institutional prestige.
% FOUNDING_PROBLEM: As banks computerized transfers in the late 1960s, statisticians and supervisors needed a defensible operational line separating monetary liabilities (electronically transferable) from non-monetary claims; the rail-enabled transfer criterion supplied it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the academic literature documenting broad-aggregate instability that led several central banks to discontinue M3/M4/M5 series (economists with no rail-operator stake), by BIS/CPSS retrospective reports commissioned across jurisdictions, and by fintech-sector submissions arguing the canonical dating misclasses non-bank instruments; the classification problem's recurrence with stablecoins and CBDC is independently attested in all three sources.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).
:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction ends the interval at 0.32: the boundary's costs are definitional and regulatory rather than fiscal — non-bank issuers pay compliance and legitimacy costs through bespoke authorization categories, and the historical record's citation priority flows to rail owners — substantial enough to bar a rope reading, far short of wealth-transfer arrangements. Suppression is 0.28 and almost entirely structural: statute, supervisory perimeter, and handbook authority carry the frame; nobody coerces scholars who prefer rival datings, so the internalized component is negligible. Theater_ratio 0.35 and rising: as instruments blurred faster than the frame adapted (aggregates discontinued, categories collapsing), a growing share of boundary activity became commemorative — jubilees, heritage campaigns, official anniversaries — performed rather than analytic. Accessibility_collapse 0.35: rival datings remain fully published and arguable; accepting the rail criterion closes no alternative. Resistance 0.60: the sibling readings constitute organized, continuous scholarly and industrial pushback. All three metric series share one seven-point grid (1967-2026) so no row substitutes an end-state scalar for an unauthored past value; 2026 points carry a projected basis, everything earlier is observed. The suppression_requirement series is authored deliberately — this story specifically traces enforcement-capacity change: hardening through statutory codification peaking around the 2000 e-money-directive wave, then a genuine thaw under open banking and stablecoin legislation. The dynamic is a hump, not an oscillation: accumulation, peak, partial liberalization. Scalars were read at interval end (2026, post-thaw).
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. Central banks occupy the agenda-setter seat with identity_locked exit: the institution's self-concept has fused with being money's definer and its rails' operator — exit is unavailable not because barriers are insurmountable but because leaving the frame would dissolve the institutional identity itself; if that frame broke, central banks would drop to one competing proposer among many and their experienced burden would rise sharply. Rail operators sit near the beneficiary pole: the dating appreciates assets they already own. Commercial banks benefit through franchise protection. Non-bank issuers occupy the target seat: the same definition that grandfathers deposits gates their products behind bespoke licences and a legitimacy discount. Statisticians, holding the analytical seat, experience the boundary as a methodological nuisance to be reconciled, neither a benefit nor a burden. One domain, four different lived arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: central_banks, banking_rail_operators, retail_commercial_banks, retail_depositors — each derives low directionality (subsidized or near-symmetric), strongest for powerless depositors with mobile exit and mildest for identity_locked central banks whose benefit is legitimacy rather than fees. Victim declared: non_bank_digital_issuers — high directionality, amplified by constrained exit: licences are obtainable, but the money category itself stays closed to them, so the derivation should place them near the full-target end. The derivation chain from these declarations plus the power and exit atoms suffices; no directionality_overrides are authored because no seat's structural relationship diverges from what beneficiary/victim data plus exit options imply.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what keeps both mislabelings off the table. Reading the arrangement as a rope would erase the asymmetric allocation: rail owners collect heritage authority and regulatory shelter through the same structure that gates challengers, so pure-coordination praise would launder the privilege. Reading it as a snare would erase the genuine function: statistics and supervision demonstrably need an operational boundary, and challengers retain real routes — bespoke licences, parallel rails, rival datings in print — so a coercion-only framing overstates closure. Mandate status: the founding problem, a defensible operational line for money-like instruments, is live and regenerates with every new instrument class, so mandatrophy_resolved stays undeclared. Watch-item for drift: with broad aggregates discontinued in major jurisdictions, the boundary's statistical consumer base has shrunk; if supervision migrates to instrument-level rules, remaining maintenance turns commemorative and the arrangement slides toward inertial persistence with a rising theater_ratio — the series here already shows that gradient beginning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This file instantiates the infrastructure_reading of kernel digital_money_emergence_boundary; which constitutive criterion — electronic rail transferability (this reading), theoretical formalizability (conceptualization_reading), or direct consumer holdings (consumer_holdings_reading) — should fix digital money''s origin?',
    'Cross-sibling corpus comparison: classify all three reading-files, compare computed types, epsilon, and beneficiary structures, and test whether any criterion commands assent from parties committed to the other two.',
    'Switching the operative criterion relocates the origin date by up to three decades, reallocates beneficiary status among rail operators, formalizers, and device makers, and changes which regulatory categories count as grandfathered money.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one of three rival readings of a single kernel; the disagreement is located in the constitutive criterion for monetary digitization.').

omega_variable(
    definitional_privilege_provenance,
    'Did the rail-dated boundary prevail because it is the operationally correct criterion, or because rail-controlling institutions captured the committees that fixed it?',
    'Archival study of statistical-handbook drafting and anniversary-commission records, plus counterfactual elicitation asking disinterested classification theorists to date digital money blind to institutional sponsorship.',
    'Capture evidence raises effective extraction on the payer seat and supports drift toward pure-extraction classification; demonstrated neutrality supports a coordination-first reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_privilege_provenance, empirical, 'Neutral operational convergence versus incumbent capture behind the canonical dating.').

omega_variable(
    m_aggregate_collapse_attribution,
    'Does the narrowing and eventual discontinuation of broad monetary aggregates genuinely begin at the rail-transfer threshold, as this reading''s structural delta asserts, or is it an artifact of later financial innovation unrelated to the 1967-1977 boundary?',
    'Reconstruct aggregate series with and without electronic-balance reclassification across the 1970s-1990s; test whether divergence tracks rail adoption or post-1980 securitization and money-market instruments.',
    'Confirms or breaks the reading''s claimed middle-boundary mechanism; if the collapse is an artifact, the reading loses its distinctive explanatory payload relative to the siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m_aggregate_collapse_attribution, empirical, 'Whether the M4/M5 collapse mechanism belongs to this boundary placement.').

omega_variable(
    enforcement_easing_durability,
    'Is the post-2010 easing of the taxonomy''s coercive machinery (open-banking mandates, admitted issuer licences, stablecoin statutes) a durable pluralization or a lull before CBDC legal frameworks re-harden bank-centered definitions?',
    'Track supervisory-perimeter drafts and CBDC legislation for whether non-bank digital balances are admitted into money definitions or ring-fenced outside them.',
    'Determines whether the suppression_requirement series keeps falling (rope-ward drift) or reverses upward (re-consolidation), dating any future type transition correctly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_easing_durability, empirical, 'Durability of the enforcement thaw after 2010.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmeb_infrastructure_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.06).
narrative_ontology:measurement_basis(dmeb_infrastructure_tr_t1967, observed).
narrative_ontology:measurement(dmeb_infrastructure_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.09).
narrative_ontology:measurement_basis(dmeb_infrastructure_tr_t1977, observed).
narrative_ontology:measurement(dmeb_infrastructure_tr_t1990, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement_basis(dmeb_infrastructure_tr_t1990, observed).
narrative_ontology:measurement(dmeb_infrastructure_tr_t2000, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement_basis(dmeb_infrastructure_tr_t2000, observed).
narrative_ontology:measurement(dmeb_infrastructure_tr_t2010, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement_basis(dmeb_infrastructure_tr_t2010, observed).
narrative_ontology:measurement(dmeb_infrastructure_tr_t2020, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2020, 0.32).
narrative_ontology:measurement_basis(dmeb_infrastructure_tr_t2020, observed).
narrative_ontology:measurement(dmeb_infrastructure_tr_t2026, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2026, 0.35).
narrative_ontology:measurement_basis(dmeb_infrastructure_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(dmeb_infrastructure_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.16).
narrative_ontology:measurement_basis(dmeb_infrastructure_be_t1967, observed).
narrative_ontology:measurement(dmeb_infrastructure_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.2).
narrative_ontology:measurement_basis(dmeb_infrastructure_be_t1977, observed).
narrative_ontology:measurement(dmeb_infrastructure_be_t1990, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1990, 0.29).
narrative_ontology:measurement_basis(dmeb_infrastructure_be_t1990, observed).
narrative_ontology:measurement(dmeb_infrastructure_be_t2000, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement_basis(dmeb_infrastructure_be_t2000, observed).
narrative_ontology:measurement(dmeb_infrastructure_be_t2010, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement_basis(dmeb_infrastructure_be_t2010, observed).
narrative_ontology:measurement(dmeb_infrastructure_be_t2020, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2020, 0.34).
narrative_ontology:measurement_basis(dmeb_infrastructure_be_t2020, observed).
narrative_ontology:measurement(dmeb_infrastructure_be_t2026, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2026, 0.32).
narrative_ontology:measurement_basis(dmeb_infrastructure_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(dmeb_infrastructure_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.14).
narrative_ontology:measurement_basis(dmeb_infrastructure_su_t1967, observed).
narrative_ontology:measurement(dmeb_infrastructure_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.21).
narrative_ontology:measurement_basis(dmeb_infrastructure_su_t1977, observed).
narrative_ontology:measurement(dmeb_infrastructure_su_t1990, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1990, 0.34).
narrative_ontology:measurement_basis(dmeb_infrastructure_su_t1990, observed).
narrative_ontology:measurement(dmeb_infrastructure_su_t2000, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2000, 0.46).
narrative_ontology:measurement_basis(dmeb_infrastructure_su_t2000, observed).
narrative_ontology:measurement(dmeb_infrastructure_su_t2010, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement_basis(dmeb_infrastructure_su_t2010, observed).
narrative_ontology:measurement(dmeb_infrastructure_su_t2020, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2020, 0.31).
narrative_ontology:measurement_basis(dmeb_infrastructure_su_t2020, observed).
narrative_ontology:measurement(dmeb_infrastructure_su_t2026, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2026, 0.28).
narrative_ontology:measurement_basis(dmeb_infrastructure_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% Constraint family: three readings of the kernel digital_money_emergence_boundary, decomposed into separate files per the epsilon-invariance principle because the constitutive criterion (rails vs theory vs consumer holdings) yields different epsilon, different beneficiary structures, and different failure modes — one file per reading, no observable-switching inside any one story. This infrastructure_reading is the evidential upstream node: installations are datable from contracts and go-live records, and its frame conditioned how later readings were received (cryptographic cash proposals arrived into a discursive world where bank rails already counted as digital money). Edges here connect to both siblings; each sibling reciprocates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
