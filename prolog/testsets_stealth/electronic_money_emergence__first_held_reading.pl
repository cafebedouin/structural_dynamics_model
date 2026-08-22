% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: First Institutional Holding Threshold for Electronic Money Emergence
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested kernel
 *   electronic_money_emergence: the claim that digital money came into
 *   existence at a discrete institutional event - the first time an
 *   institutional bearer held dematerialized currency in a form legally
 *   distinguishable from physical notes - with the observable threshold tied
 *   to legal and regulatory recognition. The standing arrangement under
 *   contest is the practice of dating electronic money's emergence this way
 *   in official statistics, legal definitions, and monetary history. That
 *   arrangement solves a genuine classification problem while concentrating
 *   definitional authority in the institutions that administer the threshold,
 *   and it writes rival datings and pre-recognition innovation out of the
 *   official record. Sibling readings (emergence-at-thinkability,
 *   emergence-as-measurement-artifact) are separate constraints in separate
 *   files; they are neither described nor averaged here. KEY AGENTS (by
 *   structural relationship): - central_bank_statistical_agencies: Agenda
 *   setter (institutional/constrained) - administers the official threshold
 *   and collects definitional authority - monetary_regulators: Secondary
 *   beneficiary and co-enforcer (institutional/constrained) - gains
 *   enforceable jurisdictional boundaries from the recognition threshold -
 *   first_institutional_bearers: Beneficiary (powerful/mobile) - collects
 *   originary prestige from being the dated event -
 *   gradualist_monetary_historians: Primary target (moderate/identity_locked)
 *   - bears exclusion of continuity accounts from the official narrative -
 *   pre_recognition_payment_operators: Target (organized/trapped) - early
 *   electronic payment innovators written out of the origin -
 *   sibling_reading_proponents: Excluded voice (moderate/constrained) -
 *   holders of rival datings outside the statistical process -
 *   comparative_historiographers: Analytical observer (analytical/analytical)
 *   - sees the full kernel structure Claim/metric independence is preserved:
 *   the reading is CLAIMED as tangled_rope while the metrics independently
 *   describe moderately extractive, increasingly enforced operation; the
 *   engine computes per-seat types from the structural data.
 *
 * KEY AGENTS:
 *   - central_bank_statistical_agencies: Agenda setter (institutional/constrained) - administers the official threshold and collects definitional authority
 *   - monetary_regulators: Secondary beneficiary and co-enforcer (institutional/constrained) - gains enforceable jurisdictional boundaries
 *   - first_institutional_bearers: Beneficiary (powerful/mobile) - collects originary prestige from being the dated event
 *   - gradualist_monetary_historians: Primary target (moderate/identity_locked) - bears exclusion of continuity accounts from the official narrative
 *   - pre_recognition_payment_operators: Target (organized/trapped) - early electronic payment innovators written out of the origin
 *   - sibling_reading_proponents: Excluded voice (moderate/constrained) - holders of rival datings outside the statistical process
 *   - comparative_historiographers: Analytical observer (analytical/analytical) - sees the full kernel structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.55).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.58).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "First Institutional Holding Threshold for Electronic Money Emergence").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, '119fbee2-b0ef-4282-aace-8708ffc2397e').
narrative_ontology:cs_kernel_codification('119fbee2-b0ef-4282-aace-8708ffc2397e', distributed).
narrative_ontology:cs_authority_grounding('119fbee2-b0ef-4282-aace-8708ffc2397e', expertise).
narrative_ontology:cs_interpretation_layer_present('119fbee2-b0ef-4282-aace-8708ffc2397e').
narrative_ontology:cs_reading_relation('119fbee2-b0ef-4282-aace-8708ffc2397e', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('119fbee2-b0ef-4282-aace-8708ffc2397e', electronic_money_emergence__m4_m5_collapse_reading, forecloses).
narrative_ontology:cs_axiom('119fbee2-b0ef-4282-aace-8708ffc2397e', foundational, legal_recognition_constitutes_monetary_existence).
narrative_ontology:cs_axiom_status(legal_recognition_constitutes_monetary_existence, holdable).
narrative_ontology:cs_axiom_grounding('119fbee2-b0ef-4282-aace-8708ffc2397e', legal_recognition_constitutes_monetary_existence, conventional).
narrative_ontology:cs_axiom('119fbee2-b0ef-4282-aace-8708ffc2397e', secondary, emergence_admits_exact_temporal_location).
narrative_ontology:cs_axiom_status(emergence_admits_exact_temporal_location, holdable).
narrative_ontology:cs_axiom_grounding('119fbee2-b0ef-4282-aace-8708ffc2397e', emergence_admits_exact_temporal_location, empirically_contingent).
narrative_ontology:cs_reference_frame('119fbee2-b0ef-4282-aace-8708ffc2397e', institutional_recognition_event).
narrative_ontology:cs_drift_state('119fbee2-b0ef-4282-aace-8708ffc2397e', contemporary_decentralized_currency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('119fbee2-b0ef-4282-aace-8708ffc2397e', '2026-08-05T14:23:11Z').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_bank_statistical_agencies).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, monetary_regulators).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, first_institutional_bearers).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, gradualist_monetary_historians).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, pre_recognition_payment_operators).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, discrete_threshold_monetary_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish the monetary aggregate definitions and accompanying historical notes that fix when dematerialized balances entered the money stock; revise statistical manuals, train successor staff in the established dating, and answer legislative queries using it. Abandoning the threshold would require rewriting aggregate histories and reconciling decades of published series, so the dating is maintained even as boundary cases accumulate.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_bank_statistical_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Enforce legal definitions of deposits and electronic funds that presuppose the recognition threshold; the crisp boundary tells them which balances are reservable, insurable, and reportable. They gain enforceable jurisdiction from the threshold and in turn prosecute it through licensing and examination.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_regulators, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, monetary_regulators, agenda_setter).

% The bank or clearing institution identified as first holding dematerialized currency in legally recognizable form. Its claim to priority is cited in official histories and industry anniversaries; the prestige is already banked and survives regardless of subsequent scholarly revision, so it has little stake in defending the threshold day-to-day.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, first_institutional_bearers, beneficiary,
    powerful, biographical, mobile, national).

% Scholars whose research programs trace money's dematerialization as a continuous process spanning decades of book-entry, telegraphic, and electronic settlement. Official dating renders their accounts footnotes to a single event; their methodological commitment to continuity is fused with their professional identity, so reframing their work around a discrete threshold is experienced as abandoning the program rather than updating a parameter.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, gradualist_monetary_historians, payer,
    moderate, generational, identity_locked, global).

% Operators of early electronic funds-transfer networks and book-entry clearing systems whose infrastructure moved dematerialized value before any regulator counted it as money. Their innovations predate the recognition threshold, so the official origin story begins after their work was done; no action available to them now alters the dating, and surviving trade associations can only petition for acknowledgment.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, pre_recognition_payment_operators, payer,
    organized, biographical, trapped, regional).

% Economists and historians who hold that digital money emerged when it became technically and socially thinkable, or that the category is a retrospective artifact of monetary statistics. They publish in journals and attend conferences but sit outside the statistical and legislative processes where the official threshold is maintained; their objections register as commentary, never as revisions.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, sibling_reading_proponents, excluded,
    moderate, generational, constrained, global).

% Analysts comparing the rival datings of digital money's emergence across the full set of readings. They hold no position in the classification itself and bear none of its costs; their vantage exposes which structural claims each reading makes and what each would change if adopted.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, comparative_historiographers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, central_bank_statistical_agencies).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one legally anchored reference point for when dematerialized currency counts as money, letting reserve rules, insurance boundaries, statistical aggregates, and historical narrative proceed from a shared threshold instead of relitigating the money/non-money boundary for each balance sheet and each decade.
% TRANSFER_FUNCTION: Moves definitional authority and originary prestige from dispersed observers and early payment operators to the recognizing institutions and the first institutional bearer; correspondingly moves the burden of proof onto any account that dates the transition elsewhere.
% ABSENT_VOICES: Proponents of the became-thinkable and measurement-artifact datings, operators of pre-recognition electronic payment networks, and users of informal electronic value transfer that preceded any legal category would all object to the threshold; they stand outside the statistical manuals, legislative hearings, and central-bank histories where the dating is reproduced.
% DISAPPEARANCE_RATIONALE: Without the fixed threshold, reserve requirements and deposit insurance boundaries lose their anchoring for electronic balances, published monetary aggregate histories become unreconcilable, and the historiography of money fragments into rival timelines - regulatory treatment, statistical continuity, and origin narratives all rearrange around whichever dating each institution adopts.
% FOUNDING_PROBLEM: Mid-twentieth-century monetary authorities faced payment obligations settling as book-entry and later electronic records with no physical bearer, and needed a determinate answer to whether and when such balances constituted money for reserve requirements, deposit classification, and monetary statistics.
% FOUNDING_PROBLEM_CORROBORATION: Contemporaneous central-bank bulletins and legislative records attest the classification problem was live when the threshold was drawn, and historians of payments corroborate it from outside the benefiting parties; no source outside the statistical and regulatory establishment attests that the specific first-holding threshold remains the live solution rather than inherited orthodoxy.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55 at interval end) and rising across the interval: the threshold began as a working solution to a genuine classification problem and hardened into orthodoxy whose maintenance channels definitional authority to the agencies that own it. Suppression (0.58) reflects active enforcement - statistical manuals, legal definitions, textbook reproduction - rather than participant preference; the rival datings remain publishable but are kept out of official channels. Theater (0.28) is low-moderate: the dating does real statistical and legal work, but a growing share of activity is performative precision - anniversary commemorations and exact dates attached to what was, on the ground, a decade-spanning drift. Accessibility collapse is moderate (0.45): the sibling readings remain fully articulable, so alternatives have not collapsed. Resistance (0.50) is sustained scholarly contestation without institutional traction. Time units are years since 1960 (t=0 approximates the book-entry expansion era; t=24 the legal consolidation of electronic funds transfer; t=48 the arrival of decentralized currency). All three tracked metrics run on one shared six-point grid so no metric's series borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the threshold is simply the record: the agencies experience the convention as accurate measurement and would compute a near-coordination type from their seat. From the payer seats the same threshold operates as exclusion - continuity accounts ruled out of the official narrative, early operators written out of the origin. The engine computes this divergence from power, exit, and directional data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank statistical agencies derive d near the beneficiary end: they administer the threshold and collect its authority. Monetary regulators and first institutional bearers also sit beneficiary-side, the latter weakly so since their prestige is banked and they no longer defend the convention day-to-day. Gradualist monetary historians carry high d - they bear the extraction with identity-locked exit, placing them near the full-target end. Pre-recognition payment operators are trapped targets: their cost is already sunk and irreversible. Sibling-reading proponents are excluded rather than coordinated; their exclusion is part of what the enforcement machinery maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as rope would erase the real extraction - definitional authority concentrated in the agencies that own the measurement apparatus, plus the sidelining of rival datings. Reading it as snare would erase the genuine coordination function - a shared threshold does solve a real classification problem every monetary authority faced. The tangled-rope classification keeps both facts visible: the convention coordinates and extracts through the same structure, and its persistence depends on active maintenance of the legal-statistical boundary. The mandatrophy risk runs in a specific direction here: the founding classification problem is largely solved, and the arrangement's remaining vitality comes from defending the threshold against each new payment technology rather than from solving the original problem. The mismatch between a contested founding-problem status and a world that still rearranges around the threshold is precisely the capture signal the R5 interview exists to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_comparability,
    'Does the first-held reading measure the same underlying phenomenon as the became-thinkable and M4/M5-collapse readings, or does the shared label ''electronic money emergence'' conflate structurally distinct claims?',
    'Cross-reading comparison of each reading''s beneficiary/victim structure, epsilon value, and failure modes; convergence on one structure supports a single kernel, divergence confirms permanent decomposition.',
    'If the readings are incommensurable, the kernel label fragments into three permanent constraints and cross-reading dispute is reframed as category error rather than factual disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_comparability, conceptual, 'Whether sibling readings of the emergence kernel address one phenomenon or several.').

omega_variable(
    recognition_lag_vs_first_use,
    'Did legal and regulatory recognition of dematerialized currency coincide with its first institutional holding, or did recognition substantially lag actual use?',
    'Archival comparison of first-use records (clearing-house ledgers, interbank settlement logs) against statutory and statistical recognition dates.',
    'A substantial lag would misdate the ontological transition even on this reading''s own terms, weakening the recognition-constitutes-existence axiom and shifting the defensible threshold earlier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_lag_vs_first_use, empirical, 'Whether the legal-recognition threshold tracks or lags first actual holding.').

omega_variable(
    threshold_scope_under_decentralized_currency,
    'Can the discrete-institutional-event model survive contact with decentralized currencies that have no institutional bearer and no recognition moment?',
    'Observe how statistical agencies and regulators classify cryptocurrencies and stablecoins over the coming cycle; adoption of continuous or artifact-based treatments signals the model''s scope limit.',
    'If the model fails outside bearer-instrument eras, this reading''s scope contracts to institutionally intermediated money and the kernel contest shifts to which reading governs the unintermediated case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_scope_under_decentralized_currency, empirical, 'Whether the discrete-event threshold generalizes beyond institutionally borne money.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t0, electronic_money_emergence__first_held_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(elec_tr_t0, observed).
narrative_ontology:measurement(elec_tr_t12, electronic_money_emergence__first_held_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement_basis(elec_tr_t12, observed).
narrative_ontology:measurement(elec_tr_t24, electronic_money_emergence__first_held_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement_basis(elec_tr_t24, observed).
narrative_ontology:measurement(elec_tr_t36, electronic_money_emergence__first_held_reading, theater_ratio, 36, 0.21).
narrative_ontology:measurement_basis(elec_tr_t36, observed).
narrative_ontology:measurement(elec_tr_t48, electronic_money_emergence__first_held_reading, theater_ratio, 48, 0.25).
narrative_ontology:measurement_basis(elec_tr_t48, observed).
narrative_ontology:measurement(elec_tr_t60, electronic_money_emergence__first_held_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(elec_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(elec_be_t0, electronic_money_emergence__first_held_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(elec_be_t0, observed).
narrative_ontology:measurement(elec_be_t12, electronic_money_emergence__first_held_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement_basis(elec_be_t12, observed).
narrative_ontology:measurement(elec_be_t24, electronic_money_emergence__first_held_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement_basis(elec_be_t24, observed).
narrative_ontology:measurement(elec_be_t36, electronic_money_emergence__first_held_reading, base_extractiveness, 36, 0.47).
narrative_ontology:measurement_basis(elec_be_t36, observed).
narrative_ontology:measurement(elec_be_t48, electronic_money_emergence__first_held_reading, base_extractiveness, 48, 0.52).
narrative_ontology:measurement_basis(elec_be_t48, observed).
narrative_ontology:measurement(elec_be_t60, electronic_money_emergence__first_held_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement_basis(elec_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t0, electronic_money_emergence__first_held_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(elec_su_t0, observed).
narrative_ontology:measurement(elec_su_t12, electronic_money_emergence__first_held_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement_basis(elec_su_t12, observed).
narrative_ontology:measurement(elec_su_t24, electronic_money_emergence__first_held_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement_basis(elec_su_t24, observed).
narrative_ontology:measurement(elec_su_t36, electronic_money_emergence__first_held_reading, suppression_requirement, 36, 0.51).
narrative_ontology:measurement_basis(elec_su_t36, observed).
narrative_ontology:measurement(elec_su_t48, electronic_money_emergence__first_held_reading, suppression_requirement, 48, 0.55).
narrative_ontology:measurement_basis(elec_su_t48, observed).
narrative_ontology:measurement(elec_su_t60, electronic_money_emergence__first_held_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(elec_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, information_standard).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when did digital money emerge' decomposes into three structurally distinct claims per the epsilon-invariance principle: thinkability (became_thinkable_reading), institutional instantiation (this file), and measurement-artifact construction (m4_m5_collapse_reading). Each carries its own epsilon, beneficiaries, and victims; the readings are linked rather than merged because assigning one epsilon across observables (conceptual readiness vs. ledger fact vs. statistical category) would violate epsilon invariance. This reading sits mid-family: the artifact reading's retroactive-construction thesis cites the very institutional event this reading dates, and the thinkability reading competes laterally for the same dating slot.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
