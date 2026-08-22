% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: Institutional Recognition Threshold for Dematerialized Money (First-Held Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This story instantiates the first_held_reading of the
 *   electronic_money_emergence kernel: digital money came into existence at a
 *   discrete institutional event — the moment a chartered institution first
 *   held dematerialized currency in a form legally distinguishable from
 *   physical notes. The constraint modeled here is the standing arrangement
 *   that reading takes itself to describe: the recognition-and-classification
 *   regime (charters, settlement-finality rules, statistical aggregate
 *   definitions, and the commemorative founding canon) under which
 *   dematerialized balances count as money. Per the kernel-reading rule,
 *   epsilon's referent is this standing arrangement assessed by the reading's
 *   own lights — the reading endorses the regime as tracking a real
 *   ontological transition, so it authors moderate rather than high
 *   extraction, while the metrics remain independently authored descriptions
 *   of the regime's actual operation. The regime coordinates genuinely (law,
 *   settlement, and statistics require determinate money-objects) and
 *   extracts asymmetrically (chartered institutions define existence and
 *   collect the founding narrative; non-chartered systems bear exclusion
 *   costs), which is why the claimed type is tangled_rope. Sibling readings
 *   of the same kernel are separate constraints, linked through the network
 *   block. KEY AGENTS (by structural relationship): -
 *   central_settlement_institutions: agenda-setter and principal beneficiary
 *   (institutional/arbitrage) — operate the ledgers where dematerialized
 *   money exists and fix the recognition threshold -
 *   licensed_commercial_banks: beneficiary (powerful/constrained) — hold the
 *   canonical balances; earliest holders carry the founding-priority
 *   narrative - official_monetary_statisticians: beneficiary
 *   (organized/identity_locked) — maintain the aggregate series whose
 *   boundaries presuppose the discrete-event ontology -
 *   nonbank_electronic_payment_systems: primary target (powerful/constrained)
 *   — move dematerialized value at scale but pay charter/reporting costs or
 *   accept non-money classification - community_and_virtual_currencies:
 *   target (powerless/trapped) — functionally money-like systems defined out
 *   of the category - gradualist_monetary_historians: target
 *   (moderate/constrained) — continuous-evolution accounts subordinated to
 *   the founding-event canon - cryptographic_cash_designers: excluded voice
 *   (moderate/constrained) — built bearer-style digital cash outside
 *   recognition; never seated in the dating process -
 *   analytical_monetary_theorists: analytical observer — compares rival
 *   ontologies without stake in which date wins
 *
 * KEY AGENTS:
 *   - central_settlement_institutions: agenda-setter and principal beneficiary (institutional/arbitrage) — operate the ledgers where dematerialized money exists and fix the recognition threshold
 *   - licensed_commercial_banks: beneficiary (powerful/constrained) — hold the canonical balances; earliest holders carry the founding-priority narrative
 *   - official_monetary_statisticians: beneficiary (organized/identity_locked) — maintain the aggregate series whose boundaries presuppose the discrete-event ontology
 *   - nonbank_electronic_payment_systems: primary target (powerful/constrained) — move dematerialized value at scale but pay charter/reporting costs or accept non-money classification
 *   - community_and_virtual_currencies: target (powerless/trapped) — functionally money-like systems defined out of the category
 *   - gradualist_monetary_historians: target (moderate/constrained) — continuous-evolution accounts subordinated to the founding-event canon
 *   - cryptographic_cash_designers: excluded voice (moderate/constrained) — built bearer-style digital cash outside recognition; never seated in the dating process
 *   - analytical_monetary_theorists: analytical observer — compares rival ontologies without stake in which date wins
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.4).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.55).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Institutional Recognition Threshold for Dematerialized Money (First-Held Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, '979418e9-c431-47c4-9e81-b0168b00e9ea').
narrative_ontology:cs_kernel_codification('979418e9-c431-47c4-9e81-b0168b00e9ea', distributed).
narrative_ontology:cs_authority_grounding('979418e9-c431-47c4-9e81-b0168b00e9ea', distributed).
narrative_ontology:cs_reading_relation('979418e9-c431-47c4-9e81-b0168b00e9ea', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('979418e9-c431-47c4-9e81-b0168b00e9ea', electronic_money_emergence__m4_m5_collapse_reading, forecloses).
narrative_ontology:cs_axiom('979418e9-c431-47c4-9e81-b0168b00e9ea', foundational, monetary_existence_requires_institutional_holder).
narrative_ontology:cs_axiom_status(monetary_existence_requires_institutional_holder, holdable).
narrative_ontology:cs_axiom_grounding('979418e9-c431-47c4-9e81-b0168b00e9ea', monetary_existence_requires_institutional_holder, conventional).
narrative_ontology:cs_axiom('979418e9-c431-47c4-9e81-b0168b00e9ea', secondary, onset_datable_from_legal_recognition_records).
narrative_ontology:cs_axiom_status(onset_datable_from_legal_recognition_records, holdable).
narrative_ontology:cs_axiom_grounding('979418e9-c431-47c4-9e81-b0168b00e9ea', onset_datable_from_legal_recognition_records, empirically_contingent).
narrative_ontology:cs_reference_frame('979418e9-c431-47c4-9e81-b0168b00e9ea', discrete_institutional_onset).
narrative_ontology:cs_drift_state('979418e9-c431-47c4-9e81-b0168b00e9ea', contemporary_stablecoin_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('979418e9-c431-47c4-9e81-b0168b00e9ea', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_settlement_institutions).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, licensed_commercial_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, official_monetary_statisticians).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, nonbank_electronic_payment_systems).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, community_and_virtual_currencies).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, gradualist_monetary_historians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the ledgers and settlement systems in which dematerialized balances exist; charter and supervise the institutions permitted to hold them; publish the statistical definitions that decide what counts as money; convene the standard-setting bodies where the category boundary is maintained. Collect settlement authority and the founding narrative. Exit is meaningless from this seat: they define the arena any exit would lead into.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_settlement_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, central_settlement_institutions, beneficiary).

% Hold customer balances as ledger entries whose character as money rests on charter and supervision; the earliest holders of fully dematerialized wholesale balances carry the founding-priority narrative in their corporate histories. Leaving the chartered perimeter means forfeiting the deposit franchise, so they defend the boundary that protects them.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, licensed_commercial_banks, beneficiary,
    powerful, biographical, constrained, national).

% Maintain the monetary aggregate series whose category boundaries presuppose the discrete-event ontology; manuals, revision cycles, and career ladders are built from the framework. Switching ontologies would mean discarding the professional apparatus itself, so the framework and the profession have fused.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, official_monetary_statisticians, beneficiary,
    organized, biographical, identity_locked, national).

% Move dematerialized value at scale — card networks, automated clearing operators, later wallet providers — without originating the canonical ledger entries. To be counted as money-holders they must accept charter, capital, and reporting burdens; otherwise their instruments are classified as claims or services rather than money. Their business is the perimeter, so exit from the perimeter is exit from the business.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, nonbank_electronic_payment_systems, payer,
    powerful, biographical, constrained, global).

% Local electronic exchange schemes, in-game currencies, and mutual-credit ledgers function as money for their users but are defined out of the category: taxed as goods or barred outright, with no seat in the recognition conversation. Exit would mean abandoning the exchange itself, so they bear the classification from inside it.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, community_and_virtual_currencies, payer,
    powerless, immediate, trapped, regional).

% Trace continuous evolution from telegraphic transfer through card systems to ledger money; their periodizations compete with the founding-event canon in textbooks, curricula, and commemoration funding. Dissent is publishable but career-costly, so the constraint operates on them as a professional incentive gradient.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, gradualist_monetary_historians, payer,
    moderate, generational, constrained, continental).

% Built bearer-style dematerialized cash before and outside institutional recognition; their systems were dismissed as non-money precisely for lacking an institutional bearer. They would testify that the founding-date canon erases the technical lineage, but they were never seated in the statistical and legal processes that fixed the date.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, cryptographic_cash_designers, excluded,
    moderate, generational, constrained, global).

% Compare the rival ontologies of money emergence across readings; hold no stake in which date wins; see the full structure, including the dependence of official monetary history on the institutions it dates.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, analytical_monetary_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, central_settlement_institutions).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legally operative answer to what counts as money: settlement finality, cross-institution double-entry accounting, monetary statistics, and judicial treatment of dematerialized balances all require a determinate category boundary, and the recognition threshold supplies one.
% TRANSFER_FUNCTION: Moves definitional authority and historical priority toward chartered institutions; moves legal certainty and monetary legitimacy away from non-chartered value systems; moves the cost of category maintenance (compliance, reporting, classification risk) onto those outside the chartered perimeter.
% ABSENT_VOICES: Designers and operators of pre-institutional electronic value systems, users of informal digital exchange, and gradualist historians of payment technology are outside the recognition conversation; the founding-date canon was fixed by the institutions that later claimed it, and the voices with the strongest grounds to contest the dating were never seated in the process.
% DISAPPEARANCE_RATIONALE: If the recognition regime vanished overnight, monetary statistics would lose their category boundaries, settlement law would lose its answer to what the transferred object is, insolvency and collateral law would lose their grip on ledger balances, and the founding canon anchoring a century of official monetary history would dissolve — the legal-statistical architecture built on the discrete-event ontology would have to be re-founded from scratch.
% FOUNDING_PROBLEM: As payments dematerialized over the twentieth century, monetary authorities needed to know whether book-entry and wire-transferred balances remained money for reserve requirements, statistical aggregates, and legal-tender purposes; the discrete-event recognition threshold answered when the new kind began so that law and statistics could track it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set: legislative preparatory works for funds-transfer statutes and e-money directives, court rulings allocating liability for electronic transfers, successive revisions of international monetary-statistics manuals, and payment-law scholarship all attest that classification of dematerialized balances was and remains a live legal-statistical problem. None of these sources collects the regime's rents, though legislatures and courts share an interest in the category's continued administrability.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).
:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.40 at interval end) because the regime's definitional authority converts into real rents — franchise protection for chartered holders, founding-priority narratives, perimeter control over who may issue money-like claims — while the underlying classification work is genuine and load-bearing. Suppression (0.55) is structural rather than violent: the boundary is maintained by legal definitions, statistical manuals, and supervisory practice, and it actively forecloses rival periodizations from official channels without banning them. Theater (0.28) reflects a mostly functional classification apparatus with a growing commemorative layer (centenary narratives, museum exhibits, anniversary publications) as the founding event recedes into usable myth. Accessibility collapse is moderate-low (0.40): gradualist and artifact ontologies remain fully publishable alternatives; only official channels privilege the discrete-event frame. Resistance (0.50) is real and ongoing — the sibling readings themselves are its most articulate forms. The temporal series run on one shared seven-point grid with all three metrics authored at every point. Suppression_requirement is tracked deliberately: this story's enforcement picture is dynamic — recognition machinery was built up over the century (funds-transfer statutes, e-money directives, proportionate licensing regimes), so enforcement-capacity change is part of the narrative, not a static backdrop. Base extractiveness accumulates through the postwar consolidation of definitional rents, peaks around the e-money directive era, and eases slightly afterward as licensing pathways admitted nonbank issuers, softening exclusion-by-denial while commemorative theater kept rising.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the agenda-setter seat (central settlement institutions), the regime is its own ontology: classification work it performs on objects its ledgers constitute, with extraction near nil by its own accounting. From the beneficiary seats, the regime is mild subsidy — banks collect the deposit franchise and statisticians collect professional authority. From the payer seats, the same structure operates as exclusion: nonbank systems pay to enter or are erased, community currencies are taxed out of monetary existence, and historians watch the founding-event canon subordinate their periodizations. The statistician seat adds an identity-lock wrinkle: its exit is not merely costly but conceptually unavailable, since the professional apparatus (manuals, revision cycles, career ladders) is built from the category system itself — institutional identity fusion, where the organization has become its function. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Central settlement institutions sit nearest the beneficiary pole (d near 0.0): they define the category, administer the threshold, and collect the largest share of definitional authority — no override is needed because the beneficiary declaration plus arbitrage-grade exit already places them there. Licensed commercial banks derive low d as declared beneficiaries with constrained exit. Official monetary statisticians derive low d as beneficiaries, with identity_locked exit amplifying their attachment to the arrangement. Among targets, community and virtual currencies sit nearest the full-target pole (trapped, powerless, defined out entirely); nonbank electronic payment systems derive somewhat lower effective targeting because chartering offers a partial buy-in path — constrained rather than trapped exit; gradualist monetary historians bear career-level rather than existential costs, placing them at moderate-high d. The beneficiary/victim declarations map cleanly onto these structural relationships, so no directionality overrides are authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, not dead: every novel dematerialized instrument (e-money, stablecoins, prospective CBDCs) re-triggers the same classification question the regime was built to answer, so the mandate has not outlived its function and mandatrophy_resolved is false. The classification prevents mislabeling in both directions. It blocks the snare reading: the coordination function is genuine and load-bearing — settlement finality, insolvency law, and monetary statistics all fail without a determinate answer to what counts as money — so the regime is not cover for pure extraction. It equally blocks the piton reading: the function is not atrophied, the administrators bear real maintenance costs they cannot cheaply shed, and the payer seats are hurt specifically enough to keep contesting the boundary. Tangled_rope preserves both facts simultaneously: coordinated participants, asymmetric payers, active enforcement holding the seam. The R5 mismatch check confirms: founding_problem_status is live and disappearance_verdict is world_rearranges — no zombie flag, no mandate decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the first_held_reading of the electronic_money_emergence kernel; would adopting a sibling reading change the constraint''s beneficiary/victim structure and epsilon?',
    'Generate the sibling stories (became_thinkable_reading, m4_m5_collapse_reading) and compare computed classifications across the family; divergence in beneficiary sets and epsilon locates the disagreement.',
    'Under the artifact reading the primary beneficiaries shift from chartered institutions to statistical bureaucracies and epsilon rises sharply; under the thinkable reading the victim set gains pre-institutional innovators credited with the real transition and this reading''s regime appears as a late-arriving claim-jumper on their achievement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate structurally different constraints over the same referent.').

omega_variable(
    first_event_identifiability,
    'Which historical event, if any, satisfies ''first institutional bearer holding dematerialized currency distinguishable from physical notes'' — 1918 Fedwire telegraphic transfers, 1960s book-entry government securities, 1970s computerized clearing, or another candidate?',
    'Archival reconstruction fixing the distinguishability criteria in advance, then testing each candidate event against them; the earliest qualifying legally recognized dematerialized balance dates the onset.',
    'If no single event satisfies the criteria, the reading''s discrete-onset premise fails on its own terms and the constraint collapses toward the gradualist account; if several candidates tie, the dating becomes conventional rather than ontological and the regime''s commemorative layer loses its anchor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_event_identifiability, empirical, 'Whether the reading''s threshold event is uniquely identifiable in the historical record.').

omega_variable(
    recognition_or_constitution,
    'Does legal and regulatory recognition record a monetary transition that already occurred, or constitute the category it dates?',
    'Counterfactual analysis of unrecognized dematerialized systems: if they performed every monetary function (unit of account, means of payment, store of value) before recognition, recognition records; if enforceability, finality, and insolvency treatment changed categorically at recognition, recognition constitutes.',
    'Recording supports this reading''s moderate epsilon; constitution supports the m4_m5_collapse sibling and raises measured extraction, since the regime would be manufacturing the very object it claims to discover and date.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_or_constitution, conceptual, 'The exact structural location of the disagreement between this reading and the measurement-artifact sibling.').

omega_variable(
    exclusion_cost_allocation,
    'Are the costs borne by non-chartered value systems (legal uncertainty, taxation as goods rather than money, canonical erasure) extraction imposed by the regime, or the legitimate price of a regulatory perimeter?',
    'Compare jurisdictions that opened licensing pathways admitting nonbank issuers against those maintaining closed perimeters; measure innovation, exclusion severity, and consumer-protection outcomes across the two.',
    'If open-perimeter jurisdictions retain stability while lowering exclusion costs, part of the measured extraction is policy choice rather than coordination necessity, pushing the classification toward snare-flavored asymmetry; if closed perimeters are stability-necessary, the costs are the floor of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_cost_allocation, preference, 'Whether outsider costs are definitional rent or the unavoidable price of a determinate money category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 1918, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1918, electronic_money_emergence__first_held_reading, theater_ratio, 1918, 0.05).
narrative_ontology:measurement(elec_tr_t1940, electronic_money_emergence__first_held_reading, theater_ratio, 1940, 0.08).
narrative_ontology:measurement(elec_tr_t1958, electronic_money_emergence__first_held_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__first_held_reading, theater_ratio, 1970, 0.14).
narrative_ontology:measurement(elec_tr_t1985, electronic_money_emergence__first_held_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__first_held_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(elec_tr_t2024, electronic_money_emergence__first_held_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(elec_be_t1918, electronic_money_emergence__first_held_reading, base_extractiveness, 1918, 0.18).
narrative_ontology:measurement(elec_be_t1940, electronic_money_emergence__first_held_reading, base_extractiveness, 1940, 0.22).
narrative_ontology:measurement(elec_be_t1958, electronic_money_emergence__first_held_reading, base_extractiveness, 1958, 0.28).
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__first_held_reading, base_extractiveness, 1970, 0.34).
narrative_ontology:measurement(elec_be_t1985, electronic_money_emergence__first_held_reading, base_extractiveness, 1985, 0.36).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__first_held_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(elec_be_t2024, electronic_money_emergence__first_held_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1918, electronic_money_emergence__first_held_reading, suppression_requirement, 1918, 0.15).
narrative_ontology:measurement(elec_su_t1940, electronic_money_emergence__first_held_reading, suppression_requirement, 1940, 0.2).
narrative_ontology:measurement(elec_su_t1958, electronic_money_emergence__first_held_reading, suppression_requirement, 1958, 0.28).
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__first_held_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement(elec_su_t1985, electronic_money_emergence__first_held_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__first_held_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(elec_su_t2024, electronic_money_emergence__first_held_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, information_standard).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'when did digital money emerge' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that cannot share one story. Became_thinkable_reading dates emergence to conceptual-technical possibility (upstream: possibility precedes instantiation, and its account is cited as background by the other two). First_held_reading (this file) dates emergence to discrete institutional instantiation with legal-regulatory recognition (midstream: the official-dating position). M4_m5_collapse_reading denies the middle term entirely, treating the category as a retroactive statistical artifact (downstream negation). Each carries its own epsilon, beneficiaries, and victims; the upstream thinkability account lends evidential texture to the institutional account, while the artifact account attacks the institutional account's ontological premise directly. Linkage runs through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
