% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Interbank Electrification Boundary for Digital Money's Emergence (Infrastructure Reading)
 *   domain: economic/technological/historiographical
 *
 * SUMMARY:
 *   Monetary economics, financial history, and payment-system governance
 *   share a periodization question: when did digital money begin? This story
 *   instantiates ONE reading of that contested kernel — the infrastructure
 *   reading, which dates emergence to the moment institutions could move
 *   money electronically (automated teller machines 1967, the automated
 *   clearinghouse 1972, the cross-border messaging cooperative 1977) and
 *   locates digital money's core in bank-movable deposits even while
 *   consumers hold nothing digital. On this reading the middle boundary
 *   holds: the M4/M5 aggregate distinction begins collapsing here as
 *   electronic bank deposits blur the line between money and its
 *   near-substitutes. Per the epsilon-invariance principle, the colloquial
 *   label 'when did digital money emerge' decomposes into three structurally
 *   distinct claims with different epsilon values and different victim sets;
 *   the sibling readings (conceptualization, consumer-holdings) are separate
 *   constraint files linked through network.affects_constraints. The
 *   claim/metric gap is deliberate: the reading CLAIMS tangled_rope structure
 *   while the authored metrics describe its actual operation — moderate
 *   framing rents accruing to rail incumbents atop a genuine
 *   statistical-coordination function — and the engine measures any
 *   divergence rather than the author reconciling it.
 *
 * KEY AGENTS:
 *   - - central_money_statistics_authorities: Agenda-setter (institutional/analytical) — compiles the aggregate definitions and manuals that fix the dating
 *   - - interbank_rail_operators: Primary beneficiary (institutional/arbitrage) — operates the rails the dating anchors to its networks
 *   - - commercial_banks: Dual-positioned beneficiary and fee-payer (powerful/identity_locked) — holds the paradigm deposit liabilities, pays rail access fees
 *   - - nonbank_payment_providers: Primary target (organized/mobile) — bears the legitimacy and supervisory-framing costs of latecomer status
 *   - - alternative_periodization_scholars: Target (moderate/constrained) — bears citation and canon costs for rival datings
 *   - - retail_epurse_pioneers: Excluded voice (powerless/trapped) — consumer-holdings claimants with no seat in the maintaining forums
 *   - - independent_monetary_economists: Analytical observer — sees the full structure without administering it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.46).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.38).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Interbank Electrification Boundary for Digital Money's Emergence (Infrastructure Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "economic/technological/historiographical").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '55835568-eef6-4d2e-8d81-f1b071f47e41').
narrative_ontology:cs_kernel_codification('55835568-eef6-4d2e-8d81-f1b071f47e41', distributed).
narrative_ontology:cs_authority_grounding('55835568-eef6-4d2e-8d81-f1b071f47e41', expertise).
narrative_ontology:cs_interpretation_layer_present('55835568-eef6-4d2e-8d81-f1b071f47e41').
narrative_ontology:cs_reading_relation('55835568-eef6-4d2e-8d81-f1b071f47e41', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('55835568-eef6-4d2e-8d81-f1b071f47e41', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('55835568-eef6-4d2e-8d81-f1b071f47e41', foundational, digital_existence_requires_institutional_movability).
narrative_ontology:cs_axiom_status(digital_existence_requires_institutional_movability, holdable).
narrative_ontology:cs_axiom_grounding('55835568-eef6-4d2e-8d81-f1b071f47e41', digital_existence_requires_institutional_movability, conventional).
narrative_ontology:cs_axiom('55835568-eef6-4d2e-8d81-f1b071f47e41', secondary, consumer_access_is_diffusion_not_emergence).
narrative_ontology:cs_axiom_status(consumer_access_is_diffusion_not_emergence, holdable).
narrative_ontology:cs_axiom_grounding('55835568-eef6-4d2e-8d81-f1b071f47e41', consumer_access_is_diffusion_not_emergence, conventional).
narrative_ontology:cs_reference_frame('55835568-eef6-4d2e-8d81-f1b071f47e41', interbank_electrification_origin).
narrative_ontology:cs_drift_state('55835568-eef6-4d2e-8d81-f1b071f47e41', contemporary_nonbank_rail_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('55835568-eef6-4d2e-8d81-f1b071f47e41', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, interbank_rail_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, nonbank_payment_providers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, alternative_periodization_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, bank_deposit_paradigm_of_digital_money).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, m4_m5_aggregate_blurring_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compile and revise the monetary aggregate definitions and statistical manuals that fix when electronic forms of money enter the official record. They convene the working groups that decide how new instruments slot into existing categories, and their publications are the reference point other institutions cite. Their stake in the dating is methodological reputation: a re-dating would force restatement of long historical series.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_money_statistics_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Operate the shared messaging and clearing networks — cross-border financial telecommunications cooperatives, automated clearinghouses, and settlement links — over which member institutions move payment instructions. The canonical dating places these networks at the origin of digital money, anchoring the operators' standing in standards bodies and casting their infrastructure as constitutive rather than incidental. They fund anniversary retrospectives and historical exhibits that reinforce the dating.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, interbank_rail_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Hold the deposit liabilities that the canonical aggregates treat as the paradigm case of digitally movable money, and pay membership and per-message fees to the rail operators for access. The dating credits banks with having been digital since the 1970s, a status that supports their positioning against non-bank entrants; at the same time they bear the compliance and modernization costs the rails impose. An institution constituted as a bank cannot practically leave the charter system its identity rests on.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, payer).

% Build wallets, e-money issuance, stablecoin systems, and merchant payment services outside the chartered banking perimeter. Under the canonical dating their products register as recent arrivals to a category banks founded, which colors supervisory posture and public narrative; industry associations lobby for category parity. They can pivot products, restructure, or relocate operations more readily than chartered institutions.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, nonbank_payment_providers, payer,
    organized, biographical, mobile, global).

% Historians and economists whose research dates digital money to theoretical formalization or to consumer-held instruments publish against the dominant timeline. Their work tends to be received as prehistory or footnote rather than as the emergence account. Careers, grant funding, and citation flows run through venues that presuppose the established dating, and switching research programs mid-career carries real cost.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, alternative_periodization_scholars, payer,
    moderate, biographical, constrained, continental).

% The companies that issued stored-value cards and early cryptographic cash to consumers in the 1990s mostly wound down after commercial failure. They hold no seat in the statistical working groups or standard-setting forums where the dating is maintained; their claim that consumer-held instruments constitute digital money survives mainly in archival and retrospective literature.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, retail_epurse_pioneers, excluded,
    powerless, biographical, trapped, global).

% Researchers outside the statistical establishment examine how aggregate definitions were chosen and what turns on the dating. They publish critiques and comparisons of the rival timelines without administering the categories.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, independent_monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, interbank_rail_operators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared temporal anchor and category boundary for monetary statistics, historical analysis, and supervisory treatment: it fixes when digitally movable money begins (interbank electrification, 1967–1977) and locates its core in bank-movable deposits, letting aggregate definitions, back-cast historical series, and regulatory categories coordinate on one timeline.
% TRANSFER_FUNCTION: Moves definitional authority and historical priority toward the banking sector: non-bank instruments are framed as later entrants into a category banks constituted, so citation standing, supervisory sympathy, and innovation-narrative credit flow toward incumbent rails and chartered banks.
% ABSENT_VOICES: Consumer-side pioneers (stored-value and cryptographic-cash issuers) and scholars dating emergence to conceptualization or consumer adoption are outside the statistical-manual conversation. Their objection — that the boundary marks where banks sit, not where money's digital form began — is voiced mainly in adjacent literature, not in the working groups that maintain the canon.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, monetary historiography, aggregate back-casting, and fintech-supervisory framing would all reorganize around one of the rival datings: curricula would re-sequence, statistical agencies would face series-restatement pressure, and non-bank providers would gain a different narrative position depending on which rival boundary won.
% FOUNDING_PROBLEM: In the 1960s and 1970s, interbank and cross-border settlement ran on paper and telex: payment instructions moved by physical courier and typed message, and transaction volumes threatened to overwhelm correspondent-banking grids. The automated teller machine (1967), the automated clearinghouse (1972), and the cross-border messaging cooperative (1977) solved a real operational problem — moving payment instructions electronically between institutions.
% FOUNDING_PROBLEM_CORROBORATION: Academic payment-system histories and surviving central-bank archives documenting pre-electronic settlement volumes corroborate, from outside the benefiting parties, that the interbank-coordination problem was real and remains live. What no one outside the banking-and-statistics community attests is the further inference that solving that problem constitutes the emergence of digital money — that step is asserted only within the community that administers the boundary, which is itself signal.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.46 at interval end) because the boundary transfers no money directly; what it moves is definitional authority, historical priority, and supervisory framing — real but diffuse goods. Suppression (0.38) is discursive rather than coercive: rival datings are not banned, they are marginalized through manual editions, textbook canons, and venue gatekeeping. Theater (0.25) reflects a growing commemorative layer — cooperative anniversaries, 'fifty years of electronic payments' retrospectives, museum exhibits — laid over a boundary that still performs genuine classificatory work daily. Accessibility collapse is low-to-moderate (0.40) precisely because the sibling readings remain live and articulable; understanding the boundary does not foreclose its alternatives. Resistance (0.35) comes from fintech coalitions and rival-periodization scholarship. The temporal series run on one shared grid (T=0..50, mapping roughly 1975–2025) with all three metrics authored at every point. The suppression_requirement trajectory is the story's enforcement dynamic: the boundary needed little defense while uncontested (T=0), then faced serial challenges — cryptographic-cash formalization in the mid-1980s, consumer e-purses in the 1990s, decentralized digital cash after 2009 — and the establishment's canonical-defense activity intensified accordingly, hence the monotonic rise to 0.38 rather than a flat line.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and rail-operator seats should compute as experiencing a descriptive bookkeeping convention: from inside the statistical establishment the dating simply records when the machinery existed. The payer seats should compute differently: from a non-bank provider's position the same boundary operates as an incumbency tax on legitimacy, and from a rival scholar's position as a citation gate. Commercial banks straddle the divide — credited with origin status while paying the rails' fees — so their computed position should sit between the poles. The engine derives these divergences from the declared roles, exits, and power atoms; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Rail operators sit nearest the beneficiary pole: the dating subsidizes their standing and they bear none of its costs. Commercial banks derive low-but-not-minimal directionality — beneficiaries of the deposit-paradigm framing yet simultaneously fee-payers into the rail system, a genuinely dual position carried by secondary_role rather than an override. Non-bank payment providers sit near the target pole with mobile exit damping effective extraction somewhat; rival scholars sit near the target pole with constrained exit amplifying it, since their career capital is sunk in venues that presuppose the canon. The excluded e-purse pioneers would be targets but hold no seat — their exclusion is recorded, not scored. Observers are directionally neutral by construction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — paper-based interbank settlement nearing grid failure — is corroborated as live from outside the beneficiary set, so no mandatrophy resolution is declared and none should be: the arrangement still solves the problem it was built for. The classification discipline cuts both ways here. Reading the boundary as pure coordination (rope) would erase the documented asymmetry between rail incumbents and non-bank challengers; reading it as pure extraction (snare) would erase the genuine statistical function that monetary aggregates cannot do without. The tangled_rope claim keeps both halves visible. The forward risk is decay rather than capture: if non-bank rails become the statistical norm and the dating survives only as commemoration, theater_ratio continues climbing past functional maintenance and the structure drifts toward inertial persistence — the rising theater series is the early indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_placement_underdetermination,
    'Which boundary placement — institutional movability, theoretical thinkability, or consumer holdings — correctly dates digital money''s emergence, and is the disagreement among the three readings empirical or definitional?',
    'Comparative downstream analysis: trace what aggregate back-casting, supervisory treatment of non-bank instruments, and historical narrative actually change under each dating. If outcomes converge, the dispute is definitional and resolvable by convention; if they diverge materially, the boundary choice allocates real resources.',
    'If definitional, this reading''s advantage over its siblings is conventional rather than evidential, weakening the incumbency-based extraction claim; if substantive, the boundary placement distributes regulatory and reputational resources and the asymmetric-extraction reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_placement_underdetermination, conceptual, 'Whether the kernel''s rival datings differ substantively or only conventionally.').

omega_variable(
    rail_operator_rent_vs_incident,
    'Do interbank rail operators collect rents from the boundary placement, or does their advantaged position merely coincide with a dating that would have been adopted for coordination reasons regardless?',
    'Counterfactual archival study of the 1970s–1980s statistical-manual deliberations: who proposed the dating, which alternatives were on the table, and whether rail-operator positions correlate with adoption decisions.',
    'Documented rent capture confirms asymmetric extraction riding a real coordination function; demonstrated coincidence supports a pure-coordination reading and lowers effective extraction for the beneficiary seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rail_operator_rent_vs_incident, empirical, 'Whether the boundary''s beneficiary structure reflects design or coincidence.').

omega_variable(
    practice_drift_reversibility,
    'Will the contemporary migration of digital-money practice onto non-bank rails (stablecoins, central-bank digital currencies, instant-payment schemes) force revision of the canonical dating, or will the boundary absorb the new instruments without rewriting origination accounts?',
    'Observe successive revisions of the international monetary-statistics manuals and flagship historical surveys for whether origination narratives are rewritten or merely extended.',
    'Absorption without revision pushes theater_ratio further upward — commemorative maintenance of a strained boundary — and signals decay toward inertial persistence; explicit revision re-dates the constraint''s referent and reshuffles the family''s upstream/downstream order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_drift_reversibility, empirical, 'Whether the bank-centric dating absorbs or yields to non-bank practice.').

omega_variable(
    aggregate_blurring_attribution,
    'Does the M4/M5 convergence attributed to this boundary reflect the boundary''s causal role in category construction, or independent technological substitution between deposits and near-monies?',
    'Econometric decomposition of aggregate convergence against instrument-level substitution data spanning the period before and after the boundary''s codification in official statistics.',
    'If attribution fails, the reading''s distinctive structural delta weakens and its classification converges toward a generic information standard with minimal extraction; if it holds, the boundary actively constructs the phenomenon it describes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregate_blurring_attribution, empirical, 'Causal credit for the aggregate-blurring phenomenon this reading claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dme_infra_read_tr_t0, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dme_infra_read_tr_t10, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(dme_infra_read_tr_t20, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(dme_infra_read_tr_t30, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(dme_infra_read_tr_t40, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(dme_infra_read_tr_t50, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(dme_infra_read_be_t0, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(dme_infra_read_be_t10, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(dme_infra_read_be_t20, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(dme_infra_read_be_t30, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(dme_infra_read_be_t40, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(dme_infra_read_be_t50, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 50, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(dme_infra_read_su_t0, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(dme_infra_read_su_t10, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(dme_infra_read_su_t20, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(dme_infra_read_su_t30, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(dme_infra_read_su_t40, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 40, 0.37).
narrative_ontology:measurement(dme_infra_read_su_t50, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when did digital money emerge' fails the epsilon-invariance test as a single constraint: dating by institutional movability, by theoretical formalization, and by consumer-held instruments yields materially different epsilon values, beneficiary sets, and failure modes. It therefore decomposes into a three-story constraint family. Citation practice orders the family upstream-to-downstream: the canonical infrastructure dating frames the conceptualization account as prehistory and the consumer-holdings account as mere diffusion, so this reading exerts structural pressure on both siblings' operating environment without logically eliminating either — hence coexists_with toward the conceptualization reading and influences toward the consumer-holdings reading. Each file documents the decomposition; forcing one story to span all three datings would make epsilon observer-dependent, which the chi formula forbids.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
