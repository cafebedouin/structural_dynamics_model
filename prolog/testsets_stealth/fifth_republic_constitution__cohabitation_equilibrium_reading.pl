% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Dual Executive — Cohabitation Equilibrium Reading
 *   domain: constitutional/political/comparative-government
 *
 * SUMMARY:
 *   The French Fifth Republic fuses two sources of democratic legitimacy in
 *   one executive: a directly elected president and a prime minister
 *   answerable to the National Assembly. This story authors the
 *   cohabitation-equilibrium reading of that arrangement: the two poles
 *   mutually constrain each other, and the working division of labor —
 *   presidential leadership of foreign affairs and defense, governmental
 *   control of domestic policy — is a negotiated settlement that must be
 *   renewed whenever electoral outcomes place the poles in opposed camps. The
 *   reading treats the arrangement as having a real coordination function (it
 *   stabilized a regime that had cycled through governments and preserved
 *   alternation channels in both directions) while imposing real costs whose
 *   incidence rotates: whichever pole loses the negotiation forfeits its
 *   preferred domains, and the diffuse costs of split accountability and
 *   policy discontinuity land on voters and the administrative machine. Per
 *   the corpus's claim/metric independence rule, the claimed type below
 *   states the authoring seat's structural judgment while the metrics
 *   describe the arrangement's observed operation independently; the engine
 *   computes per-seat classifications from the structural data. This story is
 *   one member of a three-story family over the same constitutional text; see
 *   network.dual_formulation_note. KEY AGENTS (by structural relationship): -
 *   incumbent_president: Executive pole and recurring beneficiary
 *   (powerful/constrained) — holds appointment initiative, the dissolution
 *   threat, and the foreign-defense portfolio; episodically a target when an
 *   opposed Assembly majority closes domestic domains -
 *   prime_minister_government: Second executive pole, alternating beneficiary
 *   and bearer of presidential dominance (powerful/constrained) — controls
 *   domestic policy and answers to the Assembly -
 *   parliamentary_majority_parties: Structural beneficiary (organized/mobile)
 *   — guaranteed governing role and spoils in either configuration -
 *   french_voters: Primary payer (organized/trapped) — carry split-mandate
 *   accountability and policy-whiplash costs - ministerial_civil_service:
 *   Secondary payer (moderate/constrained) — executes potentially
 *   incompatible directives from the two poles - assembly_minority_blocs:
 *   Excluded voice (organized/constrained) — outside the bilateral allocation
 *   negotiation - constitutional_council: Arbitral observer
 *   (institutional/analytical) — hardens allocation conventions through
 *   case-by-case rulings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.58).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.6).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Dual Executive — Cohabitation Equilibrium Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional/political/comparative-government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '4374127c-edde-47af-869f-9ed284a8a598').
narrative_ontology:cs_kernel_codification('4374127c-edde-47af-869f-9ed284a8a598', fixed_text).
narrative_ontology:cs_authority_grounding('4374127c-edde-47af-869f-9ed284a8a598', practice).
narrative_ontology:cs_interpretation_layer_present('4374127c-edde-47af-869f-9ed284a8a598').
narrative_ontology:cs_reading_relation('4374127c-edde-47af-869f-9ed284a8a598', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('4374127c-edde-47af-869f-9ed284a8a598', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('4374127c-edde-47af-869f-9ed284a8a598', foundational, dual_legitimacy_requires_negotiated_allocation).
narrative_ontology:cs_axiom_status(dual_legitimacy_requires_negotiated_allocation, holdable).
narrative_ontology:cs_axiom_grounding('4374127c-edde-47af-869f-9ed284a8a598', dual_legitimacy_requires_negotiated_allocation, conventional).
narrative_ontology:cs_axiom('4374127c-edde-47af-869f-9ed284a8a598', secondary, mutual_constraint_preserves_alternation_channels).
narrative_ontology:cs_axiom_status(mutual_constraint_preserves_alternation_channels, holdable).
narrative_ontology:cs_axiom_grounding('4374127c-edde-47af-869f-9ed284a8a598', mutual_constraint_preserves_alternation_channels, instrumental).
narrative_ontology:cs_reference_frame('4374127c-edde-47af-869f-9ed284a8a598', negotiated_domain_allocation_equilibrium).
narrative_ontology:cs_drift_state('4374127c-edde-47af-869f-9ed284a8a598', post_quinquennat_fragmentation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4374127c-edde-47af-869f-9ed284a8a598', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_government).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_majority_parties).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, french_voters).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, ministerial_civil_service).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, negotiated_domain_allocation_convention).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, dual_legitimacy_balance_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, regime_stability_rationale_of_1958).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected by direct universal suffrage for a five-year term. Appoints the prime minister, may dissolve the National Assembly once per year, commands the armed forces, and claims leadership of foreign policy and defense. When the Assembly majority shares the president's camp, the president sets the overall agenda and the government executes it. When the majority opposes the president, the president retains the diplomatic and military portfolio, keeps appointment initiative, and negotiates over the remaining domains. Leaving the position mid-term is possible only by resignation, which concedes the field to the opposing camp.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, incumbent_president, agenda_setter,
    powerful, biographical, constrained, national).

% Appointed by the president but responsible to the National Assembly, which can force resignation through censure. Directs domestic policy, legislation, and the administrative apparatus. With a friendly majority, the premiership executes the presidential program and absorbs parliamentary friction on the president's behalf. Under an opposed majority, the premiership becomes the domestic-policy pole in its own right, proposing legislation and nominations the president must accept or publicly fight. Exit runs through dismissal by the president or defeat in the Assembly; either ends the officeholder's tenure immediately.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_government, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_government, beneficiary).

% Party coalitions that hold or seek the Assembly majority. The dual structure guarantees them a governing role whoever wins the presidency: a sympathetic president hands them the full program, and a hostile one makes them the counterweight that staffs the government. They collect cabinet seats, committee chairs, and local patronage in both configurations. Between elections they can reposition, rebuild coalitions, or campaign for the presidency themselves, so their commitment to any particular allocation is provisional.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_majority_parties, beneficiary,
    organized, generational, mobile, national).

% Elect the president every five years and the Assembly every five years, on calendars that overlapped from 2002 until 2022 and diverged again thereafter. A vote for a presidential candidate can be neutralized weeks or months later by a legislative outcome the same electorate produced; conversely, a legislative majority can be stalemated by a president that same electorate also chose. Accountability for policy outcomes splits across two mandates with different questions on each ballot. There is no exit from the constitutional order short of emigration; the available lever is switching votes between the two contests.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, french_voters, payer,
    organized, generational, trapped, national).

% Senior officials and ministry staff execute directives originating from the president's staff at the Elysee and from the government at Matignon. In aligned periods the lines of authority are clear and careers advance predictably. In divergent periods the same ministry can receive incompatible instructions on appointments, budgets, or international negotiating positions, and officials absorb the resulting delay and risk. Alternation between camps brings reshuffles and program reversals that land on ongoing files. Careers are bound to the administration; mobility means moving between ministries rather than out of the structure.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, ministerial_civil_service, payer,
    moderate, biographical, constrained, national).

% Parties and groups without a share in the executive negotiation. Domain allocation between the two executive poles is worked out bilaterally between the Elysee and Matignon; minority blocs learn the allocation from announcements and can respond only through floor votes, committee obstruction, or censure attempts. They would press for allocation through open parliamentary procedure where their numbers would count directly. Their practical leverage is limited to making governing expensive.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, assembly_minority_blocs, excluded,
    organized, biographical, constrained, national).

% Nine-member body that rules on the constitutionality of statutes, ordinances, and procedural moves, and arbitrates boundary disputes between the executive poles when they reach it. Its decisions have hardened several allocation conventions, including ordinance domains, referendum scope, and dissolution conditions. It takes cases as they come, publishes reasoned decisions, and holds no policy agenda of its own beyond the text it administers.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles two simultaneously valid democratic mandates — direct presidential election and Assembly-majority confidence — so that they produce one governable executive instead of a permanent legitimacy collision. It fixes a default division of labor (foreign affairs and defense versus domestic policy), provides arbitration channels for boundary disputes, and keeps a peaceful alternation path open for whichever pole loses an election.
% TRANSFER_FUNCTION: Moves policy-domain control and appointment leverage between the presidential and governmental poles according to electoral outcomes; moves cabinet posts, committee chairs, and patronage to the parliamentary majority in either configuration; and moves the costs of split accountability and program discontinuity onto voters and the ministerial administration, with negotiation concessions paid by whichever pole holds fewer domains at a given moment.
% ABSENT_VOICES: Assembly minority blocs and junior coalition partners are outside the bilateral Elysee-Matignon room where domains are allocated; they would press for allocation through open parliamentary procedure. Holders of the rival readings object from outside this reading's framework — sovereignist presidentialists deny the president should be constrained at all, and parliamentary supremacists deny the president should retain an unallocated foreign-defense floor. Foreign negotiating partners experience divided French interlocution during cohabitation without any seat in the arrangement.
% DISAPPEARANCE_RATIONALE: If the negotiated-allocation arrangement vanished overnight, the two poles would not simply continue as before: either the presidency absorbs the full executive, appointing and directing governments without Assembly-mediated allocation, or the Assembly subordinates the presidency to its authorization. Cabinet formation, alternation mechanics, voter expectations about who answers for foreign policy, and the Council's arbitral docket would all reorganize within one electoral cycle.
% FOUNDING_PROBLEM: The preceding regime cycled through governments under assembly supremacy while the state faced the Algerian emergency. The 1958 settlement was built to give the executive a stable head independent of daily parliamentary arithmetic while preserving governmental responsibility to the Assembly — and, on this reading, to force the two executive legitimacy sources into negotiated coexistence rather than winner-take-all.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the Constitutional Council's own decisions repeatedly ground allocation boundaries in regime-stability reasoning; officially commissioned reform bodies (the Vedel committee of 1992, the Balladur committee of 2007) and the comparative-politics literature on semi-presidentialism attest both the original instability problem and the dispute over whether it remains live. No attestation comes solely from the presidency, the government, or the parliamentary majority.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.58 at interval end) and historically unstable: it peaked during the three cohabitations (1986-88, 1993-95, 1997-2002), when mutual blocking was intense and each pole extracted where it retained leverage, and fell during the engineered-alignment era that followed the 2000 five-year term, when the divergent-mandate condition largely disappeared and the negotiated-allocation function went dormant. Suppression (0.60) tracks enforcement effort rather than street-level coercion: the machinery is constitutional arbitration, majority discipline, dissolution threats, and — since 2022 — routinized recourse to Article 49.3 to hold a fragmented Assembly at bay. Theater is low-to-moderate (0.26): most activity is functional negotiation, with a symbolic layer (solemn presidential addresses, anniversary invocations of the spirit of the institutions) that thickens when practice and rhetoric diverge. All three series run on one shared ten-point grid spanning 1958-2024. The dominant dynamic is cyclical rather than monotonic: extraction and enforcement rise when the presidential and legislative calendars deliver opposed mandates and fall when they align. The cycle is partly an extraction mechanism in itself — each activation window lets the temporarily advantaged pole harvest its domains while the diffuse costs accumulate continuously across windows — and partly a safety valve that has absorbed repeated electoral shocks without regime rupture. Base_properties values reflect the interval endpoint (T=66, 2024), a revival phase in which Assembly fragmentation has reactivated mutual constraint after roughly fifteen dormant years.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seats compute differently, and the two agenda-setting seats differ from each other. From the Elysee, the arrangement looks like a guarantor of continuity that occasionally becomes an obstacle: the president experiences constraint mainly as domestic-domain foreclosure during cohabitation while retaining an unchallenged foreign-defense floor. From Matignon, the same structure looks like its inverse: full domestic command under a friendly president, subordination to presidential appointment initiative under a hostile one. From the voter seat, the structure is experienced as a standing question about who is answerable for what — a cost that does not rotate off. The Council experiences the arrangement as a stream of boundary disputes to settle case by case. The engine computes these per-seat classifications from the declared power, exit, and beneficiary/victim data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (incumbent_president, prime_minister_government, parliamentary_majority_parties) drive low directionality for those seats: domain control, appointment leverage, cabinet posts, and committee power flow to them in every configuration. Victim declarations (french_voters, ministerial_civil_service) drive high directionality: split-mandate accountability and directive conflict are costs those seats cannot decline. The rotation matters for reading the numbers: the president's derived directionality reflects his modal position across the cycle (net beneficiary, with episodic target phases during cohabitation), and the prime minister's mirrors it inversely; the temporal series records the oscillation that a static per-seat value summarizes. Majority parties sit nearest the beneficiary pole because their gains are configuration-independent. Voters sit nearest the target pole because their cost persists in both configurations and their exit is effectively nil. Scope is national throughout, so verification of the allocation's terms is comparatively easy and no large scope amplification applies. Receipt-surface note: gains were checked seat by seat and none captures them durably — domain control and spoils rotate with the electoral cycle, which is why gain_flow is authored as diffuse rather than assigned to the presidency despite its structural advantages.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this arrangement as pure extraction would erase its coordination record: the dual structure ended the governmental churn of the preceding regime, absorbed two cohabitation transitions and a three-way fragmented Assembly without regime rupture, and kept alternation running in both directions. Classifying it as pure coordination would erase the rotating extraction: every activation window produces winners and losers by design, and the diffuse costs never rotate off the voter and administrative seats. The mandatrophy question is live rather than settled: the founding problem (executive instability under assembly rule) was substantially solved decades ago, and between 2002 and 2022 the negotiated-allocation function was largely engineered out of operation by calendar synchronization — a period in which maintaining the equilibrium's rhetoric while not practicing it pushed the arrangement toward inertial maintenance. The post-2022 fragmentation revived the function, which argues the mandate was deferred rather than dead; omega quinquennat_alignment_lock tracks exactly this question. The mandatrophy_resolved flag is therefore left unset: the evidence supports contested, not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the fifth_republic_constitution kernel (reading: cohabitation_equilibrium_reading). How would the sibling readings restructure the constraint''s beneficiary and victim surface?',
    'Compare the compiled stories for hyper_presidential_reading and parliamentary_constraint_reading: each assigns its own epsilon, beneficiaries, and victims over the same constitutional text. The disagreement localizes in whether Article 5''s arbitration clause or Article 20''s responsibility clause is the load-bearing provision.',
    'Under the hyper-presidential sibling, extraction concentrates on the Assembly and voters with the president as near-pure beneficiary; under the parliamentary sibling, extraction concentrates on the presidency. This reading''s moderate, rotating profile holds only if the negotiated-allocation premise holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed identity of the constraint within its kernel.').

omega_variable(
    reserved_domain_boundary_status,
    'Is the presidential reservation of foreign affairs and defense a constitutional fact, or a convention that survives only while cohabitation camps honor their truces?',
    'Observe the next full cohabitation: if a prime-ministerial government successfully asserts control over diplomatic appointments or treaty negotiation, the reservation is convention; if Constitutional Council jurisprudence or settled practice upholds presidential exclusivity, it approaches textual fact.',
    'If convention, the allocation is fully renegotiable at each cohabitation and extractiveness variance rises; if entrenched, the president''s floor of domain control stabilizes his directionality nearer the beneficiary pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserved_domain_boundary_status, empirical, 'Textual versus conventional status of the reserved foreign-policy domain.').

omega_variable(
    quinquennat_alignment_lock,
    'Did the 2000 five-year term and the inverted electoral calendar permanently suppress the divergent-mandate condition this reading presupposes, or merely postpone it?',
    'Track post-2022 Assembly fragmentation: repeated hung assemblies, reliance on Article 49.3, and dissolution cycles indicate the divergent-mandate condition has returned despite calendar synchronization.',
    'If permanently suppressed, this reading decays toward inertial maintenance — the equilibrium kept rhetorically, practiced never — and theater_ratio should trend upward; if episodic, the combined coordination-and-extraction profile persists with cyclical activation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quinquennat_alignment_lock, empirical, 'Whether calendar engineering killed or deferred the cohabitation condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fift_tr_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(fift_tr_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(fift_tr_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(fift_tr_t28, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 28, 0.3).
narrative_ontology:measurement(fift_tr_t35, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(fift_tr_t42, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 42, 0.23).
narrative_ontology:measurement(fift_tr_t50, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(fift_tr_t58, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 58, 0.22).
narrative_ontology:measurement(fift_tr_t66, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 66, 0.26).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fift_be_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(fift_be_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(fift_be_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(fift_be_t28, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 28, 0.68).
narrative_ontology:measurement(fift_be_t35, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 35, 0.66).
narrative_ontology:measurement(fift_be_t42, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 42, 0.5).
narrative_ontology:measurement(fift_be_t50, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 50, 0.44).
narrative_ontology:measurement(fift_be_t58, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 58, 0.46).
narrative_ontology:measurement(fift_be_t66, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 66, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fift_su_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(fift_su_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(fift_su_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(fift_su_t28, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 28, 0.63).
narrative_ontology:measurement(fift_su_t35, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 35, 0.65).
narrative_ontology:measurement(fift_su_t42, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 42, 0.52).
narrative_ontology:measurement(fift_su_t50, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(fift_su_t58, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 58, 0.45).
narrative_ontology:measurement(fift_su_t66, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 66, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, resource_allocation).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% Constraint family over the fifth_republic_constitution kernel. The colloquial label for the Fifth Republic's dual executive covers three structurally distinct claims with different beneficiary/victim surfaces and different epsilon referents; they are authored as separate stories and linked here for contamination propagation. This member (cohabitation_equilibrium_reading) authors the negotiated-allocation claim; hyper_presidential_reading and parliamentary_constraint_reading author the rival allocations. Edges run from this reading to both siblings because shifts in the negotiated equilibrium's credibility raise or lower the plausibility conditions of each rival reading without resolving the dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
