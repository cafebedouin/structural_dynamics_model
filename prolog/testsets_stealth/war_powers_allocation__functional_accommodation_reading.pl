% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: Contextual War Powers Allocation (Functional Accommodation Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the war_powers_allocation kernel:
 *   the functional accommodation reading, under which the proper allocation
 *   of war-initiation authority varies with operational context — imminent
 *   threats permit unilateral presidential action, prolonged campaigns
 *   require congressional authorization. The epsilon referent is the standing
 *   arrangement under contest: actual post-1973 American allocation practice
 *   (statutory notification regimes, episodic AUMFs, unilateral short
 *   strikes, executive-defined hostilities thresholds), assessed BY THIS
 *   READING'S OWN LIGHTS. The reading endorses context-dependence as such, so
 *   it does not score the arrangement as a wholesale usurpation; what it does
 *   register is the executive's progressive exploitation of the ambiguity
 *   zone — operations of functionally prolonged character conducted under
 *   imminent-threat framings, notification rituals substituting for
 *   deliberation. The claim/metric gap is deliberate: the reading CLAIMS
 *   tangled_rope (genuine timing-coordination function plus real asymmetric
 *   extraction), and the metrics are authored independently as descriptively
 *   true of the arrangement's operation. Sibling readings
 *   (congressional_primacy_reading, inherent_executive_reading) are separate
 *   constraints in separate files; this file does not hedge epsilon across
 *   them.
 *
 * KEY AGENTS:
 *   - institutional_presidency: agenda-setter and primary beneficiary (institutional/arbitrage) — controls operational framing and collects discretion in the ambiguity zone
 *   - congress_as_institution: primary payer (institutional/constrained) — holds formal powers whose exercise the arrangement renders episodic and reactive
 *   - legislators_preferring_deniability: secondary beneficiary (organized/constrained) — collect electoral insulation from war votes
 *   - citizen_electorate: payer (powerless/trapped) — bears the costs of force initiated without deliberation
 *   - service_members: payer (powerless/trapped) — deploy under framings they cannot contest
 *   - foreign_populations_in_operational_theaters: payer and absent voice (powerless/trapped) — absorb the force itself
 *   - federal_courts: observer (institutional/analytical) — doctrinal abstention maintains the arrangement's indeterminacy
 *   - categorical_rule_advocates: excluded (moderate/constrained) — would redraw the line in either direction but hold no enforcement seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.58).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.42).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "Contextual War Powers Allocation (Functional Accommodation Reading)").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, '5bb78795-0365-4851-a74c-923dac010cc1').
narrative_ontology:cs_kernel_codification('5bb78795-0365-4851-a74c-923dac010cc1', fixed_text).
narrative_ontology:cs_authority_grounding('5bb78795-0365-4851-a74c-923dac010cc1', distributed).
narrative_ontology:cs_reading_relation('5bb78795-0365-4851-a74c-923dac010cc1', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bb78795-0365-4851-a74c-923dac010cc1', war_powers_allocation__inherent_executive_reading, influences).
narrative_ontology:cs_axiom('5bb78795-0365-4851-a74c-923dac010cc1', foundational, allocation_tracks_operational_context).
narrative_ontology:cs_axiom_status(allocation_tracks_operational_context, holdable).
narrative_ontology:cs_axiom_grounding('5bb78795-0365-4851-a74c-923dac010cc1', allocation_tracks_operational_context, instrumental).
narrative_ontology:cs_axiom('5bb78795-0365-4851-a74c-923dac010cc1', secondary, categorical_rules_degrade_force_decisions).
narrative_ontology:cs_axiom_status(categorical_rules_degrade_force_decisions, holdable).
narrative_ontology:cs_axiom_grounding('5bb78795-0365-4851-a74c-923dac010cc1', categorical_rules_degrade_force_decisions, empirically_contingent).
narrative_ontology:cs_reference_frame('5bb78795-0365-4851-a74c-923dac010cc1', contextual_operational_equilibrium).
narrative_ontology:cs_drift_state('5bb78795-0365-4851-a74c-923dac010cc1', contemporary_gray_zone_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5bb78795-0365-4851-a74c-923dac010cc1', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, institutional_presidency).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, legislators_preferring_deniability).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress_as_institution).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, citizen_electorate).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, service_members).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, foreign_populations_in_operational_theaters).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, youngstown_zone_of_twilight_framework).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, contextual_flexibility_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, political_question_abstention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates military operations and chooses the legal frame case by case: cites imminent threat or national interest for short strikes, notifies Congress after the fact, and seeks formal authorization mainly when a campaign is expected to be long or politically costly. Controls the intelligence and timing information that defines what counts as imminent. Because it shapes facts on the ground faster than rules can catch them, it can keep any given operation inside the short-war window by framing, tempo, and sequencing.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, institutional_presidency, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, institutional_presidency, beneficiary).

% Holds the purse and the formal powers to declare war and authorize force. Passes authorizations episodically, often retroactively, and periodically adopts notification or termination demands that lapse unenforced. Its enforcement tools — funding conditions, statutory deadlines, impeachment — require sustained cross-party majorities that have been difficult to assemble, particularly when members of the president's party share his operational interests.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress_as_institution, payer,
    institutional, generational, constrained, national).

% Individual members who benefit from not having to vote on most uses of force. A recorded war vote is an electoral liability whichever way it goes; executive action lets them criticize, support, or stay silent at low cost. They praise the arrangement's flexibility publicly and rarely spend political capital to formalize or restrict it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, legislators_preferring_deniability, beneficiary,
    organized, biographical, constrained, national).

% Bears the costs of armed conflict — taxes, casualties, geopolitical blowback — while responsibility for initiating it is diffused across two branches. Accountability runs through elections that rarely turn on war powers and through representatives who frequently never voted on the operations in question.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, citizen_electorate, payer,
    powerless, generational, trapped, national).

% Deploy under orders whose legal justification shifts with the administration's framing of the operation. They carry the physical risk of missions undertaken without a formal authorization debate, and professional obligations bar them from contesting the allocation question themselves.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, service_members, payer,
    powerless, immediate, trapped, global).

% Live where strikes and deployments occur. They hold no seat in the domestic process that decides whether force is used against them, and the speed that makes unilateral action attractive domestically is the same speed that leaves them outside any deliberation at all.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, foreign_populations_in_operational_theaters, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, foreign_populations_in_operational_theaters, excluded).

% Decline to decide allocation disputes, citing standing, ripeness, and political-question doctrines. Their abstention removes the one forum that could draw the imminence line definitively; individual justices signal in dicta that the question is justiciable in principle, but no case arrives with plaintiffs they can hear.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Scholars, former officials, and some legislators who argue for a bright line in one direction or the other — authorization always required beyond immediate defense, or commander-in-chief discretion sufficient alone. They publish, testify, and litigate at the margins, but the arrangement's flexibility gives sitting officeholders in both branches standing reasons to keep the line undrawn.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, categorical_rule_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, institutional_presidency).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the timing problem of democratic war-making: enables rapid response to genuinely imminent threats, where deliberative delay would be catastrophic, while routing expected-prolonged commitments through broader legitimation. Allocation tracks operational context instead of holding categorically.
% TRANSFER_FUNCTION: Moves war-initiation discretion from Congress to the president for operations of intermediate duration and scope — the decision point for anything short of a declared long war migrates to the executive. It also moves political risk: the war vote that legislators would otherwise have to cast is transferred to the president, who absorbs both the credit and the blame.
% ABSENT_VOICES: Categorical-rule advocates in both directions, the foreign populations who absorb the force, and service members would all object if seated. The gray-zone negotiation proceeds among the presidency, a Congress whose members often prefer deniability, and courts that have declined the referee role — unanimity arises partly because everyone who would demand a bright line was never given an enforcement seat.
% DISAPPEARANCE_RATIONALE: If the contextual allocation vanished overnight, one of the two categorical regimes would replace it: either every non-immediate use of force would await Congress, freezing rapid response and handing initiative to adversaries, or the president would act alone across the board, eliminating the residual deliberative check. Deployment decisions, alliance assurances, and the electoral market for war votes would all reorganize around whichever gate replaced the ambiguity zone.
% FOUNDING_PROBLEM: The constitutional text splits war powers between a Congress that declares war and a president who commands the forces, creating a timing gap: genuine emergencies demand action in hours, deliberation takes weeks. Mid-twentieth-century practice built the contextual allocation to close that gap without amendment — letting imminence license speed and expected duration trigger deliberation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Supreme Court's own framework opinion (Justice Jackson's Youngstown concurrence) treats operational context as the operative variable; national-security-law scholarship across ideological camps, military-historical literature, and testimony from former officials of both parties acknowledge the imminence-timing problem as real. Even congressional-primacy advocates concede narrow emergency windows — the live dispute is over the window's width, not its existence.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.36 to 0.58 across the interval): the reading credits the arrangement with solving a real timing problem, but the historical record shows steady executive accumulation inside the gray zone — the 60-day reporting clock ignored without consequence, the 'hostilities' definition narrowed to preserve operational freedom, multi-year strike campaigns conducted under imminent-threat framings. Authorization episodes (the large-scale ground-war AUMFs) temporarily damp measured extraction without reversing the trend. Suppression is authored as a raw structural property, unscaled by power or scope: it reflects the coercive machinery holding categorical alternatives out of operative law — judicial abstention, electoral incentives against war votes, funding-leverage costs — and its measured requirement FALLS across the interval (0.64 to 0.42) because the arrangement's enforcement infrastructure decayed: the statutory deadline lost consequences, oversight committees atrophied, and enforcement shifted from mechanism to norm. Falling suppression and rising extraction are one dynamic, not a contradiction: as the machinery that once pushed operations toward authorization lost capacity, the ambiguity zone widened toward the executive. Theater ratio rises (0.18 to 0.37) as notification letters and 'consultations' become ritualized — performed to satisfy the statute's letter while informing rather than deliberating. Accessibility_collapse is LOW (0.35): the categorical alternatives have not collapsed — they remain fully articulated, litigated, and live, which is precisely why this is a contested kernel rather than settled law. Resistance is HIGH (0.6): inter-branch friction is the arrangement's ordinary texture, not an anomaly. All three series run on one shared seven-point grid; every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat, the arrangement is warranted flexibility it operates responsibly — the presidency experiences the gray zone as the system working. From the congressional seat, the same structure is experienced as incremental dispossession: each individual episode looks defensible, the aggregate looks like a surrendered power. The deniability-beneficiary legislators experience it as electoral protection. The trapped payer seats (electorate, service members, foreign populations) experience it as unaccountable force. The court seat experiences nothing directly — its abstention is constitutive of the arrangement rather than a position within it. The engine derives these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional presidency sits nearest the beneficiary pole: it collects the discretion, controls the framing, and holds arbitrage-grade exit (it can sequence and define operations to stay inside whatever window the rules describe). Legislators preferring deniability derive low directionality despite nominal membership in the payer branch — they receive electoral subsidy from the arrangement. Congress as institution derives high directionality as victim, moderated by its institutional power and its retained (if costly) enforcement tools. Citizen electorate, service members, and foreign populations sit near the full-target end: trapped, powerless, bearing the arrangement's costs in money, risk, and force respectively. Federal courts take the analytical seat. Scope amplification applies modestly: the arrangement operates at national-to-global scope, where verification of 'true imminence' is hardest — which is exactly where the executive's framing advantage bites.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against both symmetric mislabels. Reading the arrangement as pure coordination (rope) would erase the documented asymmetric accumulation — the extraction series rises monotonically and concentrates on identifiable payer seats. Reading it as pure extraction (snare) would erase the genuine timing function that even the arrangement's critics concede: the imminent-threat window solves a real problem that categorical rules demonstrably handle worse. Tangled rope holds both truths: coordination function plus enforced asymmetry. On the genealogy interview, the founding problem (emergency timing) remains live and the disappearance verdict is world_rearranges — the mismatch consumer finds no dead-mandate-plus-capture flag; the arrangement's persistence tracks a problem that still exists, even as its distributional terms drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the war_powers_allocation kernel; how would the classification shift if instantiated under a sibling reading?',
    'Generate the sibling stories (congressional_primacy_reading, inherent_executive_reading) over the same standing arrangement and compare computed types and per-seat classifications across the family.',
    'Under the congressional_primacy reading, epsilon rises sharply — every unauthorized strike beyond immediate defense counts as violation, pushing the arrangement toward snare-flavored capture with the presidency as capturer. Under the inherent_executive reading, epsilon falls — the same operations read as lawful command, leaving mostly the coordination residue. The tangled_rope verdict is contingent on this reading''s endorsement of contextual allocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Classification contingency on kernel reading selection').

omega_variable(
    gray_zone_boundary_location,
    'Where, operationally, does imminence end and prolongation begin — and can any administrable line be drawn at all?',
    'Accumulated litigation or statutory definition attempts fixing markers (duration caps, hostilities definitions, force-size thresholds) and observing which operations fall on which side.',
    'A narrower administrable window converts more historical operations into violations, raising the executive seat''s effective extraction and pushing its computed classification toward capture; a demonstration that no administrable line exists strengthens this reading''s core axiom and stabilizes the tangled_rope verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_zone_boundary_location, empirical, 'Location and administrability of the imminence-prolongation boundary').

omega_variable(
    enforcement_decay_reversibility,
    'Is the measured decay of enforcement capacity (falling suppression_requirement) a reversible cycle or a monotonic ratchet?',
    'Comparative historical analysis of reform surges — post-Church-committee oversight, post-Iraq authorization debates, periodic war-powers reform bills — testing whether enforcement capacity recovers under scandal conditions.',
    'If cyclical, the arrangement''s classification may oscillate between rope-leaning and capture-leaning states across reform waves; if a ratchet, continued drift toward executive capture is the base case and the payer seats'' position worsens irreversibly absent structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_reversibility, empirical, 'Whether enforcement decay is cyclical or a one-way ratchet').

omega_variable(
    legislative_deniability_weight,
    'How load-bearing is the deniability preference of individual legislators in stabilizing the arrangement, relative to executive preference?',
    'Roll-call analysis comparing authorization votes cast under protected procedures versus public positions taken on identical operations, plus interview evidence on member preferences.',
    'If deniability is load-bearing, fixing the arrangement requires changing electoral incentives rather than constitutional design — confirming the prohibitive fixing-cost assessment; if marginal, statutory restoration of enforcement mechanics could suffice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_deniability_weight, empirical, 'Weight of legislator deniability preference in the arrangement''s stability').

omega_variable(
    internalized_legislative_deference,
    'Is Congress''s non-enforcement of its own war powers structural (collective-action barriers, partisan veto points) or internalized (members have come to regard deference as institutionally proper)?',
    'Post-turnover behavior test: when chamber control flips to the opposition of a strike-conducting president, do new majorities revive enforcement tools, or do they sustain the same deferential practice?',
    'If substantially internalized, the arrangement''s suppression persists even after structural barriers are removed — enforcement restoration would fail without norm-level change, and the effective suppression experienced by the congressional seat exceeds what the structural measure records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_legislative_deference, conceptual, 'Structural versus internalized mechanism of congressional non-enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 52).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wp_functional_accommodation_tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(wp_functional_accommodation_tr_t0, observed).
narrative_ontology:measurement(wp_functional_accommodation_tr_t8, war_powers_allocation__functional_accommodation_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(wp_functional_accommodation_tr_t8, observed).
narrative_ontology:measurement(wp_functional_accommodation_tr_t17, war_powers_allocation__functional_accommodation_reading, theater_ratio, 17, 0.25).
narrative_ontology:measurement_basis(wp_functional_accommodation_tr_t17, observed).
narrative_ontology:measurement(wp_functional_accommodation_tr_t26, war_powers_allocation__functional_accommodation_reading, theater_ratio, 26, 0.28).
narrative_ontology:measurement_basis(wp_functional_accommodation_tr_t26, observed).
narrative_ontology:measurement(wp_functional_accommodation_tr_t35, war_powers_allocation__functional_accommodation_reading, theater_ratio, 35, 0.31).
narrative_ontology:measurement_basis(wp_functional_accommodation_tr_t35, observed).
narrative_ontology:measurement(wp_functional_accommodation_tr_t44, war_powers_allocation__functional_accommodation_reading, theater_ratio, 44, 0.34).
narrative_ontology:measurement_basis(wp_functional_accommodation_tr_t44, observed).
narrative_ontology:measurement(wp_functional_accommodation_tr_t52, war_powers_allocation__functional_accommodation_reading, theater_ratio, 52, 0.37).
narrative_ontology:measurement_basis(wp_functional_accommodation_tr_t52, observed).

% Extraction over time
narrative_ontology:measurement(wp_functional_accommodation_be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(wp_functional_accommodation_be_t0, observed).
narrative_ontology:measurement(wp_functional_accommodation_be_t8, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement_basis(wp_functional_accommodation_be_t8, observed).
narrative_ontology:measurement(wp_functional_accommodation_be_t17, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 17, 0.42).
narrative_ontology:measurement_basis(wp_functional_accommodation_be_t17, observed).
narrative_ontology:measurement(wp_functional_accommodation_be_t26, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 26, 0.47).
narrative_ontology:measurement_basis(wp_functional_accommodation_be_t26, observed).
narrative_ontology:measurement(wp_functional_accommodation_be_t35, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 35, 0.52).
narrative_ontology:measurement_basis(wp_functional_accommodation_be_t35, observed).
narrative_ontology:measurement(wp_functional_accommodation_be_t44, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 44, 0.55).
narrative_ontology:measurement_basis(wp_functional_accommodation_be_t44, observed).
narrative_ontology:measurement(wp_functional_accommodation_be_t52, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 52, 0.58).
narrative_ontology:measurement_basis(wp_functional_accommodation_be_t52, observed).

% Suppression requirement over time
narrative_ontology:measurement(wp_functional_accommodation_su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(wp_functional_accommodation_su_t0, observed).
narrative_ontology:measurement(wp_functional_accommodation_su_t8, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(wp_functional_accommodation_su_t8, observed).
narrative_ontology:measurement(wp_functional_accommodation_su_t17, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 17, 0.56).
narrative_ontology:measurement_basis(wp_functional_accommodation_su_t17, observed).
narrative_ontology:measurement(wp_functional_accommodation_su_t26, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 26, 0.52).
narrative_ontology:measurement_basis(wp_functional_accommodation_su_t26, observed).
narrative_ontology:measurement(wp_functional_accommodation_su_t35, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 35, 0.48).
narrative_ontology:measurement_basis(wp_functional_accommodation_su_t35, observed).
narrative_ontology:measurement(wp_functional_accommodation_su_t44, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 44, 0.45).
narrative_ontology:measurement_basis(wp_functional_accommodation_su_t44, observed).
narrative_ontology:measurement(wp_functional_accommodation_su_t52, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 52, 0.42).
narrative_ontology:measurement_basis(wp_functional_accommodation_su_t52, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, inherent_executive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'who decides on war' decomposes into three structurally distinct readings of the war_powers_allocation kernel, each with its own stable epsilon over the same standing arrangement. The congressional_primacy_reading is the textual baseline and functions as upstream — its necessity claim is cited by reformers within this reading's tradition and supplies the standard against which this reading's concessions are measured. This functional_accommodation_reading exerts downstream influence on the inherent_executive_reading by legitimating its imminent-threat core while withholding endorsement of its general claim. All three files link one another via network.affects_constraints; epsilon divergence across the family is the measurement the decomposition exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
