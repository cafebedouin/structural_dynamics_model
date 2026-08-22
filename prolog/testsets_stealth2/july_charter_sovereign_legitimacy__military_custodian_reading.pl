% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: July Charter Military Custodianship Clause (Permanent Institutional Guardian)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A post-revolutionary charter, ratified at the founding moment of a
 *   fragile new state, constitutionally ratifies the armed forces as
 *   permanent institutional guardian: the military holds veto power over
 *   legislation and senior civilian appointments, controls party registration
 *   and emergency-law renewal, and operates an economic empire shielded from
 *   civilian audit. This story instantiates the military_custodian_reading of
 *   the july_charter_sovereign_legitimacy kernel: the standing arrangement
 *   under assessment is the charter's ratification of permanent military
 *   custodianship, and ε (0.58) is authored for that arrangement by this
 *   reading's own lights — a reading that treats guardianship as largely
 *   legitimate while still registering the rent accretion and bounded
 *   contestation the arrangement produces. The sibling readings
 *   (secular-democratic, guided-nationalist) are separate constraints linked
 *   via network.affects_constraints, not folded into this one; each has its
 *   own ε, victim set, and classification. The claimed type and the metrics
 *   are independent authored facts: the custodian's own framing would claim a
 *   pure coordination rope; the metrics below describe what the arrangement
 *   demonstrably does across the interval, and the engine computes per-seat
 *   classifications from the structural data without reconciling the two.
 *
 * KEY AGENTS:
 *   - military_high_command: Agenda-setter (institutional/arbitrage) — administers the custodial terms, allocates their rents, defines the security prerogative civilian courts cannot review
 *   - military_officer_corps: Primary beneficiary (organized/mobile) — collects budget share, immunity, enterprise seats, and post-retirement placements
 *   - military_affiliated_economic_holdings: Primary beneficiary (institutional/mobile) — operates under legal privileges the settlement protects from civilian audit and competition
 *   - chartered_loyalist_parties: Dual-positioned beneficiary/payer (organized/constrained) — licensed into a bounded political space, revocable at the custodian's pleasure
 *   - autonomous_political_parties: Primary target (organized/trapped) — dissolved, banned, exiled, or driven underground by the license system
 *   - student_movement: Primary target (moderate/constrained) — campus policing, conscription leverage, and expulsion against each cohort of organizers
 *   - independent_press: Secondary target (moderate/constrained) — licensing and emergency-publication rules punish coverage of the military's budget, holdings, and vetoes
 *   - constitutional_court_justices: Excluded seat (institutional/constrained) — jurisdiction over military prerogatives stripped; would claim review authority but stands outside the settlement
 *   - international_democracy_monitors: Analytical observer (moderate/analytical) — documents the bounded contestation from outside; access itself granted at the custodian's discretion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.58).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.72).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.39).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.39).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter Military Custodianship Clause (Permanent Institutional Guardian)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional/political").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, 'cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87').
narrative_ontology:cs_kernel_codification('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', fixed_text).
narrative_ontology:cs_authority_grounding('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', extraction).
narrative_ontology:cs_interpretation_layer_present('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87').
narrative_ontology:cs_reading_relation('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', foundational, permanent_guardian_necessity).
narrative_ontology:cs_axiom_status(permanent_guardian_necessity, holdable).
narrative_ontology:cs_axiom_grounding('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', permanent_guardian_necessity, instrumental).
narrative_ontology:cs_axiom('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', secondary, civilian_institutions_developmentally_unready).
narrative_ontology:cs_axiom_status(civilian_institutions_developmentally_unready, holdable).
narrative_ontology:cs_axiom_grounding('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', civilian_institutions_developmentally_unready, empirically_contingent).
narrative_ontology:cs_reference_frame('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', founding_custodial_compact).
narrative_ontology:cs_drift_state('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', contemporary_post_founding_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cffc197e-ee3b-4f3d-9ef9-2d5f8db86b87', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_affiliated_economic_holdings).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, chartered_loyalist_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, independent_press).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, chartered_loyalist_parties).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, guardianship_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, stability_precedence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and revises the charter's custodial terms: commands the armed forces, holds veto power over legislation and senior civilian appointments, controls party registration and emergency-law renewal, and appoints the boards of the military's business holdings. Allocates the budget share, licenses which parties may compete, and defines the scope of the security prerogative that civilian courts cannot review. Its position defines the arrangement; leaving it would mean dissolving the institution it commands.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command, beneficiary).

% Staffs the guardian institution: career officers hold command posts, seats in state enterprises, and immunity from civilian prosecution for service acts. The settlement guarantees their budget share, promotion ladder, and post-retirement placements. Exit is a revolving door into civilian agencies and directorships rather than a departure from privilege.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_officer_corps, beneficiary,
    organized, biographical, mobile, national).

% A network of military-owned firms, import monopolies, and land holdings operating under legal privileges the charter settlement protects from civilian audit and competition law. Their market position depends on the custodial arrangement remaining intact; they fund veteran welfare and officer amenities, tying economic interest to institutional persistence. Capital can move across sectors, but always inside the protected perimeter.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_affiliated_economic_holdings, beneficiary,
    institutional, generational, mobile, national).

% Parties licensed to operate inside the bounded political space. They gain protected access to parliament and state resources that unlicensed rivals are denied, but they hold their position at the custodian's pleasure: registration can be revoked, platforms vetted, candidates disqualified. They cannot campaign on revising the custodial terms themselves, and leaving the licensed space means political irrelevance.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, chartered_loyalist_parties, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, chartered_loyalist_parties, payer).

% Parties that refuse or lose the charter license: dissolved, banned, or driven into exile and underground organizing. Their leadership faces prosecution under emergency decrees, their assets are seized, and their supporters are barred from state employment. Exit means abandoning political activity altogether; there is no legal channel in which they can contest the custodial terms.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    organized, biographical, trapped, national).

% Campus organizing that historically drove the revolutionary mobilization. It now faces campus security forces, expulsion and conscription leverage against activists, and curriculum controls. Each cohort inherits the policing of the last; individual students can graduate out, but the movement's continuity depends on absorbing repeated crackdowns, and its historical claim as the revolution's base gives it a standing the license system exists to contain.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    moderate, biographical, constrained, national).

% Outlets operating under licensing and emergency-publication rules the custodian administers. Coverage of the military's budget, holdings, or veto decisions invites closure, prosecution, or forced sale. Some editors self-censor to survive; others publish from abroad and lose the domestic audience. Exit into uncensored publishing means leaving the country's information space.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, independent_press, payer,
    moderate, biographical, constrained, national).

% Judges of the constitutional court, whose jurisdiction the charter settlement places off-limits as to military prerogatives, appointments, and emergency decrees. Some justices dissent in opinions that go nowhere; others accommodate, and new appointments are screened by the custodian. They would claim review authority over the custodial terms but stand structurally outside the settlement that defines those terms.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, constitutional_court_justices, excluded,
    institutional, biographical, constrained, national).

% External election and constitutional monitoring missions that observe the bounded contestation, document the license system, and report to foreign ministries and treaty bodies. They hold analytical leverage but no vote in the charter's interpretation, and their access is itself granted at the custodian's discretion — reports too critical end their accreditation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_democracy_monitors, observer,
    moderate, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the founding collective-action problem of the post-revolutionary state: a fractured revolutionary coalition, threatened counterrevolution and external intervention, and no civilian institution with national reach. The armed forces provide unified territorial defense, crisis authority, and administrative continuity while civilian institutions consolidate; the charter converts that temporary necessity into a standing guardianship office with defined veto powers over the political process.
% TRANSFER_FUNCTION: Moves political authority — veto over legislation, control of party registration, senior appointments, emergency powers — and economic resources (budget share, enterprise control, land and import licenses) from civilian political actors to the military establishment, in exchange for security and continuity services delivered to the polity as a whole.
% ABSENT_VOICES: The constitutional court's justices (jurisdiction over military prerogatives stripped), the leadership of dissolved parties (exiled, imprisoned, or barred), and the secular-democratic drafters whose text lost the ratification fight. They would object that a temporary guardianship was converted into permanent privilege by the very institution that administers it — and they are absent because the custodian controlled party licensing, emergency law, and the ratification process itself.
% DISAPPEARANCE_RATIONALE: The veto rents, chartered-party licenses, the holdings' legal privileges, and the emergency-law machinery all exist only by the arrangement's operation; overnight removal would return banned parties to open competition, expose military holdings to civilian audit, and return the officer corps to civilian budget authority. The parties dispute the valence — the custodian predicts state collapse, opponents predict institutional maturation — but not that the political order would reorganize around the removed structure.
% FOUNDING_PROBLEM: At ratification the state faced revolutionary fragmentation: armed factions, threatened counterrevolution and foreign intervention, collapsed administration, and no civilian body able to command national loyalty. The charter's custodial clause was built to guarantee continuity and defense while civilian institutions consolidated.
% FOUNDING_PROBLEM_CORROBORATION: Independent corroboration exists for the founding problem's original reality: diplomatic archives, regional security studies, and founding-period histories from outside the benefiting parties confirm the ratification-time fragility. No source outside the benefiting parties attests the problem's present liveness — the custodian's continuing-threat claims rest on intelligence assessments the custodian itself produces, while the opposition's receded-threat reading is supported by the same archival record showing external-threat assessments declining after the founding decade. The two seats attest different statuses from the same documentary base.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε 0.58, end-of-interval) is substantial: the veto structure transfers legislative, appointment, and registration authority to the officer corps, and the holdings' privileges transfer economic rents — but part of the arrangement's cost is the genuine security and continuity function the founding era required, which this reading credits as guardianship overhead rather than extraction. Suppression (0.72) is high and is authored as a raw structural property — the engine scales only extractiveness, by directionality and scope; the emergency-law regime, the license system, and campus policing are the machinery that holds the settlement against its targets. Theater ratio (0.39) has risen from 0.20 as the founding problem receded: anniversary pageantry, staged parliamentary unanimity, and stability rhetoric increasingly substitute for the guardianship function they invoke. Accessibility collapse (0.50): elections and licensed parties persist, so alternatives are not fully foreclosed, but the specific alternative of civilian command authority is unreachable within the settlement. Resistance (0.60): recurring student upsurges, underground party networks, press defiance, and the court's dormant jurisdictional dissent. All three tracked series share one time grid (t=0,10,20,30,40,50,60) so no metric is sampled against another metric's end-state; the suppression series is authored because this story specifically tracks enforcement machinery maturing — emergency powers renewed, license control hardened, then plateauing once the settlement is consolidated. Coalition note: the victim seats (banned parties, students, press) have repeatedly attempted united fronts; the license system's calibration — splitting licensed from banned opposition — is precisely what prevents cross-seat coalition, which is why moderate-power victims have not converted their numbers into leverage.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently from identical structural facts. From the high command's position the arrangement is the state's spine — it built the settlement, allocates its rents, and experiences no extraction. Banned parties and the student movement experience the same structure as the closure of political life: trapped or constrained exit, full-target directionality, high effective extraction. Chartered loyalist parties sit between — protected from rivals yet revocable at the custodian's pleasure — and should compute as dual-positioned rather than cleanly beneficiary. The constitutional court is excluded rather than coordinated: its jurisdictional claims are the settlement's silenced counter-reading, and its dormancy is itself maintained by the appointment machinery. The engine derives these divergences from power, exit, and declared position; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries: military_officer_corps (organized/mobile) and military_affiliated_economic_holdings (institutional/mobile) derive near the beneficiary end — their exits are revolving doors and capital mobility inside the settlement, so effective extraction is damped or inverted into subsidy, which is descriptively right: the settlement subsidizes them. chartered_loyalist_parties are declared beneficiaries with constrained exit; the derivation places them low-d, but their benefit is revocable — that contingency is routed to the chartered_party_position_contingency omega rather than a directionality override, because it is a structural fact about the license system, not a mis-derivation. Declared victims: autonomous_political_parties (organized/trapped), student_movement (moderate/constrained), and independent_press (moderate/constrained) derive near the target end with extraction amplified by their weak exits. The high command carries agenda_setter with secondary beneficiary position and arbitrage exit, deriving nearest the beneficiary pole. Scope is national throughout — moderate verification difficulty, modest amplification. No directionality overrides are authored: no seat's derived d is wrong in a way the per-power-atom override mechanism could repair without distorting a same-power neighbor (the organized atom holds both a beneficiary party class and a victim party class).
 *
 * MANDATROPHY ANALYSIS:
 *   This is the canonical contested hybrid, and the classification discipline prevents both standard mislabels. The custodian's framing presents pure coordination — the guardian every fragile republic needs, a rope; the opposition's framing presents pure extraction — a snare in guardianship costume. Tangled rope holds both: a genuine founding coordination function (defense, continuity, crisis authority during consolidation) AND asymmetric extraction through the same veto structure, held together by active enforcement. The mandatrophy question — has the mandate outlived its function? — is routed to evidence rather than assumed: founding_problem_status is authored 'contested', the founding_threat_liveness omega names what would resolve it, and the R5 mismatch consumer reads status x disappearance_verdict (contested x world_rearranges → no zombie flag, correctly, because the parties genuinely dispute the founding problem's liveness). The temporal series carry the drift signal instead: extraction accumulation (0.40 → 0.58) and theater rise (0.20 → 0.39) trace the arrangement's movement from guardianship toward rent preservation across the interval. If the threat-liveness omega resolved 'receded', the same structure would re-read as mandate-dead persistence — the apparatus keeps that re-read available instead of freezing the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the july_charter_sovereign_legitimacy kernel (reading: military_custodian_reading). How would the sibling readings — secular_democratic_reading and guided_nationalism_reading — of the same charter text change this constraint''s structure?',
    'Constitutional-amendment politics, jurisdictional rulings by the constitutional court, and mass mobilization episodes that force the charter''s meaning to be re-adjudicated; ratification-era drafting records showing which clauses were contested and which drafters were excluded from the final text.',
    'Under the secular-democratic sibling, the veto authority is the constraint''s violation rather than its operation — the beneficiary and victim sets invert and the military''s prerogatives become the contested object. Under the nationalist sibling, the custodial veto is an instrument of a different legitimacy ground and its costs are assessed against religious-national criteria rather than custodial ones. This story''s ε, beneficiaries, and victims are valid only for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level reading contest: which reading of the July Charter''s legitimacy clause is instantiated changes the constraint''s victim set and classification.').

omega_variable(
    guardianship_stability_separability,
    'Is the stability function the custodian delivers separable from the custodial veto authority itself, or does stability in this polity actually depend on the veto?',
    'Comparative analysis of post-revolutionary states that civilianized command authority at comparable development stages, plus natural experiments where custodial veto was suspended during transition episodes and security outcomes observed.',
    'If separable, the veto''s extraction rides on a coordination function it does not itself provide, and the arrangement trends toward pure extraction; if inseparable, part of the measured extraction is the genuine price of the coordination and the tangled-rope reading strengthens against the snare reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guardianship_stability_separability, empirical, 'Whether the coordination function and the extraction mechanism are structurally separable.').

omega_variable(
    custodial_legitimacy_internalization,
    'Is civilian compliance with the custodial settlement coerced by the enforcement machinery, or internalized as belief in the guardian''s legitimacy?',
    'Political-behavior data from liberalization episodes: if compliance and deference persist where enforcement capacity recedes, the settlement is internalized; if contestation surges immediately when machinery is relaxed, compliance was coerced.',
    'If internalized, the suppression metric overstates active coercive force — the arrangement could persist even under weakened enforcement, and transition planning that dismantles machinery without addressing the belief structure would fail; if coerced, enforcement decay alone opens transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_legitimacy_internalization, empirical, 'Structural vs. internalized compliance mechanism in the custodial settlement.').

omega_variable(
    founding_threat_liveness,
    'Is the founding threat environment — counterrevolution, external intervention, revolutionary fragmentation — still live, or did it recede while the guardianship persisted and accreted rents?',
    'Declassified external-threat assessments across the interval, comparative security data for comparable post-revolutionary states, and the custodian''s internal planning documents against its public continuing-threat claims.',
    'If the threat receded, the founding problem is dead and the arrangement persists as privilege preservation — the mandate has outlived its function and the classification drifts from tangled_rope toward snare or piton; if live, the coordination function remains genuine and part of the measured extraction is its price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_threat_liveness, empirical, 'Liveness of the founding problem the custodial clause was built to solve.').

omega_variable(
    chartered_party_position_contingency,
    'Are the chartered loyalist parties genuine beneficiaries of the settlement, or contingent actors whose licensed position is itself a form of subordination?',
    'Registration-revocation episodes: how often licenses are withdrawn or platforms vetoed, whether chartered parties ever contest custodial prerogatives without penalty, and comparison of chartered-party policy ranges against the banned opposition''s platforms.',
    'If their position is contingent, their derived directionality is too beneficiary-leaning, the coordination function thins (a licensed market is not open contestation), and the victim set effectively includes the licensed opposition — pushing the arrangement''s computed classification toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chartered_party_position_contingency, conceptual, 'Whether licensed political parties count as beneficiaries or as contingent subordinates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(july_tr_t0, observed).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(july_tr_t10, observed).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(july_tr_t20, observed).
narrative_ontology:measurement(july_tr_t30, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(july_tr_t30, observed).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(july_tr_t40, observed).
narrative_ontology:measurement(july_tr_t50, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 50, 0.37).
narrative_ontology:measurement_basis(july_tr_t50, observed).
narrative_ontology:measurement(july_tr_t60, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement_basis(july_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(july_be_t0, observed).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(july_be_t10, observed).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(july_be_t20, observed).
narrative_ontology:measurement(july_be_t30, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement_basis(july_be_t30, observed).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(july_be_t40, observed).
narrative_ontology:measurement(july_be_t50, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement_basis(july_be_t50, observed).
narrative_ontology:measurement(july_be_t60, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(july_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(july_su_t0, observed).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(july_su_t10, observed).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(july_su_t20, observed).
narrative_ontology:measurement(july_su_t30, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(july_su_t30, observed).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(july_su_t40, observed).
narrative_ontology:measurement(july_su_t50, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(july_su_t50, observed).
narrative_ontology:measurement(july_su_t60, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(july_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% The July Charter's legitimacy clause decomposes into three structurally distinct readings of one kernel text (ε-invariance decomposition; the colloquial label 'the charter's legitimacy settlement' covers three different constraints). This story authors the military_custodian_reading: the standing arrangement is the charter's ratification of permanent military guardianship, and ε is assessed for that arrangement by the custodian reading's lights. The secular_democratic_reading authors the same text as mandating civilian supremacy — under it the veto authority is violation, not operation, and the beneficiary/victim sets invert; the guided_nationalism_reading authors the text as grounding legitimacy in religious-national identity, with the custodial veto as a compatible enforcement instrument. The readings form one constraint family; the custodial settlement structurally conditions which nationalist claims receive licensed expression, while the secular reading survives as the excluded drafters' position and the court's dormant jurisdictional claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
