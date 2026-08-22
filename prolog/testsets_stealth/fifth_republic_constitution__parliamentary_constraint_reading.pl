% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Parliamentary Constraint on the Executive (Parliamentary Constraint Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Fifth Republic's confidence architecture, read through the
 *   parliamentary constraint lens: the President of the Republic, though
 *   directly elected, is a coordinated executive whose policy implementation
 *   must pass through legislative authorization. The government is appointed
 *   by the president but exists at the National Assembly's confidence; the
 *   Assembly can amend, delay, or block the executive's bills, refuse the
 *   authorizations its program requires, and censure the government (Articles
 *   20 and 49). When the Assembly withholds confidence or blocks legislation,
 *   the executive enters the arrangement's victim set: a president facing a
 *   hostile or hung Assembly cannot implement his program without negotiated
 *   authorization. The claim/metric split is deliberate: the constraint is
 *   claimed as tangled_rope — a genuine coordination function
 *   (executive-legislative alignment, cabinet stability) carrying asymmetric
 *   extraction (executive autonomy converted into majority agenda control)
 *   under active enforcement — while the metrics are authored from the
 *   arrangement's observed operation across unified, cohabitation, and
 *   hung-parliament phases.
 *
 * KEY AGENTS:
 *   - national_assembly_majority: agenda-setter and primary beneficiary (organized/constrained) — holds the confidence power; converts the executive's authorization need into agenda control
 *   - french_presidency: primary target (powerful/constrained) — must route policy implementation through authorization; retains dissolution, referendum, and emergency counter-tools that bound but do not remove the severity
 *   - prime_minister_government: secondary target and coordination interface (powerful/trapped) — exists at the Assembly's confidence; collects delegated authority through the same structure that binds it
 *   - sovereign_electorate: ultimate beneficiary (organized/constrained) — collects accountability diffusely and periodically through its Assembly majority
 *   - parliamentary_minority: excluded (organized/mobile) — present in the chamber, locked out of the confidence calculus
 *   - constitutional_council: analytical observer (institutional/analytical) — adjudicates the authorization boundary (ordinance delegations, 49.3 scope, Article 5/20 division)
 *   - eu_institutions: excluded inter-institutional party (institutional/trapped) — bears compliance consequences of blocked legislation with no seat in the confidence relationship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.68).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.62).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Parliamentary Constraint on the Executive (Parliamentary Constraint Reading)").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional/political").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, 'a6342a94-2a7a-4bea-98a3-16c9df3b6d20').
narrative_ontology:cs_kernel_codification('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', fixed_text).
narrative_ontology:cs_authority_grounding('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', lineage).
narrative_ontology:cs_interpretation_layer_present('a6342a94-2a7a-4bea-98a3-16c9df3b6d20').
narrative_ontology:cs_reading_relation('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', foundational, executive_policy_requires_assembly_authorization).
narrative_ontology:cs_axiom_status(executive_policy_requires_assembly_authorization, holdable).
narrative_ontology:cs_axiom_grounding('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', executive_policy_requires_assembly_authorization, conventional).
narrative_ontology:cs_axiom('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', foundational, popular_sovereignty_exercised_through_parliamentary_mediation).
narrative_ontology:cs_axiom_status(popular_sovereignty_exercised_through_parliamentary_mediation, holdable).
narrative_ontology:cs_axiom_grounding('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', popular_sovereignty_exercised_through_parliamentary_mediation, deontological).
narrative_ontology:cs_reference_frame('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', rationalized_parliamentary_government).
narrative_ontology:cs_drift_state('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', contemporary_hung_parliament_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a6342a94-2a7a-4bea-98a3-16c9df3b6d20', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, sovereign_electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, french_presidency).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the confidence power over the government: it can censure the government out of office, amend or block its bills, refuse the authorizations its program requires, and control the committee agenda. The executive's need for authorization is what gives the majority its leverage; what flows to it is policy-setting authority converted from presidential autonomy. Its enforcement is active — censure motions, authorization refusals, legislative blocking — and it cannot leave the confidence system without dissolving itself.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, beneficiary).

% A directly elected president who must route domestic policy implementation through a government that depends on Assembly confidence. When the Assembly withholds confidence or blocks legislation, the president's program stalls and he bears the political cost. He retains counter-tools inside the system — dissolution of the Assembly, the Article 11 referendum, ordinance delegations, and Article 16 emergency powers — which bound the arrangement's severity but are not exits from it. The arrangement bites hardest when no majority aligns with him.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, french_presidency, payer,
    powerful, biographical, constrained, national).

% Appointed by the president but existing at the Assembly's pleasure: it must win and keep confidence for every major policy, and when confidence is withdrawn it falls, as in 2024. Through the same structure that binds it, it collects delegated authority — ordinance enabling laws and the 49.3 commitment procedure that forces adoption of a text unless censured. Its horizon is short because censure risk is standing; leaving the confidence relation means resigning.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_government, payer,
    powerful, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_government, beneficiary).

% Collects an accountability channel: through its Assembly majority, the electorate can withdraw confidence from an executive whose policy it rejects, at elections on a fixed cycle. The benefit is diffuse and periodic, and mediated — voters reach the executive only through the legislature they elect. They cannot exit the constitutional order, only contest it at the ballot.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, sovereign_electorate, beneficiary,
    organized, generational, constrained, national).

% Holds seats and amendment rights but sits outside the confidence calculus: its amendments fail, its censure motions cannot pass without majority defection, and its legislative initiatives die in committee. It would demand a proportional share of the agenda-setting power the majority monopolizes. Its recourse is the next election, committee obstruction, and public opinion.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_minority, excluded,
    organized, biographical, mobile, national).

% Adjudicates where the authorization boundary falls: the scope of ordinance delegations, the limits of the 49.3 commitment, and the division of authority between the president's arbitration function and the government's direction of policy. Its rulings shape whose acts require authorization without itself collecting from or paying into the arrangement.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Requires transposition of directives and budgetary compliance from France. When the confidence majority blocks the legislation compliance requires, the EU bears the non-compliance and can respond only through infringement and deficit procedures — it has no seat in the confidence relationship whose outcomes bind it.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, eu_institutions, excluded,
    institutional, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns policy implementation with the legislative majority that carries electoral representation: cabinet formation, program approval, budget authorization, and legislative passage are decided in one confidence relationship, so the executive cannot pursue a program the Assembly's majority will not underwrite.
% TRANSFER_FUNCTION: Transfers policy-setting authority and implementation authorization from the executive (presidency and government) to the Assembly's majority, and transfers political risk — censure, resignation — onto the government. The executive gives up unilateral implementation; the majority gains agenda control; the electorate receives an accountability channel.
% ABSENT_VOICES: The parliamentary minority and the voters behind it sit in the chamber but outside the confidence calculus — their amendments fail and their censures cannot pass without majority defection. EU institutions bear the compliance consequences of blocked legislation with no seat in the relationship whose outcomes bind them.
% DISAPPEARANCE_RATIONALE: If the confidence constraint vanished overnight, the president would implement policy through the government by decree and ordinance without authorization, the Assembly would collapse toward a consultative chamber, the accountability channel between electorate and executive would close, and cabinet survival would rest on presidential pleasure rather than parliamentary confidence — the French policy process would reorganize around the Elysee.
% FOUNDING_PROBLEM: The Fourth Republic's assembly regime (1946-1958): twenty-two governments in twelve years, incoherent coalition majorities, and an executive unable to act — culminating in the 1958 crisis over Algeria that brought de Gaulle to power with a mandate to rationalize parliament.
% FOUNDING_PROBLEM_CORROBORATION: The 1958 constituent debates and constitutional historians outside the beneficiary set attest the design target was cabinet instability, addressed through rationalized parliamentarism. Doctrinal scholarship and Conseil constitutionnel practice attest the mechanism's operative function shifted toward constraining a directly elected presidency — a function its authors did not center. No outside source attests that the founding problem in its original form still governs the arrangement; the dispute between the stability reading and the accountability reading is documented across the doctrinal literature.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68 (interval end) because the authorization requirement converts real executive autonomy into majority agenda control, but it is bounded: the presidency retains dissolution, referendum, ordinance delegations, and Article 16, so the arrangement extracts without eliminating executive capacity. Suppression (0.62) reflects the enforcement machinery the arrangement requires — confidence votes, censure motions (passed against a government in 1962 and again in 2024), and the blocking power of a hostile majority; suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and national-scope verification difficulty. Theater (0.30) is moderate: confidence debates are partly ritual under unified majorities (questions asked and won), real under hung parliaments. Accessibility collapse is low (0.30): alternatives to legislative authorization remain accessible from the executive seat. Resistance is moderate-high (0.55): cohabitation presidents, reliance on 49.3, and dissolution threats are the executive's active counter-moves. The temporal series run on one shared grid — every tracked metric is authored at every time point — and are coalition-phase-driven rather than monotonic: extraction and enforcement spike in cohabitation and hung-parliament phases (1986, 1997, 2022-2025) and relax under unified majorities (1968, 2008). The oscillation is a structural property of semi-presidential coordination, not an intermittent-reinforcement mechanism; the base_properties scalars state the interval-end, enforced-phase state.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat (Assembly majority) experiences the arrangement as its constitutional prerogative — the point of the regime. The presidency experiences it as a binding limitation whose severity tracks the coalition state: near-inoperative under unified government, acute under cohabitation and hung parliaments. The government sits between: the same confidence relation is the source of its existence and the standing threat to it. Two same-power executive seats (presidency, government) diverge on exit options — the presidency retains dissolution, referendum, and Article 16; the government is trapped in the confidence relation it lives by — so the engine should compute different per-seat classifications from equal nominal power.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (national_assembly_majority, sovereign_electorate) derive low directionality — the arrangement subsidizes them with agenda control and an accountability channel. Declared victims (french_presidency, prime_minister_government) derive high directionality — they bear the authorization requirement and censure risk. The government's dual position (secondary beneficiary: it collects delegated authority through ordinance enabling laws and the 49.3 commitment) moderates its effective extraction below the presidency's. The electorate's benefit is diffuse and periodic, reached only through its Assembly majority. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Fourth Republic cabinet instability) is dead in its original form; the arrangement persists on a successor function — accountability for a directly elected presidency — and the status is authored contested rather than resolved, because the parties dispute which problem the arrangement now solves. The tangled_rope classification prevents two mislabels: the reading's own framing would present the arrangement as pure democratic coordination (rope), which the victim declarations and active-enforcement requirement refuse; a snare label would erase the genuine coordination function that stabilizes cabinets and aligns programs with representation. The mismatch consumer sees status=contested with verdict=world_rearranges and no dead-problem zombie flag, because the successor function is corroborated from outside the beneficiary set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading — the parliamentary_constraint_reading — of the fifth_republic_constitution kernel. The sibling readings (hyper_presidential_reading: president as direct sovereign minimally constrained by the legislature; cohabitation_equilibrium_reading: dual executive with negotiated authority allocation) instantiate different constraints with different victim sets and extraction structures. What follows for classification if a sibling reading is adopted instead?',
    'Comparative authoring of the sibling stories: the hyper_presidential_reading removes the executive from the victim set and places the legislative majority there (the arrangement becomes a limit on parliament); the cohabitation_equilibrium_reading makes the victim/beneficiary structure symmetric and period-dependent. The disagreement''s location is the locus of executive authority: whether the president''s Article 5 domain (arbitration, foreign policy, referendum) stands inside or outside the authorization requirement.',
    'Under the hyper_presidential_reading this story''s beneficiaries and victims swap and extraction rises on the legislative seat; under the cohabitation_equilibrium_reading the extraction structure becomes alternating rather than fixed, and no single seat holds a stable beneficiary position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest: which reading of the Fifth Republic''s executive-legislative kernel governs classification.').

omega_variable(
    article_5_article_20_reserved_domain,
    'How broad is the president''s reserved domain outside the authorization requirement — arbitration, foreign policy, defense, referendum — and does the parliamentary reading''s own logic extend authorization to all policy implementation or only to domestic legislative implementation?',
    'Doctrinal analysis of Conseil constitutionnel jurisprudence and practice across the cohabitations, where presidents claimed the reserved domain and parliaments acquiesced; measure the share of presidential action taken without Assembly authorization.',
    'A broad reserved domain confines the arrangement''s extraction to domestic policy and lowers epsilon; the reading''s maximal version (all implementation authorized) raises epsilon and widens the victim set to every presidential act.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_5_article_20_reserved_domain, conceptual, 'Boundary of the presidential reserved domain versus the authorization requirement.').

omega_variable(
    unified_vs_divided_extraction,
    'Is the arrangement''s extraction on the executive a structural constant or a coalition-state property — near-inoperative under unified government (the 1968-2017 pattern) and acute under cohabitation and hung parliaments (1986, 1997, 2022-2025)?',
    'Comparative measurement of authorization refusals, amendment rates, and censure activity across unified, cohabitation, and hung-parliament phases; treat epsilon as a distribution over coalition states if the variance is structural.',
    'If coalition-dependent, the classification should be read at the enforced state (hung parliament) and the base scalar understates enforcement-phase extraction; if structural, the scalar is stable and the phases are noise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unified_vs_divided_extraction, empirical, 'Whether extraction is coalition-state-dependent or structural.').

omega_variable(
    article_49_3_counter_extraction,
    'Does the 49.3 commitment procedure invert the arrangement''s direction — the executive extracting from the Assembly''s amendment power by forcing adoption of a text absent censure — such that the net extraction borne by the executive seat is lower than the confidence structure alone suggests?',
    'Measure amendment survival rates for 49.3-forced texts versus the same texts under ordinary procedure; count how often the threat of commitment extracted majority acquiescence without a vote.',
    'If 49.3 is an extractive counter-channel, the arrangement contains bidirectional extraction and the executive seat''s net directionality falls; if it is rationalized parliamentarism''s coordination device (as this reading holds), the confidence structure''s extraction stands as measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_49_3_counter_extraction, conceptual, 'Whether 49.3 counter-extracts from the Assembly, offsetting the extraction borne by the executive.').

omega_variable(
    dissolution_deterrence_effect,
    'Does the president''s dissolution power (Article 12) deter the Assembly''s enforcement — as in 1962 and 1997, where dissolution punished or pre-empted hostile majorities — such that the arrangement''s real extraction on the executive is lower than its formal structure suggests?',
    'Code Assembly enforcement episodes (censure attempts, authorization refusals) for dissolution risk at the time of the episode; test whether enforcement intensity drops as dissolution probability rises.',
    'If dissolution deters enforcement, the arrangement''s effective extraction is self-limiting and lower than measured; the victim set remains but the bite is bounded by the counter-tool. If dissolution no longer deters (fragmented Assemblies where dissolution risks backfiring, 2022-2025), enforcement is unbound and epsilon rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissolution_deterrence_effect, empirical, 'Whether the dissolution counter-tool deters Assembly enforcement and bounds extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 1958, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(fift_tr_t1962, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(fift_tr_t1968, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1968, 0.22).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1986, 0.15).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1997, 0.16).
narrative_ontology:measurement(fift_tr_t2008, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2008, 0.28).
narrative_ontology:measurement(fift_tr_t2018, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(fift_tr_t2025, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement(fift_be_t1962, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1962, 0.45).
narrative_ontology:measurement(fift_be_t1968, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1968, 0.3).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1986, 0.58).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1997, 0.62).
narrative_ontology:measurement(fift_be_t2008, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(fift_be_t2018, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(fift_be_t2025, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1958, 0.4).
narrative_ontology:measurement(fift_su_t1962, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1962, 0.6).
narrative_ontology:measurement(fift_su_t1968, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1968, 0.25).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1986, 0.5).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1997, 0.55).
narrative_ontology:measurement(fift_su_t2008, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2008, 0.42).
narrative_ontology:measurement(fift_su_t2018, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2018, 0.52).
narrative_ontology:measurement(fift_su_t2025, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Fifth Republic's executive-legislative balance' decomposes into three epsilon-invariant readings of one kernel (fifth_republic_constitution): this parliamentary_constraint_reading (executive bears the arrangement; beneficiary is the Assembly majority; bounded moderate epsilon), the hyper_presidential_reading (president as direct sovereign; the legislature bears the arrangement), and the cohabitation_equilibrium_reading (symmetric negotiated dual executive, period-dependent positions). Each has its own epsilon, victim set, and classification; they are linked here as a constraint family. This file instantiates only the parliamentary reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
