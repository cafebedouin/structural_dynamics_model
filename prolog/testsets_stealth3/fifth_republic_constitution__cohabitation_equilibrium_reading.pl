% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Cohabitation Equilibrium of the Fifth Republic Dual Executive
 *   domain: political/constitutional/comparative-government
 *
 * SUMMARY:
 *   The Fifth Republic's constitution installs two executives with separate
 *   mandates: a president elected by universal suffrage who appoints the
 *   prime minister, and a government that conducts policy on the confidence
 *   of the National Assembly. This story instantiates one reading of that
 *   constitutional kernel — the cohabitation equilibrium — under which
 *   divergent electoral mandates oblige president and prime minister to
 *   negotiate the partition of authority: the presidency retains the reserved
 *   domains (foreign affairs, defense, the nuclear deterrent, summit
 *   representation) while the government of the assembly's majority runs
 *   domestic, economic, and social policy. The arrangement operated in three
 *   historical episodes (1986-88, 1993-95, 1997-2002), and each combined
 *   genuine coordination value — the republic crossed divided periods without
 *   constitutional rupture — with real asymmetric costs: each principal
 *   converted domain control into blocking power, and slowed legislation,
 *   contradictory administrative direction, and blurred accountability landed
 *   on the civil service, the electorate, and external partners. Since the
 *   2000 adoption of the five-year term aligned the electoral calendars, the
 *   machinery has lain dormant and its maintenance has grown increasingly
 *   doctrinal. The epsilon referent throughout is the standing
 *   negotiated-allocation arrangement itself as this reading assesses it —
 *   not the hyper-presidential or parliamentary arrangements the sibling
 *   readings endorse. The claim and the metrics are authored independently:
 *   the reading is claimed as a tangled hybrid of coordination and
 *   extraction, and the metric series describe its actual episodic operation.
 *
 * KEY AGENTS:
 *   - president_of_the_republic: co-principal agenda holder ([powerful]/[constrained]) — retains the reserved domains across every episode; blocked domestically during divergence
 *   - prime_minister_and_government: co-principal agenda holder ([institutional]/[constrained]) — commands domestic policy on assembly confidence; tenure recallable
 *   - national_assembly_majority: sustaining beneficiary ([organized]/[constrained]) — imposes its government and legislation; exposed to dissolution and confidence-vote procedures
 *   - french_electorate: dual-legitimacy source and diffuse bearer of incoherence ([organized]/[mobile]) — resets the whole configuration at each electoral cycle
 *   - senior_civil_service: implementation target ([moderate]/[identity_locked]) — absorbs contradictory directives; fused with state service as vocation
 *   - european_union_partners: excluded external cost-bearer ([institutional]/[trapped]) — needs one interlocutor, gets a rotating answer
 *   - conseil_constitutionnel_judges: analytical observer ([institutional]/[analytical]) — self-limited out of domain arbitration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.46).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.4).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Cohabitation Equilibrium of the Fifth Republic Dual Executive").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "political/constitutional/comparative-government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, 'c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a').
narrative_ontology:cs_kernel_codification('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', fixed_text).
narrative_ontology:cs_authority_grounding('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', lineage).
narrative_ontology:cs_interpretation_layer_present('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a').
narrative_ontology:cs_reading_relation('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', fifth_republic_constitution__hyper_presidential_reading, influences).
narrative_ontology:cs_reading_relation('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', fifth_republic_constitution__parliamentary_constraint_reading, influences).
narrative_ontology:cs_axiom('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', foundational, divergent_mandates_require_negotiated_allocation).
narrative_ontology:cs_axiom_status(divergent_mandates_require_negotiated_allocation, holdable).
narrative_ontology:cs_axiom_grounding('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', divergent_mandates_require_negotiated_allocation, conventional).
narrative_ontology:cs_axiom('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', foundational, dual_democratic_legitimacies_irreducible).
narrative_ontology:cs_axiom_status(dual_democratic_legitimacies_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', dual_democratic_legitimacies_irreducible, deontological).
narrative_ontology:cs_reference_frame('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', negotiated_domain_partition).
narrative_ontology:cs_drift_state('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', contemporary_quinquennat_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8bb10b9-3f9f-4ef0-84ec-25b7ae019b2a', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_the_republic).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_and_government).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, french_electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, senior_civil_service).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, french_electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_and_government).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, balanced_dual_executive_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, pluralite_des_majorites_convention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected directly by universal suffrage for a fixed term. Appoints the prime minister, may dissolve the National Assembly once per year, commands the armed forces and the nuclear deterrent, and by settled practice directs foreign policy, defense, and summit representation. When the parliamentary majority belongs to an opposing camp, the president keeps these reserved domains but loses control of legislation and domestic administration; signing ordinances, appointing ministers, and day-to-day governance require accommodation with a government the president did not choose. Early departure is possible in principle, has occurred once (1969), and carries heavy personal and constitutional cost.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_the_republic, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_the_republic, beneficiary).

% Leads the government, which determines and conducts domestic policy and holds statutory and regulatory power. Governs on the confidence of the National Assembly; in periods of divergent mandate the prime minister comes from the majority camp and runs the domestic agenda while negotiating space around the president's reserved domains. Tenure depends on holding assembly confidence and on the president's tolerance; resignation is the main lever and using it forfeits the office.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_and_government, agenda_setter,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_and_government, payer).

% The elected chamber majority that sustains the government, passes legislation and budgets, and can impose its preferred prime minister on a president of the opposite camp. Its leverage peaks exactly when it opposes the president; it remains exposed to annual dissolution and to the confidence-vote procedure a government can invoke to bypass amendments. Censure motions against a coexistence-period government have never gathered a workable majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, beneficiary,
    organized, biographical, constrained, national).

% Voters choose the president in one election and the assembly in another, sometimes months apart, and sometimes hand the winner of one a hostile majority in the other. A split result buys mutual checking of the two camps but blurs accountability: neither principal can be charged alone with policy success or failure, and programs stall or hybridize. The corrective instrument is the next election, which resets the entire configuration.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, french_electorate, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, french_electorate, beneficiary).

% Career administrators in ministries and prefectures execute whatever the government of the day decides while absorbing instructions shaped in the president's reserved domains. In divergent periods they receive contradictory priorities from the two executive offices, reconcile them informally, and slow execution to avoid committing either principal. Careers advance within the state apparatus itself, which most entered straight from the grandes ecoles; leaving means abandoning the profession and the idea of serving the state that constitutes their working identity.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, senior_civil_service, payer,
    moderate, generational, identity_locked, national).

% Other member states and community institutions conduct diplomacy and joint policy with France and need to know who speaks for it. In divergent periods the answer shifts by subject — the president in foreign councils, the finance minister in economic ones — and partners hedge, delay, or negotiate separately with both sides. They hold no seat in the French constitutional arrangement and cannot alter it; their recourse is patience and parallel channels.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, european_union_partners, excluded,
    institutional, generational, trapped, continental).

% The constitutional court reviews legislation and referrals but has historically declined to adjudicate jurisdictional disputes between president and government, judging that the political branches must settle domain questions themselves. It observes the arrangement, has occasionally clarified procedure (notably around the 1986 privatization ordinances), and leaves substantive allocation to negotiated practice.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, conseil_constitutionnel_judges, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_the_republic).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the legitimacy collision built into the 1958 design: two executives, each democratically mandated by a different route, must share the state when their mandates diverge, without either abolishing the other or freezing the machine. Negotiated partition of domains lets political alternation continue through divided periods.
% TRANSFER_FUNCTION: Moves agenda control between the two executive branches according to electoral outcomes: the presidency converts its fixed mandate into durable command of foreign, military, and nuclear policy; the government converts assembly confidence into command of legislation, regulation, and budgets. Decision speed and policy legibility move off the table, borne diffusely by implementers, voters, and external partners.
% ABSENT_VOICES: Voters preferring coherent single-camp governance, external partners needing one interlocutor, and the administrators executing the resulting directives have no seat at the negotiation. The constitutional court withdrew itself from domain arbitration by self-limitation, leaving allocation to the two interested principals and to the doctrine that grew up around them.
% DISAPPEARANCE_RATIONALE: Without the negotiated-allocation convention, a divergent mandate resolves by institutional combat — unsigned ordinances, refused appointments, dissolution threats answered with censure attempts — ending in either chronic paralysis or extra-legal presidential predominance. The 1958 designers built the dual structure because executive conflict had incapacitated its predecessor, and each of the three historical divided periods required the partition to complete an ordinary legislative term. Remove the equilibrium and alternation itself becomes the casualty.
% FOUNDING_PROBLEM: Built in 1958 to end the regime of assembly: chronic cabinet instability (average government life under a year), executive incapacity in the face of crises including Algeria. The dual executive installed a strong arbiter-president alongside a confidence-dependent government; the cohabitation question is that design's own sequel — what the allocation looks like when the arbiter's camp loses the assembly.
% FOUNDING_PROBLEM_CORROBORATION: Standard constitutional histories document Fourth Republic cabinet turnover averaging under a year and executive paralysis, corroborating the founding problem from outside any benefiting party; the 1958 consultative-committee records show the dual-executive design responding to it. The 2000 five-year-term referendum campaign supplies external corroboration of the sequel problem: its proponents argued openly that aligned calendars were needed to prevent renewed divided periods. Supporters of the rival readings dispute the status — one camp holds the problem permanently solved by a dominant presidency, another holds it solved by subordinating the presidency — which is why the status is recorded as contested rather than dead or live.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).
:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.46) is moderate because the arrangement's costs are episodic and reciprocal: each historical episode moved blocking leverage between the principals rather than concentrating it, and between episodes the arrangement is a low-load standby. Suppression (0.40) is structural — institutional veto points (dissolution, confidence votes, ordinance signatures, appointments) — with a smaller internalized component (the republican norm that a divided period must be made to work), roughly four parts structural to one part internalized; it is authored as a raw property and is not scaled by power or scope in the engine's computation. Accessibility collapse is low (0.35): both sibling readings remain live, amendment remains available, and the five-year term demonstrates the arrangement can be engineered around rather than escaped. Resistance is high (0.60): presidents fought the constraint's implications directly (the 1986 refusal to sign privatization ordinances; the five-year-term campaign launched after the 1997 dissolution backfired), and assemblies probed censure without ever assembling a majority. The three metric series share one ten-point grid, so every tracked metric is authored at every examined point. Extraction spikes at 1986, 1993, and 1997 are the mechanism firing on the electoral cycle, not noise: each divergence re-arms both principals' veto arsenals, and the suppression_requirement series tracks the same cycle — enforcement capacity built up sharply in each episode and demobilized afterward. The post-2002 theater rise records doctrinal maintenance of a dormant mechanism rather than functional decay of an active one; the mild 2016-2022 extraction uptick reflects renewed salience as a fragmented assembly revived anticipation of a new divided period. Receipt surface: the presidency is authored as the seat the gains accrue to, because the reserved-domain capture persisted through all three episodes and every alternation — the one accrual that never reset — while prime-ministerial and assembly gains expired with each cycle. Fixing cost is authored prohibitive: replacing the equilibrium requires constitutional revision that each camp vetoes because any fix redistributes power toward the other; the five-year term succeeded only because it engineered around the trigger without reallocating authority.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-holding seats (president, prime minister, assembly majority) experience the arrangement as workable arbitration: each holds a domain, each can veto the other, and each can claim the system preserved alternation without rupture. The bearing seats experience the identical structure as imposed incoherence: administrators receive contradictory priorities and slow execution to protect themselves, voters lose single-camp accountability, and external partners cannot tell who speaks for the country. Among the nominally co-equal executives the asymmetry is exit-based rather than rank-based: the president's mandate is term-fixed and effectively irrevocable mid-term, while the prime minister serves on recallable confidence — so the negotiation regime systematically favors the president's reserved domain, and the two same-level seats compute differently despite identical constitutional standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (president, prime minister and government, assembly majority) pull those seats toward the beneficiary end; victim declarations (electorate, senior civil service) pull theirs toward the target end, with the electorate's secondary beneficiary position tempering it toward symmetry. Two overrides correct derivations the role data alone cannot produce. First, powerful -> 0.25: the president is declared a beneficiary, which alone would undershoot the domestic blockage absorbed during divergence; the override prices the reserved-domain gain against the surrendered domestic agenda. Second, institutional -> 0.40: the prime minister's dual position — running domestic policy while serving on borrowed legitimacy — is not recoverable from declarations alone. Known limitation: the constitutional council (an observer seat, for which directionality is immaterial) and the external-partner seat share the institutional atom, and the override understates the partners' true target position (roughly 0.6 and above); the electorate and civil service are left to derivation, where victim status plus exit profile (mobile electorate, identity-locked administrators) places them correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-hybrid framing is what prevents mislabeling in both directions. Reading the historical record as pure coordination erases the measurable extraction — domain rents, slowed legislation, accountability blur — that each episode's losers documented in real time. Reading it as pure extraction erases the coordination achievement: three divided periods crossed with zero constitutional rupture, in a regime family where executive conflict had destroyed predecessors. The genealogy interview locates the residue: the founding problem (chronic cabinet instability) was largely cured, but the sub-problem this reading manages — reconciling dual legitimacy when mandates diverge — is contested rather than dead, and the arrangement now persists largely as standby doctrine whose upkeep grows theatrical as the shortened presidential term suppresses its trigger. That combination — contested problem status, world-rearranges verdict, rising theater on a dormant but plausibly re-triggerable mechanism — is exactly the signature handed to the lifecycle detectors rather than resolved by fiat here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the kernel fifth_republic_constitution (the cohabitation_equilibrium_reading). What structural elements locate the disagreement among the three readings, and what would the sibling readings change?',
    'Compare the sibling constraint files directly: the disagreement lives in Articles 5, 8, and 20-21 — the boundary between arbiter-president and executive-president. The hyper_presidential_reading shifts extraction onto the legislative seats and shrinks the victim set to governmental autonomy; the parliamentary_constraint_reading lowers measured extraction and makes the presidency itself the constrained party.',
    'Adopting the hyper-presidential sibling reclassifies this arrangement as heavily extractive toward the legislature; adopting the parliamentary sibling inverts the beneficiary and target sets. This file''s classification holds only under its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which kernel, which reading, where the readings diverge, what siblings would change.').

omega_variable(
    quinquennat_dormancy_terminal_or_latent,
    'Is the post-2002 dormancy of the negotiated-allocation machinery terminal decay (the mechanism will never fire again) or episodic latency (intact, awaiting the next divergent mandate, e.g. a 2027-style reversal)?',
    'Observe the next presidential-legislative divergence: if the incoming government of an opposing camp negotiates domain partition and the presidency accepts it, the mechanism is latent; if either side forces the old scripts to break (refused coexistence, constitutional standoff), decay is terminal or the reading itself has been repudiated.',
    'Terminal decay pushes this story toward inertial-theatrical territory (rising theater ratio already points that way); confirmed latency keeps the tangled coordination-plus-extraction structure authoritative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quinquennat_dormancy_terminal_or_latent, empirical, 'Whether the quinquennat suppressed the mechanism''s trigger or killed the mechanism.').

omega_variable(
    reserved_domain_bindingness,
    'Is the presidential reserved domain (foreign affairs, defense, nuclear, summitry) a binding convention of the arrangement, or a revocable practice that prime ministers may contest?',
    'Track prime-ministerial behavior in European and diplomatic councils across administrations: several have already nibbled at economic-diplomacy boundaries; systematic assertion of foreign-policy competence by a coexisting government would convert the convention into contested terrain.',
    'If revocable, the reading''s allocation map is unstable and measured extraction becomes bidirectional rather than presidency-favoring; if binding, the presidency''s durable capture (see gain_flow) is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserved_domain_bindingness, empirical, 'Bindingness of the domain-partition convention that defines this reading.').

omega_variable(
    reading_relation_edge_typing,
    'Are the edges from this reading to its siblings correctly typed as influences, or should they be coexists_with (static simultaneous holding by different factions)?',
    'Examine whether realized divided periods changed the siblings'' legitimacy conditions (they did — each episode was cited as proof against the hyper-presidential claim and for the parliamentary one), versus merely coexisting in discourse. If later episodes stop shifting the siblings'' standing, downgrade to coexists_with.',
    'Under influences, drift signals propagate along these edges in contamination analysis; under coexists_with, the siblings are independent and this reading''s fortunes do not forecast theirs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_edge_typing, conceptual, 'Framing under-determination in the typed edges between sibling readings.').

omega_variable(
    split_ticket_intentionality,
    'Do voters who produce a divided result intend mutual checking (making the electorate partly a beneficiary of the arrangement), or do they arrive at it accidentally and bear it as pure cost?',
    'Electoral-behavior studies of split-outcome periods: survey evidence on strategic balancing motives versus independent concurrent shocks (unemployment, scandal) driving the two votes apart.',
    'If balancing is intentional, the electorate seat sits nearer symmetry than its victim declaration suggests and the accountability-blur cost is a price knowingly paid; if accidental, the seat is a clean target and effective extraction on it is understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(split_ticket_intentionality, preference, 'Whether the electorate''s mixed position is chosen balance or suffered accident.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1958, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1958, 0.18).
narrative_ontology:measurement_basis(fift_tr_t1958, observed).
narrative_ontology:measurement(fift_tr_t1969, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1969, 0.17).
narrative_ontology:measurement_basis(fift_tr_t1969, observed).
narrative_ontology:measurement(fift_tr_t1981, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1981, 0.16).
narrative_ontology:measurement_basis(fift_tr_t1981, observed).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.12).
narrative_ontology:measurement_basis(fift_tr_t1986, observed).
narrative_ontology:measurement(fift_tr_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1993, 0.13).
narrative_ontology:measurement_basis(fift_tr_t1993, observed).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1997, 0.12).
narrative_ontology:measurement_basis(fift_tr_t1997, observed).
narrative_ontology:measurement(fift_tr_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2002, 0.24).
narrative_ontology:measurement_basis(fift_tr_t2002, observed).
narrative_ontology:measurement(fift_tr_t2008, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2008, 0.31).
narrative_ontology:measurement_basis(fift_tr_t2008, observed).
narrative_ontology:measurement(fift_tr_t2016, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2016, 0.35).
narrative_ontology:measurement_basis(fift_tr_t2016, observed).
narrative_ontology:measurement(fift_tr_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement_basis(fift_tr_t2022, observed).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1958, 0.28).
narrative_ontology:measurement_basis(fift_be_t1958, observed).
narrative_ontology:measurement(fift_be_t1969, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1969, 0.3).
narrative_ontology:measurement_basis(fift_be_t1969, observed).
narrative_ontology:measurement(fift_be_t1981, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1981, 0.33).
narrative_ontology:measurement_basis(fift_be_t1981, observed).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.64).
narrative_ontology:measurement_basis(fift_be_t1986, observed).
narrative_ontology:measurement(fift_be_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1993, 0.59).
narrative_ontology:measurement_basis(fift_be_t1993, observed).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1997, 0.63).
narrative_ontology:measurement_basis(fift_be_t1997, observed).
narrative_ontology:measurement(fift_be_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2002, 0.52).
narrative_ontology:measurement_basis(fift_be_t2002, observed).
narrative_ontology:measurement(fift_be_t2008, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2008, 0.47).
narrative_ontology:measurement_basis(fift_be_t2008, observed).
narrative_ontology:measurement(fift_be_t2016, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2016, 0.44).
narrative_ontology:measurement_basis(fift_be_t2016, observed).
narrative_ontology:measurement(fift_be_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2022, 0.46).
narrative_ontology:measurement_basis(fift_be_t2022, observed).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement_basis(fift_su_t1958, observed).
narrative_ontology:measurement(fift_su_t1969, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1969, 0.29).
narrative_ontology:measurement_basis(fift_su_t1969, observed).
narrative_ontology:measurement(fift_su_t1981, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1981, 0.28).
narrative_ontology:measurement_basis(fift_su_t1981, observed).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.72).
narrative_ontology:measurement_basis(fift_su_t1986, observed).
narrative_ontology:measurement(fift_su_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1993, 0.64).
narrative_ontology:measurement_basis(fift_su_t1993, observed).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1997, 0.66).
narrative_ontology:measurement_basis(fift_su_t1997, observed).
narrative_ontology:measurement(fift_su_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2002, 0.48).
narrative_ontology:measurement_basis(fift_su_t2002, observed).
narrative_ontology:measurement(fift_su_t2008, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2008, 0.42).
narrative_ontology:measurement_basis(fift_su_t2008, observed).
narrative_ontology:measurement(fift_su_t2016, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2016, 0.39).
narrative_ontology:measurement_basis(fift_su_t2016, observed).
narrative_ontology:measurement(fift_su_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2022, 0.4).
narrative_ontology:measurement_basis(fift_su_t2022, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Fifth Republic's dual executive' covers three structurally distinct constraints — one per reading of the shared kernel. This file (cohabitation equilibrium) authors the negotiated-partition arrangement: moderate, episodic, bidirectional extraction with the polity bearing coherence costs. The hyper-presidential sibling authors a higher-extraction arrangement targeting legislative and governmental autonomy during aligned periods; the parliamentary-constraint sibling authors a lower-extraction arrangement with the presidency itself as the constrained seat. The siblings are downstream of the same upstream ambiguity (Articles 5/8/20-21), and each cites episodes of this arrangement as evidence for its own claim — hence this reading influences both. Epsilon differs across the family by construction; no single file may average across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, powerful, 0.25).
constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
