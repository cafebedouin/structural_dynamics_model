% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Coordinate Construction Reading: Three Co-Equal Branches With Distributed Interpretive Authority and No Final Arbiter
 *   domain: legal/political philosophy/institutional design
 *
 * SUMMARY:
 *   The arrangement under description: a constitutional order in which three
 *   co-equal branches each interpret the constitution within its own sphere,
 *   with no branch holding final, unchallengeable interpretive authority.
 *   Each branch's reading is exposed to challenge by the other two;
 *   legislative override and executive non-acquiescence operate as standing
 *   correction mechanisms rather than anomalies. The arrangement solves the
 *   interpretive-tyranny problem by refusing to concentrate the solution
 *   anywhere, and pays for that refusal in permanent inter-branch friction
 *   whose costs land below the branch level. KEY AGENTS (by structural
 *   relationship): legislative chambers (institutional/constrained) —
 *   beneficiary of protected statutory readings, payer of override and
 *   deadlock costs; executive officeholders (institutional/constrained) —
 *   beneficiary via enforcement-discretion interpretive space, payer of
 *   rejected actions; judicial officeholders (institutional/identity_locked)
 *   — beneficiary via tenured interpretive sphere, payer of ignored rulings,
 *   professionally fused with the interpreter role; constitutional minorities
 *   (powerless/constrained) — collect multi-venue access; policy-seeking
 *   coalitions (organized/constrained) — bear the multi-filter delay;
 *   crisis-exposed populations (powerless/trapped) — bear deadlock timing
 *   costs directly; general electorate (organized/trapped) — pays
 *   accountability fog; subnational governments and indigenous nations
 *   (excluded) — governed by outputs they cannot contest from within;
 *   constitutional scholarship (analytical) — observes the full structure.
 *
 * KEY AGENTS:
 *   - - legislative_chambers: branch seat (institutional/constrained) — protected statutory readings collected; override and deadlock costs paid
 *   - - executive_officeholders: branch seat (institutional/constrained) — enforcement-discretion interpretive space collected; rejection and freeze costs paid
 *   - - judicial_officeholders: branch seat (institutional/identity_locked) — tenured interpretive sphere collected; unenforced-ruling costs paid; identity fused with the interpreter role
 *   - - constitutional_minorities: dispersed claimants (powerless/constrained) — multi-venue access collected; exhaustion risk carried
 *   - - policy_seeking_coalitions: demand carriers (organized/constrained) — multi-filter delay and dilution borne
 *   - - crisis_exposed_populations: timing-cost bearers (powerless/trapped) — absorb deadlock elapsed time directly
 *   - - general_electorate: principal (organized/trapped) — mutual-veto protection collected; accountability fog paid
 *   - - subnational_governments: absent voice (organized/constrained) — bound by outputs, no seat
 *   - - indigenous_nations: absent voice (organized/trapped) — treaty readings redefined without consent or exit
 *   - - constitutional_scholarship: analytical observer — sees the full structure across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.46).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.3).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction Reading: Three Co-Equal Branches With Distributed Interpretive Authority and No Final Arbiter").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "legal/political philosophy/institutional design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, 'cfcf5448-d2a5-4f11-b62d-25f98275964c').
narrative_ontology:cs_kernel_codification('cfcf5448-d2a5-4f11-b62d-25f98275964c', fixed_text).
narrative_ontology:cs_authority_grounding('cfcf5448-d2a5-4f11-b62d-25f98275964c', distributed).
narrative_ontology:cs_reading_relation('cfcf5448-d2a5-4f11-b62d-25f98275964c', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('cfcf5448-d2a5-4f11-b62d-25f98275964c', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('cfcf5448-d2a5-4f11-b62d-25f98275964c', foundational, no_final_interpretive_arbiter).
narrative_ontology:cs_axiom_status(no_final_interpretive_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('cfcf5448-d2a5-4f11-b62d-25f98275964c', no_final_interpretive_arbiter, instrumental).
narrative_ontology:cs_axiom('cfcf5448-d2a5-4f11-b62d-25f98275964c', secondary, branch_interpretive_equality).
narrative_ontology:cs_axiom_status(branch_interpretive_equality, holdable).
narrative_ontology:cs_axiom_grounding('cfcf5448-d2a5-4f11-b62d-25f98275964c', branch_interpretive_equality, deontological).
narrative_ontology:cs_reference_frame('cfcf5448-d2a5-4f11-b62d-25f98275964c', coordinate_departmental_equilibrium).
narrative_ontology:cs_drift_state('cfcf5448-d2a5-4f11-b62d-25f98275964c', contemporary_judicial_ascendancy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cfcf5448-d2a5-4f11-b62d-25f98275964c', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_chambers).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, executive_officeholders).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, judicial_officeholders).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, constitutional_minorities).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, policy_seeking_coalitions).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, crisis_exposed_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, general_electorate).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, legislative_chambers).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, executive_officeholders).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, judicial_officeholders).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, general_electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Passes statutes under its own reading of the constitution and asserts interpretive prerogative over the texts it enacts. Its readings are insulated from unilateral reversal by the other branches, which is what it collects from the arrangement. It pays when enacted laws are struck down or left unexecuted, and when its agenda stalls in inter-branch disagreement; its recourse is to legislate again, attempt override, or pursue amendment, all of which are slow and politically expensive.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislative_chambers, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, legislative_chambers, payer).

% Interprets the constitution through enforcement choices, signing statements, and administrative action, and is shielded from having any other branch's reading imposed on it without its participation. It pays when courts reject its actions, when legislatures cut off funding or authority, and when its programs are frozen in inter-branch dispute. Its levers are enforcement discretion, appointment, and public mobilization rather than exit.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, executive_officeholders, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, executive_officeholders, payer).

% Holds life tenure and issues rulings under its own constitutional readings, protected from electoral removal and from legislative veto over the rulings themselves. Its professional identity is constituted by the role of constitutional interpreter; abandoning the claim to interpretive authority would dissolve what it is to be a judge. It pays when rulings are openly ignored, under-enforced, or stripped of practical effect, costs it absorbs without any channel of retaliation beyond further rulings.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judicial_officeholders, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, judicial_officeholders, payer).

% Groups whose preferred policies or rights claims lose with the current majority. The arrangement gives them multiple independent venues in which to press a claim: a loss in one branch does not close the others, so a rejected petition can be renewed as legislation, litigation, or executive initiative. The double edge is that winning ultimately requires assent across every venue rather than persuading a single decision-maker, which can convert protection into exhaustion.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_minorities, beneficiary,
    powerless, generational, constrained, national).

% Organized interests and electoral majorities carrying specific policy demands. Their demands pass through three independent interpretive filters before taking effect, so they routinely see platforms diluted, delayed, or killed in venues they did not contest directly, even after winning elections. They can shift strategy between branches but cannot leave the system, and the delay itself consumes their coalition's cohesion and resources.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, policy_seeking_coalitions, payer,
    organized, biographical, constrained, national).

% Populations needing timely collective response — disaster relief, pandemic containment, fiscal stabilization — whose needs arrive on nature's schedule rather than the institutional calendar. When branches deadlock over the constitutional propriety of a response, these populations absorb the elapsed time directly: aid arrives late, diluted, or not at all. They cannot exit the polity, switch venues, or wait out another election cycle.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, crisis_exposed_populations, payer,
    powerless, immediate, trapped, national).

% Votes as the ultimate principal over all three branches but cannot cleanly assign responsibility for outcomes, because each branch attributes failure to the others' interference. It collects the mutual-veto guarantee against rapid capture by any single organ, and pays in accountability fog: punished governments blame obstructive courts or obstructionist legislatures, and voters face difficulty connecting results to anyone removable.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, general_electorate, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, general_electorate, beneficiary).

% State and provincial governments bound by whatever meaning the federal branches' interpretive contest produces, with no vote in that contest itself. They absorb interpretive whiplash — mandates, funding shifts, and preemption whose constitutional footing changes as the federal balance tilts — and would press for stability and subsidiarity guarantees if admitted to the conversation.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, subnational_governments, excluded,
    organized, generational, constrained, regional).

% Nations with treaty relationships mediated entirely through the federal branches' readings of constitutional and treaty text, holding standing in the documents but no seat in the inter-branch interpretive process that redefines those readings. Shifts in which branch's interpretation prevails change the practical force of treaties and trust obligations without their consent, and they have no exit from the framework that governs them.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, indigenous_nations, excluded,
    organized, generational, trapped, national).

% Academic and comparative constitutional scholarship spanning multiple jurisdictions. It maps how the arrangement operates, tracks the historical movement between coordinate and concentrated-finality practice, and supplies the external vantage point from which the branches' self-descriptions can be checked against behavior.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates ultimate interpretive authority so that no single branch can fix the constitution's meaning unilaterally: each branch reads the text within its sphere, and every branch's reading is subject to challenge by the other two, producing a mutual-veto equilibrium that protects each branch's interpretive autonomy and the polity against interpretive monoculture.
% TRANSFER_FUNCTION: Moves interpretive authority and agenda-setting leverage among the three branches as each contests the others' readings, and moves the costs of constitutional conflict — delay, deadlock, and diffused accountability — onto policy-seeking coalitions, crisis-exposed populations, and the electorate, who bear them without a seat in the contest.
% ABSENT_VOICES: Subnational governments and indigenous nations live under the outputs of the federal interpretive contest with no vote in it; they would object to interpretive instability and to unilateral redefinition of obligations touching them. Ordinary citizens participate only as an electorate that cannot assign blame within the contest. Founding-era departmentalists survive in scholarship but are absent from institutional practice.
% DISAPPEARANCE_RATIONALE: If the coordinate boundary vanished overnight, one branch would consolidate final interpretation — courts by doctrinal accumulation, the legislature by definitional statute or override, the executive by enforcement discretion — and the other branches would reorganize around either acquiescence or overt defiance. The legal system's background assumption that constitutional meaning is contested but stably multi-sourced would collapse into judicial finality, legislative definition, or executive construction, each a different operating world for every named seat.
% FOUNDING_PROBLEM: Post-revolutionary experience with unified sovereign power posed the question of who guards the guardians: any single organ claiming definitive constitutional interpretation becomes a self-interested arbiter positioned above the people's other representatives. The arrangement answers by making each watcher answerable to the others — no final arbiter, because any final arbiter would eventually serve itself.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from outside the benefiting parties: rival-reading advocates themselves attest that the founding generation operated on coordinate understandings (judicial-supremacy and parliamentary-primacy partisans concede the departmentalist genealogy while disputing its adequacy); comparative constitutional scholarship on presidential systems and weak-form veto players independently documents the design's logic; and historical records of inter-branch override attempts corroborate that the problem framing, not just the arrangement, drove behavior. No branch's own self-description is treated as probative.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__coordinate_construction_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).
:- end_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.46 (current, matching the 2026 measurement endpoint) because the arrangement generates no concentrated rents — no seat collects a revenue stream from the others' conduct — but imposes real, recurring costs through its own operation: multi-venue filtering of policy demands, crisis-window deadlock, and accountability diffusion that lets each branch deflect blame. Suppression is authored at 0.30 as a raw structural property (unscaled by power or scope, unlike extractiveness): alternatives within the design space are not suppressed — override statutes, non-acquiescence, amendment, and rival institutional designs remain live options — but holding the equilibrium does require continuous active resistance to encroachment, which is enforcement effort rather than exit-blocking. Theater ratio is authored at 0.42: the checking functions are substantively real, but a growing share of activity is ceremonial assertion of co-equality that masks an actual tilt (see the theater_ratio series and the drift discussion in kernel_context), approaching but not crossing the proxy-substitution threshold. Accessibility collapse is low (0.25) because rival constitutional designs and rival readings remain fully articulable and politically live; resistance is high (0.58) because every branch periodically contests the boundary itself and the contest is the arrangement's normal condition. Claim and metrics are authored independently: claimed_type tangled_rope rests on structure (a genuine coordination function — mutual veto against interpretive tyranny — operating through the same channels that impose asymmetric-feeling costs on non-branch seats, held together by active enforcement), not on any tuning toward predicted engine output. The extractiveness series is cyclical rather than monotonic: crisis episodes (Reconstruction-era breakdown circa 1866, New Deal confrontation circa 1937) spike inter-branch conflict costs, followed by settlement-driven relaxation; the cycle is driven by external shocks amplified by the multi-veto structure, and during spikes the accountability diffusion component becomes partially self-serving (blame-shifting is easiest exactly when costs peak), so the oscillation carries a mild intermittent-reinforcement character without being primarily an extraction device. Base properties reflect the interval-end plateau, not the trough.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the branch seats the arrangement presents as autonomy insurance: each branch experiences the boundary chiefly as protection of its own readings, with costs experienced as the price rivals impose. From the payer seats — policy coalitions, crisis populations, electorate — the identical structure presents as obstruction and fog: demands die in venues never contested, deadlines lapse, and no one is answerable. Within the branch tier the judicial seat diverges further: identity_locked exit plus life tenure make the judicial seat the arrangement's most durable occupant, simultaneously its largest beneficiary (accumulated doctrinal weight) and a distinctive payer (rulings can be ignored without remedy available to the court), so the judicial seat's experienced type shifts with enforcement weather in a way the legislative seat's does not. The excluded seats experience the arrangement as a finished fact that periodically redefines their obligations. The engine computes these divergences from the structural data; this story does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Branch seats carry low directionalities: each is a declared beneficiary whose readings the arrangement shields (d well below 0.5), with the judicial seat pushed modestly upward from the pure-beneficiary pole by identity-locked exit and its exposure to unenforced rulings, and the legislative and executive seats similarly lifted slightly by their own recurring payment of override and rejection costs — all three are dual-positioned beneficiaries/payers. Constitutional minorities sit near the beneficiary pole (multi-venue access subsidizes their claims) with residual ambiguity handled by an omega. Payer seats carry high directionalities: policy-seeking coalitions near-full target (pay through every channel, constrained exit), crisis-exposed populations nearest the full-target pole (powerless, trapped, absorbing timing costs with no strategic recourse), and the general electorate near-symmetric (collects mutual-veto protection, pays accountability fog). Excluded seats are not in the beneficiary/victim derivation but register as governed parties bearing output volatility. Scope is national for nearly all seats, so scope amplification of effective extraction is modest; the analytical seat sits outside the arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing any organ from becoming a self-interested final interpreter — is contested rather than dead: rival readings argue the danger is obsolete under modern legitimacy norms, while the arrangement's own persistence and the recurring success of override and non-acquiescence mechanisms indicate the problem framing remains operative. Because status is contested and the verdict is world_rearranges, the mismatch consumer finds no dead-problem-plus-dependence zombie signature; mandatrophy is not resolved. The classification discipline matters in both directions here. A pure-coordination (rope) labeling would erase the real costs borne below the branch tier; a pure-extraction (snare) labeling — the natural move for a partisan of a rival reading who sees the arrangement as cover for judicial self-aggrandizement — would erase the genuine mutual-protection function every branch seat demonstrably collects. Tangled rope holds both truths: coordination through the same channels that deliver the costs. The forward risk is decay rather than capture: the maintenance-burden series rises monotonically, and if successful corrections cease while ceremonial co-equality rhetoric continues expanding, the arrangement drifts toward inertial performance — the piton-adjacent receipt cell — with the operative constraint quietly becoming the concentrated-finality design this reading denies. The omegas flag exactly that threshold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the coordinate_construction_reading of kernel constitutional_authority_boundary; what structurally changes under the sibling readings judicial_supremacy_reading and parliamentary_primacy_reading, and where exactly does the disagreement sit?',
    'Not resolvable by data within this story: the readings are separate constraints (separate files) linked by network.affects_constraints. Resolution happens at the family layer by comparing each reading''s epsilon, beneficiary/victim structure, and drift state over the shared referent of constitutional practice.',
    'Under judicial_supremacy_reading the arrangement acquires a monopoly beneficiary (the judicial seat) and new victims (overridden legislative and executive seats); under parliamentary_primacy_reading beneficiaries shift to electoral majorities and victims to judicial independence and minorities. If the disagreement is relocated — e.g., a hybrid reading conceding finality in narrow domains — this story''s victim set and epsilon both move.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; disagreement located in whether and where the text assigns final interpretive authority.').

omega_variable(
    intrinsic_vs_degraded_deadlock_costs,
    'Are the measured conflict costs (multi-filter delay, crisis deadlock, accountability diffusion) intrinsic prices of mutual-veto protection, or degradation products that a healthy coordinate equilibrium would not produce?',
    'Compare eras of high cross-branch comity against polarized eras with the institutional design held constant: if costs fall sharply under restored comity, they are degradation products; if a floor persists across regimes, they are intrinsic.',
    'If intrinsic, moderate epsilon is a permanent feature and the tangled_rope classification is stable; if degradation-driven, epsilon falls toward rope territory under restored comity, and the extraction currently attributed to the arrangement belongs instead to the surrounding political pathology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_vs_degraded_deadlock_costs, empirical, 'Whether the arrangement''s extraction floor is structural or circumstantial.').

omega_variable(
    judicial_ascendancy_threshold,
    'Has practice drift toward court-centered finality crossed the threshold at which the coordinate reading no longer describes the operative constraint — leaving this arrangement maintained mainly as ceremonial assertion while the operative constraint converges on the judicial_supremacy sibling?',
    'Track the frequency and success rate of legislative overrides, executive non-acquiescence episodes, and jurisdiction-affecting enactments over coming decades: continued successful corrections keep the reading operative; their cessation marks the crossover.',
    'Past the threshold, this story''s epsilon goes stale — the operative constraint is the sibling''s, this arrangement survives as performance, and the correct classification drifts toward piton with rising theater_ratio; before it, the rising maintenance burden is recoverable enforcement effort.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_ascendancy_threshold, empirical, 'Whether the coordinate arrangement remains operative or has become theatrical maintenance beneath a de facto final arbiter.').

omega_variable(
    minority_multi_venue_net_effect,
    'Does requiring constitutional minorities to win assent across all three branches protect them (multiple independent access points) or exhaust them (losers nowhere, winners must sweep)?',
    'Compare minority policy and rights-claim success rates under coordinate arrangements versus concentrated-finality systems with comparable demographics; measure renewal rates of claims across venues after initial defeat.',
    'If protection dominates, constitutional_minorities stay a beneficiary seat and epsilon is damped; if exhaustion dominates, the group migrates toward the victim column, raising epsilon and pushing the classification toward snare-flavored territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_multi_venue_net_effect, empirical, 'Net sign of the multi-venue filter for politically losing groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1800, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(cons_tr_t1838, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1838, 0.18).
narrative_ontology:measurement(cons_tr_t1866, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1866, 0.22).
narrative_ontology:measurement(cons_tr_t1937, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1937, 0.26).
narrative_ontology:measurement(cons_tr_t1958, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1958, 0.3).
narrative_ontology:measurement(cons_tr_t2001, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 2001, 0.36).
narrative_ontology:measurement(cons_tr_t2026, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(cons_be_t1800, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1800, 0.32).
narrative_ontology:measurement(cons_be_t1838, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1838, 0.35).
narrative_ontology:measurement(cons_be_t1866, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1866, 0.47).
narrative_ontology:measurement(cons_be_t1937, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1937, 0.49).
narrative_ontology:measurement(cons_be_t1958, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1958, 0.41).
narrative_ontology:measurement(cons_be_t2001, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 2001, 0.43).
narrative_ontology:measurement(cons_be_t2026, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 2026, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1800, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(cons_su_t1838, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1838, 0.28).
narrative_ontology:measurement(cons_su_t1866, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1866, 0.38).
narrative_ontology:measurement(cons_su_t1937, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1937, 0.44).
narrative_ontology:measurement(cons_su_t1958, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1958, 0.52).
narrative_ontology:measurement(cons_su_t2001, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement(cons_su_t2026, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'who finally interprets the constitution?' decomposes into three structurally distinct constraints — one per reading of the kernel constitutional_authority_boundary. This story (coordinate_construction_reading, claimed tangled_rope, moderate extraction from conflict costs, no monopoly beneficiary) is upstream in the sense that both siblings define themselves AGAINST its premise: judicial_supremacy_reading concentrates finality in courts (creating a monopoly beneficiary and a new victim set of overridden branches), and parliamentary_primacy_reading relocates finality to the legislature (shifting beneficiaries to electoral majorities and victims to judicial independence and minorities). Each member carries its own stable epsilon over the fixed referent of the standing arrangement it describes; the family edges allow contamination analysis — degradation of the coordinate arrangement (rising theater, failing corrections) feeds legitimacy conditions for both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
