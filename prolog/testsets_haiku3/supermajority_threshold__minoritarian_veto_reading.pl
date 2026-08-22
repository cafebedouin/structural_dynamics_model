% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Constitutional Supermajority Amendment Threshold as Minoritarian Veto Lock-In
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The U.S. Constitution's supermajority amendment requirement (Article V:
 *   2/3 of Congress + 3/4 of states) is presented as a safeguard against
 *   hasty constitutional change and protection for constitutional stability.
 *   This reading instantiates the minoritarian_veto framing: the same
 *   threshold functions as an entrenchment mechanism that gives blocking
 *   minorities (geographic, regional, ideological) the power to permanently
 *   block reforms demanded by contemporary majorities, converting historical
 *   privilege into permanent constitutional veto. The threshold's legitimacy
 *   claim rests on protecting consensus formation and constitutional
 *   stability; this reading's structural claim is that the threshold enables
 *   durable minorities to block reforms even when consensus for change exists
 *   and the founding problem (rapid constitutional instability) has long
 *   since been solved. The constraint is CLAIMED as snare (the reading's
 *   verdict) while the authored metrics describe a substantially extractive,
 *   actively enforced lock-in: the engine measures whether the divergence
 *   holds or whether the consensus-safeguard logic is defensible even given
 *   the authored metrics.
 *
 * KEY AGENTS:
 *   - Status quo beneficiaries (powerful, arbitrage exit): material and institutional winners under current constitutional arrangements; benefit from threshold's veto on redistributive, rights-expansive, or power-shifting amendments.
 *   - Entrenched elites (institutional, arbitrage exit): institutional actors whose authority derives from current constitutional allocation; benefit from gatekeeping role preserved by supermajority requirement.
 *   - Contemporary majority coalitions (organized, constrained exit): electoral majorities seeking constitutional reform but blocked by geographic distribution or supermajority barriers; constrained in their ability to translate electoral power into constitutional change.
 *   - Reform constituencies (moderate, identity-locked exit): groups seeking constitutional recognition or protection (voting rights, labor rights, religious freedom, historical remedy); their constitutional subordination is embedded in identity and juridical status; exit means accepting permanent constitutional inferiority.
 *   - Blocking coalition minorities (organized, constrained exit): regional, geographic, or cultural minorities whose position in state legislatures gives them supermajority-blocking power; their veto leverage is entirely derivative of the threshold structure.
 *   - Analytical observer (assessment of whether threshold calibrates to consensus-formation costs vs. pure entrenchment).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.81).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.76).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Constitutional Supermajority Amendment Threshold as Minoritarian Veto Lock-In").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional/political").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '9020b3fe-c66c-4381-aa24-a7c9acf9d16a').
narrative_ontology:cs_kernel_codification('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', formalized).
narrative_ontology:cs_authority_grounding('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', extraction).
narrative_ontology:cs_interpretation_layer_present('9020b3fe-c66c-4381-aa24-a7c9acf9d16a').
narrative_ontology:cs_reading_relation('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', foundational, supermajority_blocks_majoritarian_reforms).
narrative_ontology:cs_axiom_status(supermajority_blocks_majoritarian_reforms, holdable).
narrative_ontology:cs_axiom_grounding('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', supermajority_blocks_majoritarian_reforms, empirically_contingent).
narrative_ontology:cs_axiom('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', foundational, blocking_minorities_constitute_veto_beneficiaries).
narrative_ontology:cs_axiom_status(blocking_minorities_constitute_veto_beneficiaries, holdable).
narrative_ontology:cs_axiom_grounding('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', blocking_minorities_constitute_veto_beneficiaries, empirically_contingent).
narrative_ontology:cs_axiom('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', secondary, entrenchment_delegitimizes_constitutional_stability_claim).
narrative_ontology:cs_axiom_status(entrenchment_delegitimizes_constitutional_stability_claim, holdable).
narrative_ontology:cs_axiom_grounding('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', entrenchment_delegitimizes_constitutional_stability_claim, deontological).
narrative_ontology:cs_reference_frame('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', majoritarian_constitutional_legitimacy).
narrative_ontology:cs_drift_state('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', contemporary_gridlock_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9020b3fe-c66c-4381-aa24-a7c9acf9d16a', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majority_coalitions).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, blocking_coalition_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose material interests, legal immunities, or institutional advantages are locked in by the current constitutional arrangement. They benefit from the supermajority threshold's capacity to block reforms that would redistribute power, resources, or constitutional rights. Examples: corporations sheltered by dormant commerce clause interpretation, holders of wealth protected by weak property-tax provisions, regional power bases entrenched in state government structures. They have the resources to mount blocking coalitions at the amendment level and benefit from maintaining the status quo indefinitely.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    powerful, generational, arbitrage, national).

% Institutional actors—legislatures, courts interpreting constitutional text, political parties controlling amendment-blocking positions—whose authority and discretionary power flow from current constitutional allocations. The supermajority threshold protects their gatekeeping role: amendment cannot occur without their consent or at least their failure to block. They benefit from the structural veto regardless of policy preference, because the veto preserves their institutional position.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, beneficiary,
    institutional, generational, arbitrage, national).

% Electoral majorities that wish to enact constitutional reforms—voting-rights expansion, wealth redistribution, institutional restructuring, rights recognition—but find themselves unable to translate electoral victory into constitutional change. They possess temporary political power (a legislative supermajority or sustained electoral coalition) but lack the super-supermajority required to amend, or face the geographic distribution problem: their majority is insufficiently concentrated in enough states to achieve three-fourths state ratification. They pay in blocked reforms, deferred rights, and postponed necessary changes.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majority_coalitions, payer,
    organized, biographical, constrained, national).

% Groups seeking constitutional recognition or protection—disenfranchised populations seeking voting rights, religious minorities seeking disestablishment protections, workers seeking labor rights, subjects of historical injustice seeking constitutional remedy—whose political demands require constitutional change but cannot command the supermajority threshold. Their identity and historical status are entangled with the particular constitutional subordination: exit looks like accepting permanent constitutional inferiority or leaving the jurisdiction. They bear the extractive cost of permanent constitutional exclusion.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_constituencies, payer,
    moderate, biographical, identity_locked, national).

% Geographic or ideological minorities—often rural, regional, or culturally distinct populations—whose ability to constitute enough state legislatures to block amendment gives them veto power disproportionate to their electoral strength. They may not be the primary beneficiaries but function as enforcement agents: their willingness to block amendments they oppose (whether or not those amendments threaten them) sustains the supermajority threshold's operative veto. Their power derives entirely from the threshold structure; without it, their minority status would translate to minority political influence.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, blocking_coalition_minorities, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, blocking_coalition_minorities, observer).

% Analytical seat: those assessing whether the threshold serves constitutional stability or entrenchment. They examine whether the supermajority requirement calibrates to genuine consensus-formation costs and reversibility burdens, or whether it enables durable minorities to block reforms even when substantial consensus exists for change.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, rational_constitutional_designers, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensure constitutional amendments reflect deep, sustained consensus and protect constitutional stability by raising the bar for amendment; create deliberation time for constitutional change to undergo public scrutiny and social learning.
% TRANSFER_FUNCTION: Transfers the authority to block constitutional reform from any arbitrary group to those who can sustain opposition across multiple electoral cycles and geographic regions, enabling blocking minorities to preserve the constitutional status quo against majoritarian demands for change.
% ABSENT_VOICES: Those whose only participation in the amendment process would be as blocking dissidents (voters and constituencies in non-blocking states, electoral minorities nationwide) are present but structurally unable to affect amendment outcome once a blocking coalition forms in sufficient states. Future generations who will inherit locked-in constitutional constraints are completely absent: they cannot vote or ratify amendments, but they will bear the consequences of today's constitutional lock-in.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold disappeared and amendments required only simple majority (or supermajority of one Congress/generation), the constitutional order would undergo rapid transformation: voting-rights guarantees would be strengthened, wealth-concentration constitutional provisions would be revisited, institutional power redistributions would proceed, and historical constitutional subordinations (e.g., slavery-legacy constitutional provisions, gendered property norms) would be formally remedied. The blocking minorities' veto power would evaporate, and the status quo beneficiaries' constitutional protections would erode.
% FOUNDING_PROBLEM: The Framers in 1787 designed the supermajority requirement to prevent constitutional churn and instability: colonial and state constitutions had been amended and replaced with alarming frequency, and the Framers sought to create a durable, stable supreme law that would not be subject to majoritarian passion of the moment.
% FOUNDING_PROBLEM_CORROBORATION: Federalist Papers 49-51 and historical records of pre-1789 constitutional instability attest the founding problem from the designers' seat. However, 230+ years of U.S. constitutional history demonstrates that the founding problem has been solved: constitutional churn has not recurred, and instability from rapid amendment is not a contemporary threat. Scholars of American constitutionalism (Ackerman, Sunstein) note that the founding problem was real and the supermajority requirement has solved it—but solving a historical problem does not entail that the solution serves a present function. Contemporary constitutional scholars studying gridlock and reform-blocking (Greenhouse, Tucker) attest that the constraint now functions as entrenchment, not stabilization. No independent analyst claims the supermajority requirement is necessary today to solve constitutional instability.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) and RISING over the 50-year interval: as demographic, economic, and ideological polarization increases, the blocking power of geographic minorities (rural, conservative, regional) grows disproportionate to their electoral strength, and the number of reforms blocked by the threshold increases. The founding problem—constitutional instability from rapid swings—has not recurred; the growth in blocked reforms reflects accumulating entropy between majoritarian will and constitutional possibility, not protection against instability. Suppression is high (0.76) because the threshold actively prevents reform advocates from translating political victory into constitutional change; the veto is structural and coercive (blocked amendments are not failed coordination; they are coerced blocking). Theater rises from 0.25 to 0.42: the legitimacy narrative (protecting consensus and stability) persists even as the constraint's operative function drifts toward pure entrenchment. The time series is authored on a single grid: all three metrics appear at t=0,10,20,30,40,50, capturing the rising extractiveness and rising theater ratio over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats and the victim seats compute maximally divergent types from identical structural data: (1) From the entrenched-elite seat: the threshold is a rope—genuine coordination solution to a persistent problem (constitutional stability), benefiting all by reducing amendment chaos. (2) From the reform-constituency seat: the threshold is a snare—extractive veto that blocks necessary reform and locks in historical injustice. The engine computes each seat's perspective from power, time_horizon, and exit_options. Entrenched elites hold institutional power, generational time horizons, and arbitrage exit (they can adapt to any new constitutional arrangement and maintain position); they perceive coordination stability. Reform constituencies hold moderate power, biographical time horizons, and identity-locked exit (their status is constitutionally defined); they perceive extractive blocking. Neither perspective is subjective bias; both flow from structural position. The measured divergence IS the payload the per-seat computation exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Status quo beneficiaries and entrenched elites are the structural beneficiaries: they collect the veto privilege and the preservation of their advantageous constitutional allocation. Their directionality is low (d near 0.0 = full beneficiaries). Contemporary majorities and reform constituencies are the structural victims: they pay in blocked reforms, postponed rights, and constitutional subordination. Their directionality is high (d near 1.0 = full targets). Blocking-coalition minorities occupy a hybrid position: they are neither the primary beneficiaries (they do not collect the extraction; they are enforcement agents) nor the primary victims (blocking amendments that threaten them is not extraction). Their directionality is moderate (d near 0.5). The supermajority structure amplifies the directionality asymmetry: it creates a mechanism where geographic position (state legislative strength) becomes a proxy for constitutional veto power, independent of electoral or population legitimacy. A reform constituency in a highly concentrated geographic region (e.g., voting-rights advocates concentrated in urban centers) faces trapped exit: they cannot exit the jurisdiction without abandoning their political community, and their geographic position makes them unable to constitute a blocking coalition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional instability from rapid swings in fundamental law) was a live, specific crisis the Framers faced: repeated state constitutions with 2-4 year amendment cycles had produced constitutional churn and instability. The 200+ years of U.S. constitutional history post-1789 show the founding problem has been SOLVED: constitutional instability from rapid amendment is not a recurring threat. In that time, only 27 amendments have been ratified (3 of which were repealed, and 22 of which address specific crises or civil rights expansions, not constitutional drift). The threshold has not prevented constitutional instability (it has prevented it the way a locked vault prevents bank robbery—by making amendment nearly impossible). The constraint now operates as pure entrenchment: it blocks reforms that majorities desire and that address genuine problems (voting rights in the 1960s, women's suffrage before 1920, etc.), not reforms that cause destabilization. The mandate has died; the constraint persists. This meets the piton-class definition (function atrophied, constraint maintained theatrically), but the authored metrics indicate substantial ongoing extraction rather than costless theater. The constraint is better classified as a snare sustained by the blocking minorities' continued willingness to enforce the veto, with theater-ratio growth indicating increasing reliance on legitimacy narratives (consensus, stability) to justify blocking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (constitutional instability from rapid amendment) still live, or has it been solved by institutional practice and political norms?',
    'Historical analysis of amendment frequency and constitutional churn before and after 1789, and comparative study of constitutional systems with lower amendment thresholds (Canada, Germany, Australia) to assess whether lower thresholds produce constitutional instability.',
    'If the founding problem is solved, the threshold''s legitimacy rests on entrenchment rather than stability protection, and the constraint should be classified as snare with no coordination function. If the problem recurs (rapid-amendment crisis), the threshold''s role as stabilizer is vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether constitutional instability from rapid amendment remains a threat or has been solved by institutional practice.').

omega_variable(
    consensus_formation_rate_calibration,
    'What is the empirical rate at which genuine constitutional consensus forms across the electorate, and does the supermajority threshold calibrate to that rate, or does it exceed it?',
    'Behavioral analysis of opinion formation on constitutional questions; comparison of time-to-consensus on amendments that were eventually ratified vs. amendments that remain blocked despite sustained supermajority support in public polling.',
    'If consensus typically forms within 10-15 years and the supermajority threshold permits amendment ratification in that timeframe for reforms with sustained support, calibration is achieved. If consensus is blocked for decades despite majority support in public opinion, the threshold exceeds the consensus-formation rate and functions as entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_formation_rate_calibration, empirical, 'Whether the supermajority requirement calibrates to actual consensus-formation dynamics or exceeds them.').

omega_variable(
    blocking_coalition_structural_dynamics,
    'Is the blocking-coalition minority''s veto power essential to its political survival, or would it retain political influence even if amendments required only simple supermajority (55% or 60%) instead of 75%?',
    'Political simulation and comparative-institution analysis: does the blocking-coalition constituency depend on amendment veto to protect its interests, or does it have sufficient state legislative power to block objectionable reforms through statutory means (state-level obstruction, legislative gridlock)?',
    'If the blocking coalition''s power is entirely dependent on the supermajority amendment threshold (veto privilege dissolves at lower supermajority), the threshold is performing the sole function of empowering a minority to block what they cannot defeat legislatively. If the coalition retains blocking power at lower thresholds, the threshold''s elevation to 75% is pure extraction (belt-and-suspenders entrenchment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocking_coalition_structural_dynamics, empirical, 'Whether blocking-minority veto power depends on the 75%-threshold specifically, or whether lower supermajorities would achieve similar protection.').

omega_variable(
    contemporary_majority_composition_fluidity,
    'Are the contemporary majorities blocked by the threshold stable coalitions reflecting deep consensus, or transient electoral coalitions that happen to agree on one amendment?',
    'Coalition-stability analysis: track which majorities form around which amendments and whether they persist across multiple election cycles or dissipate.',
    'If blocked majorities are stable and durable (20+ years of consistent supermajority support for a reform), the blocking is entrenchment of minority preference against consensus. If blocked majorities are transient (appear for one election cycle, dissipate), the blocking may serve a stabilization function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_majority_composition_fluidity, empirical, 'Whether the threshold blocks transient majorities or stable, durable reform coalitions.').

omega_variable(
    identity_lock_in_reform_constituencies,
    'To what extent do reform constituencies'' identities become fused with the very constitutional subordination they seek to remedy, making exit (accepting constitutional inferiority) psychologically untenable rather than merely politically costly?',
    'Qualitative analysis of reform advocacy narratives and comparative study of exit rates from jurisdictions where constitutional remedies are blocked vs. jurisdictions where they are available.',
    'If identity-lock is strong (constitutional remedy is core to self-conception), suppression is higher than the structural measure suggests, and the constraint''s effective extraction is underestimated. If exit is available and affordable, the measured suppression better captures the true picture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_reform_constituencies, empirical, 'Whether identity-lock amplifies the suppressive power of the threshold beyond its structural barriers.').

omega_variable(
    reading_alternative_calibration,
    'Would the adaptive_gradient_reading''s prescriptive solution (evidence-based calibration of the supermajority threshold to consensus-formation rates) prove operationalizable, or would it collapse back into either pure minimization (simple majority, no threshold) or the current 75% lock-in?',
    'Institutional design analysis: what would calibration procedure look like? Who would perform it? How would they measure consensus formation? How would they enforce a revised threshold against entrenched minorities who benefit from current supermajority?',
    'If calibration proves impossible or politicized (consensus-formation data becomes itself contested), the adaptive_gradient reading''s middle path is illusory and the choice collapses to the minoritarian_veto and consensus_safeguard poles. If calibration is achievable, the adaptive_gradient reading offers a coherent alternative to both extremes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_alternative_calibration, conceptual, 'Whether the adaptive_gradient reading''s middle path between snare and safeguard is operationalizable or collapses under implementation.').

omega_variable(
    reading_foreclosure_possibility,
    'Does the minoritarian_veto reading''s core premise logically foreclose the consensus_safeguard reading, or can both readings be held simultaneously in different decision-making contexts?',
    'Logical analysis: is there a framework in which the supermajority threshold can simultaneously (a) protect constitutional stability by raising the bar for amendment, and (b) function as minoritarian veto by blocking reforms with majority support? Or are these logically incompatible premises?',
    'If foreclosure holds, the readings are in genuine logical conflict and one must be abandoned if the other''s premises are accepted. If both can coexist, the conflict is empirical (debate about which function is dominant) rather than logical (debate about which premise is true).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether the minoritarian_veto and consensus_safeguard readings are logically foreclosed (mutually exclusive) or empirically contested (both logically possible, debate is about which is operative).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 30, 0.79).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 50, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 50, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__minoritarian_veto_reading, 0.2).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% The supermajority_threshold kernel (the formal Article V amendment requirement) is contested by three distinct readings that instantiate three structurally different constraints. The minoritarian_veto_reading (this constraint) frames the supermajority as a snare enabling blocking minorities to entrench status quo against majoritarian will. The consensus_safeguard_reading frames it as a rope protecting constitutional stability and deep consensus. The adaptive_gradient_reading frames it as a scaffold requiring evidence-based tuning to consensus-formation rates. All three are readings of the same formal kernel; they differ in their assessment of the kernel's operative function (stability, entrenchment, or calibrated tool) and legitimacy. Each reading instantiates a different constraint with different beneficiaries, victims, and ε values, linked by network edges to show kernel family relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
