% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Mutual-Vulnerability Deterrence Equilibrium (Total War Reachable but Priced Out of Preference)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the deterrence_equilibrium_reading of the kernel
 *   total_war_possibility_space: total war between great powers remains
 *   strategically reachable - kept alive in planning space by continuous
 *   doctrine development, counterforce targeting, and theorized escalation
 *   ladders - and is restrained by a priced cost-benefit calculation in which
 *   mutual vulnerability makes initiation unacceptably expensive. The
 *   standing arrangement under contest is the material deterrence regime
 *   itself: the arsenals, commands, modernization cycles, and signaling
 *   apparatus that generate continuous investment in war-fighting capability
 *   as the deterrent signal. Epsilon's referent is that arrangement as THIS
 *   reading assesses it - not the prohibition arrangement the disarmament
 *   seats endorse, and not the foreclosed-space the contraction reading
 *   describes. Per the epsilon-invariance principle, the colloquial label
 *   'why no World War III' decomposes into three structurally distinct claims
 *   carried by three linked stories: this file (material cost calculus), the
 *   nuclear_taboo_reading file (constructed normative prohibition), and the
 *   space_contraction_reading file (removal from the strategically
 *   thinkable). Each carries its own epsilon, victim structure, and
 *   classification; they are linked through network.affects_constraints. The
 *   claimed type (tangled_rope) and the authored metrics are independent
 *   facts: the claim asserts a genuine coordination function joined to
 *   asymmetric extraction under active enforcement; the metrics report the
 *   arrangement's observed operating profile without being tuned to any
 *   predicted engine output. Time points in the measurement series are years
 *   elapsed since 1950 (0 = 1950, 12 = Cuban missile crisis era, 33 = Able
 *   Archer / SDI peak, 41 = Soviet dissolution, 66 = post-Crimea
 *   modernization surge, 75 = 2025).
 *
 * KEY AGENTS:
 *   - - great_power_leaderships: Primary beneficiary (institutional/identity_locked) - hold the arsenal that constitutes great-power standing and collect its security yield; simultaneously bear its budgetary and escalation costs
 *   - - strategic_command_establishments: Agenda-setter (institutional/identity_locked) - write the doctrine, run the machinery, certify readiness, and produce the threat assessments that justify force sizing; organizationally fused with the mission
 *   - - defense_industrial_contractors: Concentrated beneficiary (powerful/constrained) - receive the multi-decade procurement stream that continuous deterrent-signal investment generates
 *   - - extended_deterrence_ally_states: Protected beneficiary (institutional/constrained) - collect security under the umbrella while absorbing entrapment risk and hosting burdens
 *   - - defense_budget_taxpayers: Diffuse payer (powerless/trapped) - fund the arrangement annually with no decision rights and no opt-out
 *   - - targeted_civilian_populations: Involuntary risk-bearer (powerless/trapped) - live inside adversary target packages without consent and without exit
 *   - - forward_basing_host_communities: Localized payer (powerless/trapped) - carry the concentrated site-level burdens the broader public disperses
 *   - - disarmament_movements: Excluded voice (organized/constrained) - organize objection from outside the deterrence conversation, answered with theory rather than engagement
 *   - - deterrence_theory_community: Analytical observer (analytical/analytical) - formalizes the escalation ladders this reading predicts will remain live
 *   - - arms_control_verification_regimes: Institutional observer (institutional/analytical) - inspects and measures the arrangement's compliance surface, with access that tracks the crisis cycle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.58).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.6).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Mutual-Vulnerability Deterrence Equilibrium (Total War Reachable but Priced Out of Preference)").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, 'f1b65da5-a136-4fd2-96f2-30a9d6e2789f').
narrative_ontology:cs_kernel_codification('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', distributed).
narrative_ontology:cs_authority_grounding('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', expertise).
narrative_ontology:cs_interpretation_layer_present('f1b65da5-a136-4fd2-96f2-30a9d6e2789f').
narrative_ontology:cs_reading_relation('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', total_war_possibility_space__space_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', foundational, reciprocal_vulnerability_prices_war_out_of_preference).
narrative_ontology:cs_axiom_status(reciprocal_vulnerability_prices_war_out_of_preference, holdable).
narrative_ontology:cs_axiom_grounding('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', reciprocal_vulnerability_prices_war_out_of_preference, empirically_contingent).
narrative_ontology:cs_axiom('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', secondary, credible_signal_requires_continuous_capability_investment).
narrative_ontology:cs_axiom_status(credible_signal_requires_continuous_capability_investment, holdable).
narrative_ontology:cs_axiom_grounding('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', credible_signal_requires_continuous_capability_investment, instrumental).
narrative_ontology:cs_reference_frame('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', rational_deterrence_equilibrium).
narrative_ontology:cs_drift_state('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', contemporary_multipolar_revisionist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f1b65da5-a136-4fd2-96f2-30a9d6e2789f', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, great_power_leaderships).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, strategic_command_establishments).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_contractors).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_ally_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, defense_budget_taxpayers).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, targeted_civilian_populations).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, forward_basing_host_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, great_power_leaderships).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_ally_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Heads of government and senior cabinets of nuclear-armed states. They direct arsenal policy, authorize posture and employment doctrine, and conduct crisis bargaining under the shadow of retaliation. They draw international standing and security yield from possessing the arsenal and pay for it through defense budgets and exposure to escalation they do not fully control. Renouncing the arsenal would mean surrendering a constitutive marker of great-power rank and betting national survival on unverifiable promises.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, great_power_leaderships, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, great_power_leaderships, payer).

% Institutional operators of the deterrent: strategic commands, war-planning staffs, weapons-laboratory complexes. They draft targeting doctrine, run exercises, certify readiness, and produce the threat assessments that justify force sizing. Their budgets, career ladders, and institutional purpose are bound up with continuation of the mission; the organizations have grown into the function they perform, and their personnel cannot relocate their expertise outside it.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_command_establishments, agenda_setter,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, strategic_command_establishments, beneficiary).

% Aerospace, missile, submarine, and electronics firms supplying the modernization cycle. Revenue arrives through multi-decade procurement programs whose continuation depends on threat persistence; engineering staff and production lines are specialized to this demand and costly to redirect to civilian markets.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_contractors, beneficiary,
    powerful, biographical, constrained, global).

% Allied governments sheltered under another power's nuclear umbrella. They receive protection they do not arm themselves, host forward infrastructure and sit inside adversary target sets as a consequence, and trade policy autonomy for the guarantee. Leaving the umbrella means rebuilding a deterrent from nothing or accommodating the adversary on unfavorable terms.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_ally_states, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_ally_states, payer).

% General publics financing the arrangement through annual appropriations. They bear the opportunity cost of every modernization dollar with no direct decision rights and no way to withhold payment; their stake reaches them only as aggregate budget lines and deferred social spending.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, defense_budget_taxpayers, payer,
    powerless, immediate, trapped, national).

% Urban populations on every side whose homes sit in adversary target packages. They consented to nothing, cannot relocate out of the category of the targeted, and absorb the risk side of the exchange while receiving none of its proceeds. Their exposure is renewed automatically with each modernization cycle.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, targeted_civilian_populations, payer,
    powerless, generational, trapped, global).

% Towns hosting missile fields, bomber bases, submarine ports, and missile-defense sites. They carry concentrated local burdens - land seizure, accidents, alert noise, protest disruption - that the wider beneficiary public experiences only diffusely. Moving away forfeits home and livelihood; staying means the burdens continue.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, forward_basing_host_communities, payer,
    powerless, biographical, trapped, local).

% Transnational campaigns and humanitarian-initiative coalitions pressing for prohibition and abolition. They negotiated their own treaty instrument outside the nuclear-armed states' forum, are locked out of the deterrence conversation proper, and are answered with deterrence theory rather than engagement when they intrude on it.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, disarmament_movements, excluded,
    organized, generational, constrained, global).

% Strategic-studies scholars and analysts who formalize escalation ladders, stability criteria, and signaling models. Their frameworks supply the intellectual scaffolding the arrangement reasons with; their careers advance by keeping the planning problem analytically alive, which gives them a stake in the problem remaining open.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, deterrence_theory_community, observer,
    analytical, biographical, analytical, global).

% Treaty bodies and inspection apparatuses that measure force levels and compliance. They depend on the arrangement existing to have anything to verify; their access, staffing, and relevance rise and fall with each turn of the crisis cycle.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_verification_regimes, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_contractors).
narrative_ontology:fixing_cost_class(total_war_possibility_space__deterrence_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains predictable mutual-vulnerability relations among nuclear-armed great powers: each side can price the other's response thresholds with enough confidence that crisis bargaining has a known floor beneath which neither side's calculations permit descent.
% TRANSFER_FUNCTION: Moves fiscal resources from general taxpayers to strategic forces and their industrial suppliers; moves involuntary risk onto civilian populations under targeting and onto host communities near the infrastructure; moves security assurance downward to allied protectorates in exchange for alignment.
% ABSENT_VOICES: Targeted civilian populations were never consulted and have no seat in any forum where the arrangement is reviewed. Future generations inherit waste streams and standing risk with no representation. Disarmament coalitions and the states parties to the prohibition treaty deliberate outside the deterrence room entirely and are treated as naive or hostile when they seek entry.
% DISAPPEARANCE_RATIONALE: If the mutual-vulnerability equilibrium vanished overnight, crisis bargaining would lose its calibrated backstop: alliances built on the umbrella would renegotiate within months, the strategic-industrial economy of commands, laboratories, and contractors would unravel or frantically repurpose, and either accelerated rearmament or unconstrained escalation paths would replace the current priced standoff. Every named seat's situation is organized around the arrangement's continuation.
% FOUNDING_PROBLEM: After 1945 the problem was that industrial-scale total war between great powers had become existentially catastrophic and historically recurrent: two world wars in three decades, culminating in the atomic bombings. The arrangement was built to keep a third such war from occurring by making its initiation carry unacceptable and certain cost.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: declassified Executive Committee recordings and Soviet archival materials show adversary leaderships themselves treating mutual vulnerability as the operative restraint at decision moments; neutral historiography of the world wars and the Hiroshima and Nagasaki assessments attests the founding catastrophe; humanitarian-initiative coalitions and prohibition-treaty states parties attest that the catastrophic potential remains live while disputing the solution. None of these corroborating seats collects from the arrangement.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.58 because the arrangement demonstrably delivers its coordination good - no great-power war among nuclear-armed states across the interval - while simultaneously carrying substantial extraction beyond coordination necessity: warhead counts far exceeding any deterrence requirement at peak, contractor rents embedded in multi-decade procurement, and involuntary risk imposed on non-consenting populations. Suppression (0.60) is structural rather than internalized in this reading: exit from the game is priced as unsafe (unilateral restraint invites exploitation under the reading's own logic), alliance discipline constrains protectorates, and budgetary compulsion leaves payers no withholding option; the internalized-suppression variant - populations and elites who no longer perceive alternatives at all - is conceptually the territory of the taboo and space-contraction sibling readings and is flagged through the kernel omega rather than folded into this scalar. Theater ratio (0.40) reflects a permanently mixed operation: parades, timed exercise tempos, and declaratory signaling are performative, but the underlying capability is real and exercised. Accessibility collapse (0.55) captures how thoroughly the accepted deterrence frame crowds out deep-cut and abolition alternatives in elite deliberation while real alternative tracks (arms-control treaties, the prohibition treaty) remained legally open. Resistance (0.50) records recurrent, organized, occasionally effective opposition that has never dislodged the core. The measurement series runs on ONE shared ten-point grid (every tracked metric authored at every point) and shows a full oscillatory cycle: extraction and suppression rise with tension (1960s, 1980s, post-2014), relax during detente and post-Cold-War contraction, and re-accumulate afterward. The oscillation is partly an extraction mechanism in itself: each crisis resets the baseline upward, so the ratchet advances even across relaxations - the 2000s trough (0.46) never returned to the 1950 level, and the 2020s crest rides on a permanently enlarged floor. The scalars in base_properties represent the standing end-state (final grid point), not the series mean.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the strategic-command seat the arrangement is custodianship: the organization that prevented apocalypse and must never blink. From the taxpayer seat it is a budget line whose opportunity cost is invisible. From the ally-state seat it is simultaneously a shield and a hostage registration - protection purchased with placement inside the adversary's target set. From the targeted-civilian seat it is a wager placed on their lives without their signature. From the excluded disarmament seat the whole structure is an indefensible gamble dressed as prudence. From the analytical seat it is a solvable model whose continued solvability is a career asset. Nothing in the prose adjudicates among these; the engine derives per-seat types from the power, exit, and role data, and the divergence between the command seat's computed experience and the payer seats' computed experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation toward low directionality for the collecting seats: leaderships and commands collect standing, mission, and budget; contractors collect procurement revenue; ally states collect protection - each sits nearer the subsidized end of the scale. Victim declarations drive the payer seats toward high directionality, amplified by exit conditions: taxpayers and host communities are trapped (no withholding, no relocation), and targeted civilians combine trapped exit with global spatial scope, which scales effective extraction further upward since verification of what is done in their name is hardest at that range. The command establishments are deliberately dual-positioned (agenda_setter with beneficiary secondary): administration pulls toward agenda-setting while revenue collection gives them a partial beneficiary position, so their derived directionality should sit between the pure collector and the pure administrator. Ally states carry beneficiary-payer duality for the same reason - the umbrella subsidizes them while their hosting and entrapment costs partially offset it. No directionality overrides are authored: the structural declarations plus exit atoms are sufficient to place each seat, and overriding would substitute authorial assertion for derivable structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two symmetrical mislabels. Reading the arrangement as pure snare erases the demonstrated coordination yield: whatever ultimately caused the eighty-year great-power peace, the material apparatus correlates with it, adversary leaderships behaved as if it bound them, and a pure-cover story would not have survived repeated crisis stress tests. Reading it as pure rope erases the asymmetric extraction: overkill capacity far beyond deterrence requirement, procurement rents, involuntary risk imposition on non-consenting populations, and an enforcement apparatus whose budget justification regenerates itself. The scaffold label fails because nothing in the arrangement anticipates its own obsolescence - deterrence claims permanence, declares no sunset, and its practitioners treat transition talk as irresponsibility. The piton label fails because the function has not atrophied: capability is exercised, doctrine is revised, and modernization is technically substantive even where politically theatrical. The mandatrophy-relevant watch item is the mismatch consumer's flag: this reading authors founding_problem_status = live (the catastrophe potential persists as long as arsenals exist), but if the sibling readings prove correct - if norm or imaginative foreclosure, not material cost, does the restraining work - then the material apparatus persists past its causal necessity, status flips toward dead while world_rearranges still holds, and the zombie flag fires against the modernization budget. The kernel omega carries that contingency explicitly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_binding_mechanism_contest,
    'The kernel total_war_possibility_space admits three readings - this deterrence-equilibrium reading (material cost-benefit restraint), the nuclear_taboo_reading (constructed normative prohibition), and the space_contraction_reading (foreclosure from the strategically thinkable). Which mechanism actually binds, and is this reading''s instantiation therefore the operative constraint?',
    'Process-trace declassified crisis deliberations (Executive Committee recordings, Able Archer archives, recent-crisis communication records): if leaders weigh costs and force postures at decision moments, the equilibrium reading binds; if they reason in terms of transgression and norm violation, the taboo reading binds; if total-war options never enter the option lists at all, the contraction reading binds.',
    'If norm or foreclosure proves to be the binding mechanism, this constraint''s coordination function is misattributed to material calculation, its extractive apparatus loses its warrant, and the arrangement recomputes closer to the sibling readings'' structures - with direct consequences for the modernization budget''s justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_binding_mechanism_contest, conceptual, 'Which of three rival mechanisms restrains total war; records that this story is one reading of the shared kernel.').

omega_variable(
    counterforce_eroding_mutual_vulnerability,
    'Does the growth of counterforce capability - accuracy improvements, yield control, conventional global strike, missile defense - erode the certainty of retaliatory vulnerability on which this reading''s price mechanism depends?',
    'Compare second-strike survivability assessments against counterforce kill-chain maturity across the current modernization cycle; track crisis-model sensitivity analyses as delivery accuracy improves.',
    'If first-strike plausibility rises, the cost calculation softens: the constraint''s stabilizing coordination function degrades while its extractive apparatus persists, pushing the standing arrangement toward enforced extraction without the peace dividend that justified it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_eroding_mutual_vulnerability, empirical, 'Whether technological drift undermines the mutual-vulnerability premise of the equilibrium.').

omega_variable(
    signal_vs_capability_investment_split,
    'What proportion of the continuous strategic investment stream functions as deterrent signal rather than usable war-fighting capability?',
    'Budget and readiness audits separating demonstrative posture (parade systems, announced programs, exercise tempo correlated with political calendars) from operationally exercised capability with validated logistics.',
    'A rising signal share raises theater_ratio and indicates drift toward inertial or theatrical maintenance of the arrangement; a low share strengthens the genuine-coordination half of the hybrid classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(signal_vs_capability_investment_split, empirical, 'Signal-versus-capability split of the investment stream the arrangement generates.').

omega_variable(
    threat_estimate_causation_direction,
    'Does the strategic establishment''s demanded capability track the external threat environment, or does the threat estimate track the establishment''s budgetary and mission requirements?',
    'Compare threat-document language and force-sizing rationales across budget cycles, especially the post-Cold-War contraction and the post-2014 expansion; test whether identified requirements contract when budgets do.',
    'If threat estimates follow budgets, the extraction flowing to the command and contractor seats is capture-driven rather than threat-driven, and the payer seats'' burden reads as captured rent rather than coordination cost - shifting the effective balance decisively toward the extractive pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_estimate_causation_direction, empirical, 'Direction of causation between threat perception and establishment demand.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twps_deterrence_eq_tr_t0, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(twps_deterrence_eq_tr_t12, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(twps_deterrence_eq_tr_t25, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement(twps_deterrence_eq_tr_t33, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 33, 0.46).
narrative_ontology:measurement(twps_deterrence_eq_tr_t41, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 41, 0.34).
narrative_ontology:measurement(twps_deterrence_eq_tr_t50, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(twps_deterrence_eq_tr_t58, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 58, 0.34).
narrative_ontology:measurement(twps_deterrence_eq_tr_t66, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 66, 0.38).
narrative_ontology:measurement(twps_deterrence_eq_tr_t71, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 71, 0.37).
narrative_ontology:measurement(twps_deterrence_eq_tr_t75, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 75, 0.4).

% Extraction over time
narrative_ontology:measurement(twps_deterrence_eq_be_t0, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(twps_deterrence_eq_be_t12, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(twps_deterrence_eq_be_t25, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(twps_deterrence_eq_be_t33, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 33, 0.62).
narrative_ontology:measurement(twps_deterrence_eq_be_t41, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 41, 0.6).
narrative_ontology:measurement(twps_deterrence_eq_be_t50, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 50, 0.46).
narrative_ontology:measurement(twps_deterrence_eq_be_t58, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 58, 0.51).
narrative_ontology:measurement(twps_deterrence_eq_be_t66, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 66, 0.57).
narrative_ontology:measurement(twps_deterrence_eq_be_t71, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 71, 0.56).
narrative_ontology:measurement(twps_deterrence_eq_be_t75, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(twps_deterrence_eq_su_t0, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(twps_deterrence_eq_su_t12, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(twps_deterrence_eq_su_t25, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(twps_deterrence_eq_su_t33, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 33, 0.68).
narrative_ontology:measurement(twps_deterrence_eq_su_t41, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 41, 0.5).
narrative_ontology:measurement(twps_deterrence_eq_su_t50, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(twps_deterrence_eq_su_t58, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 58, 0.46).
narrative_ontology:measurement(twps_deterrence_eq_su_t66, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 66, 0.55).
narrative_ontology:measurement(twps_deterrence_eq_su_t71, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 71, 0.54).
narrative_ontology:measurement(twps_deterrence_eq_su_t75, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 75, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__space_contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the nuclear peace' decomposes under the epsilon-invariance principle into three structurally distinct constraints sharing the kernel total_war_possibility_space. This file carries the deterrence_equilibrium_reading (material cost calculus; beneficiaries include the strategic-industrial complex; epsilon approximately 0.58). The nuclear_taboo_reading file carries the normative-prohibition claim (different victim set - transgressor-side honor and normative capital; different epsilon). The space_contraction_reading file carries the imaginative-foreclosure claim (epsilon turns on what planning institutions can no longer conceive). This reading is structurally upstream of the taboo sibling in one direction - the visible material apparatus is what taboo accounts must explain around - and in logical tension with the contraction sibling, which denies the reachability this reading asserts. Cross-family contamination runs through modernization politics: each sibling story's evidence base is cited in the others' policy arguments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
