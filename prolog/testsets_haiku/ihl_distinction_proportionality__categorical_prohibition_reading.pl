% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Martens Clause Categorical Prohibition of Autonomous Lethal Weapons Systems
 *   domain: international_humanitarian_law / military_ethics / technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the categorical prohibition reading of the
 *   contested IHL distinction/proportionality kernel. The reading claims that
 *   Martens Clause principles of humanity and public conscience prohibit
 *   autonomous lethal weapons systems categorically, regardless of technical
 *   performance. Crossing the threshold of machine-decided killing violates
 *   human dignity per se and is thus unlawful under all circumstances. This
 *   reading directly competes with the human_agency_reading (which grounds
 *   prohibition in the need for irreducible human moral judgment) and the
 *   outcomes_based_reading (which permits autonomous systems if they
 *   demonstrably satisfy distinction/proportionality requirements). All three
 *   readings interpret the same kernel (IHL's foundational commitments to
 *   humanity and proportionality) but reach structurally different constraint
 *   instantiations with different beneficiary/victim structures, different
 *   enforcement mechanisms, and different technical implications. The
 *   categorical prohibition reading produces the highest extractiveness
 *   because it bans an entire technology class regardless of performance,
 *   creating maximum asymmetry between states that have already invested in
 *   autonomous systems and those that have not.
 *
 * KEY AGENTS:
 *   - Anti-militarist civil society: Primary beneficiary, mobilizes around dignity-based principle
 *   - States lacking advanced weapons capability: Beneficiary through equalization, coalition-builder
 *   - States with autonomous weapons programs: Primary target/payer, faces research prohibition
 *   - Military innovation constituencies: Payer, faces career/funding truncation
 *   - ICRC and humanitarian bodies: Agenda-setter, enforces the categorical rule
 *   - Non-signatory military powers: Constrained payer, isolated from signatory network
 *   - Weapons designers and engineers: Trapped payer, faces professional remapping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.82).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.71).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Martens Clause Categorical Prohibition of Autonomous Lethal Weapons Systems").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law / military_ethics / technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '7253feb0-4247-4fff-bcef-bf766b86cb54').
narrative_ontology:cs_kernel_codification('7253feb0-4247-4fff-bcef-bf766b86cb54', formalized).
narrative_ontology:cs_authority_grounding('7253feb0-4247-4fff-bcef-bf766b86cb54', lineage).
narrative_ontology:cs_interpretation_layer_present('7253feb0-4247-4fff-bcef-bf766b86cb54').
narrative_ontology:cs_reading_relation('7253feb0-4247-4fff-bcef-bf766b86cb54', ihl_distinction_proportionality__human_agency_reading, influences).
narrative_ontology:cs_reading_relation('7253feb0-4247-4fff-bcef-bf766b86cb54', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('7253feb0-4247-4fff-bcef-bf766b86cb54', foundational, machine_decided_killing_violates_inherent_dignity).
narrative_ontology:cs_axiom_status(machine_decided_killing_violates_inherent_dignity, holdable).
narrative_ontology:cs_axiom_grounding('7253feb0-4247-4fff-bcef-bf766b86cb54', machine_decided_killing_violates_inherent_dignity, deontological).
narrative_ontology:cs_axiom('7253feb0-4247-4fff-bcef-bf766b86cb54', foundational, martens_clause_applies_comprehensively_to_autonomy).
narrative_ontology:cs_axiom_status(martens_clause_applies_comprehensively_to_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('7253feb0-4247-4fff-bcef-bf766b86cb54', martens_clause_applies_comprehensively_to_autonomy, deontological).
narrative_ontology:cs_reference_frame('7253feb0-4247-4fff-bcef-bf766b86cb54', human_dignity_centered_ihl_framework).
narrative_ontology:cs_drift_state('7253feb0-4247-4fff-bcef-bf766b86cb54', contemporary_military_autonomous_systems_proliferation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7253feb0-4247-4fff-bcef-bf766b86cb54', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_advanced_weapons_capability).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_autonomous_weapons_programs).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_innovation_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, non_signatory_military_powers).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, future_combatants_and_civilians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, non_signatory_military_powers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, future_combatants_and_civilians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, weapons_designers_and_engineers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for binding legal prohibition on autonomous lethal weapons systems, grounding their claim in Martens Clause principles of humanity and public conscience. Believes machine-decided killing is inherently incompatible with human dignity and irreducible human moral agency. Collects legitimacy and operational momentum from this reading's adoption into international law; their mobilization success depends on the ban's formalization and state compliance. Can exit the advocacy role (move to other causes) or adjust positions, but their coalition power is durable across issues.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, mobile, global).

% Benefit from a categorical ban because they lack the technological and financial capacity to develop or acquire autonomous weapons systems. A legal prohibition levels the asymmetry: without it, technologically advanced militaries would dominate asymmetrically. They have leverage through collective voting in UN bodies and can coalition-build with civil society and humanitarian actors. Can exit by defecting to non-signatory status or demanding outcomes-based revision, but that would forfeit coalition power and leave them vulnerable to autonomous weapons proliferation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_advanced_weapons_capability, beneficiary,
    powerful, generational, mobile, global).

% Bear the cost of a categorical prohibition by surrendering investment in autonomous weapons research, forgoing military advantage, and accepting constraint on their technological sovereignty. They face domestic pressure from defense industries and military establishments that view autonomous systems as force-multipliers and existential competitive advantages. Exit from the constraint would require violating binding international law and risking sanctions, isolation, or humanitarian accountability. Their constrained exit reflects the sunk cost of signatory status and reputational damage of defection.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_autonomous_weapons_programs, payer,
    powerful, generational, constrained, global).

% Defense contractors, military research institutions, and operational commanders invested in autonomous weapons development. They pay in forgone research funding, cancelled procurement programs, intellectual property restrictions, and operational doctrine limitations. Their exit options are structurally limited: they cannot simply move the technology to a non-signatory state without risking legal liability and market access. Career paths in autonomous weapons research are truncated; professional identity is remapped away from that specialization.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_innovation_constituencies, payer,
    institutional, biographical, trapped, global).

% International humanitarian law bodies (ICRC, UN mechanisms, regional courts) adjudicate compliance with the categorical ban. They enforce through fact-finding, legal opinions, and accountability mechanisms. Their operational mandate is bounded by the reading's structural claim: if the categorical prohibition is adopted into binding law, these bodies enforce it; if outcomes-based readings prevail, their role becomes technical assessment rather than bright-line enforcement. Their analytical seat permits detachment from enforcement costs.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, humanitarian_verification_bodies, agenda_setter,
    institutional, generational, analytical, global).

% States that refuse to sign or ratify a categorical prohibition treaty pay the cost of international isolation and reputational damage. They also face the benefit of maintaining autonomous weapons research capacity for competitive advantage, but that benefit is constrained by the fact that signatory states' embargo on technology transfer and cooperation limits their access to advanced components and collaboration networks. Constrained exit reflects the difficulty of building autonomous weapons capability in isolation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, non_signatory_military_powers, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, non_signatory_military_powers, beneficiary).

% Benefit from the categorical prohibition because they are protected from autonomous lethal force in future conflicts. They also pay indirectly if the prohibition reduces overall military effectiveness and prolongs conflicts, or if it incentivizes non-state actors to adopt autonomous systems the law does not regulate. Trapped exit reflects their powerlessness to influence the legal regime and their inevitable exposure to whatever constraint regime emerges.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, future_combatants_and_civilians, beneficiary,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, future_combatants_and_civilians, payer).

% Professional practitioners in autonomous systems fields (roboticists, AI researchers employed by defense sectors) bear the career cost of research prohibition. Some exit into civilian AI fields; others face remapped professional identities or employment disruption. The constraint truncates a category of technical work, not merely redirects it. Constrained exit reflects the specialization of their skills and the limited demand for autonomous weapons expertise outside military contexts; identity_locked dimension reflects the degree to which professional identity is fused with autonomous systems research.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, weapons_designers_and_engineers, payer,
    moderate, biographical, constrained, global).

% Would be capable of acquiring or developing autonomous weapons outside the signatory regime but are excluded from direct participation in the governance debate because they are not party to the international legal framework. Their voice — that autonomous systems provide legitimate force multiplication and that outcomes-based assessment is more rigorous than categorical bans — is structurally absent from the formal adjudication, though present in strategy documents and military procurement. Trapped because they cannot jointly negotiate the treaty terms.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, rival_militaries_without_prohibition_signatories, excluded,
    powerful, generational, trapped, global).

% International Court of Justice, International Criminal Court, and regional human rights courts assess whether the categorical prohibition is consistent with state sovereignty, proportionate to the legitimate security interests it addresses, and coherent with existing IHL. They do not set the constraint but interpret and apply it; their analysis feeds back into whether the reading persists or erodes. Analytical seat permits distance from enforcement machinery.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, observer_international_courts, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, humanitarian_verification_bodies).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__categorical_prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared legal commitment that autonomous lethal weapons systems are categorically unlawful under international humanitarian law, coordinating state behavior around a bright-line rule rather than requiring case-by-case assessment of technical performance. Solves the verification problem by removing the need to evaluate autonomous systems' targeting quality — the rule is that no such systems are lawful, period. This eliminates the technological arms-race dynamic where states compete to develop systems just sophisticated enough to evade scrutiny.
% TRANSFER_FUNCTION: Moves the right to conduct autonomous lethal targeting from military innovation constituencies and technologically advanced states to international humanitarian legal authority (ICRC, UN mechanisms, treaty enforcement bodies). The constraint transfers legitimacy and normative authority from technical performance metrics to categorical human dignity principles. It also transfers opportunity cost: research funding that would have gone to autonomous weapons development is either redirected or forfeited.
% ABSENT_VOICES: States with active autonomous weapons programs (notably: the US, Russia, China, Israel, and others) are parties to the debate but their military establishments oppose the categorical reading. More structurally excluded: non-state military actors (insurgent groups, private military companies, autonomous weapons developers outside state control) have no seat at the treaty table but could theoretically adopt the technology outside the legal regime. Also excluded: future combatants and civilians who will experience the consequences of compliance or non-compliance but have no representation in the negotiating process.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition and its enforcement mechanisms disappeared, military R&D budgets would flow into autonomous weapons development globally; military doctrines would be rewritten to incorporate autonomous targeting; arms races in lethal AI would accelerate; and the international legal framework would collapse to outcomes-based assessment or permissiveness. The world does not return to pre-prohibition status quo — it rearranges into a military technology landscape structured around autonomous systems as competitive necessity. Conversely, if the prohibition persists and is enforced, military innovation constituencies are permanently redirected, military doctrines remain constrained to human-in-the-loop frameworks, and the international legal architecture stabilizes around the human dignity boundary.
% FOUNDING_PROBLEM: The deployment of autonomous lethal weapons systems without irreducible human moral judgment at the moment of force application violates the Martens Clause principles of humanity and public conscience, which are foundational to international humanitarian law. The founding problem is framed as an existential threat to the normative basis of IHL itself: if machines can decide who dies, human dignity and moral agency are delegated to algorithms, and the legal order that protects civilians and combatants collapses into technical assessment divorced from conscience.
% FOUNDING_PROBLEM_CORROBORATION: Civil society organizations (International Committee of the Red Cross, Amnesty International, Campaign to Stop Killer Robots) and a coalition of non-aligned states attest the founding problem is live and urgent. Military establishments in technologically advanced states attest the problem is either overstated or resolvable through outcomes-based regulation rather than categorical prohibition. Academic researchers in robotics and AI ethics are divided: some affirm the dignity-based founding problem; others argue autonomous systems can be designed to satisfy IHL's distinction and proportionality requirements and that prohibition forecloses legitimate civilian uses (humanitarian mine-clearance, search-and-rescue). The corroboration from outside the benefiting parties is mixed: humanitarian law experts lean toward prohibition; technologists and military strategists lean toward performance-based regulation.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the categorical prohibition removes an entire technology class from legitimate use, irrespective of technical performance. This is maximally asymmetric: it extracts from military innovation constituencies and technologically advanced states while benefiting those without the capability. Suppression is moderately high (0.71) because enforcement requires inspections, verification regimes, and potentially sanctions against non-compliant states; the constraint cannot persist on voluntary adoption alone — it requires active coercive enforcement. Theater ratio is moderate-low (0.28) because while there is diplomatic theater around compliance, the enforcement machinery (export controls, verification inspections, legal accountability) is substantive. The measurement trajectory shows rising extractiveness and suppression over the 40-unit interval as treaty adoption spreads and enforcement mechanisms mature, while theater ratio rises slowly — suggesting that as the constraint becomes institutionalized, the proportion of performative activity increases relative to function (governments perform compliance more than enforcement becomes harder). Accessibility collapse is high (0.79) because once the categorical rule is adopted, alternatives (performance-based regulation, national opt-outs) collapse for signatory states; non-signatories retain access to autonomous weapons but face economic and diplomatic isolation. Resistance is moderate-high (0.68) because military establishments in technologically advanced states actively resist the ban, though they lack the coalition power to prevent it.
 *
 * PERSPECTIVAL GAP:
 *   The constraint should compute very differently across seats. From the civil society and non-aligned state perspective, the categorical prohibition is protective coordination (preventing an arms race, grounding law in dignity). From the perspective of technologically advanced military establishments, the constraint is extraction (forgoing competitive advantage, research prohibition). From the humanitarian verification bodies' perspective, it is a bright-line rule they enforce. The engine computes these per-seat divergences from the structural data: high power asymmetry, constrained vs. mobile exit options, opposing beneficiary/victim roles, and institutional vs. military time horizons all drive different d values and thus different type classifications across seats. A state with autonomous weapons capability and constrained exit sees this as snare-flavored extraction; a state lacking capability and mobile coalition options sees it as rope-flavored coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (anti-militarist civil society, states without weapons capability) derive low d (beneficiary end): they gain legitimacy, security, and equalization without bearing the cost of research prohibition or international isolation. Victims (states with programs, military innovation constituencies, weapons designers) derive high d (target end): they bear the research prohibition, the opportunity cost of forgone military advantage, the career disruption, and the enforcement costs. The verification bodies sit near symmetric (d ~0.5): they bear enforcement costs but gain institutional authority and mission legitimacy. Non-signatory military powers derive high d (trapped, isolated) despite retaining technical capability because the constraint's persistence depends on their exclusion and isolation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('autonomous lethal weapons violate human dignity per se') is contested: civil society and humanitarian law experts attest it is live and urgent; military establishments and technologists attest it is either overdrawn or resolvable through performance-based regulation. The disappearance verdict ('world_rearranges') indicates the constraint has real organizational salience — its removal would restructure military innovation and international relations. However, the founding_problem_status is 'contested,' not 'live.' This mismatch (contested status + world_rearranges verdict) surfaces the possibility that the constraint persists not because the founding problem is universally accepted as urgent, but because beneficiary coalitions (non-aligned states + civil society) have sufficient power to impose the ban despite opposition from militarily advanced states. This is a classic mandatrophy candidate: the constraint may outlive its original justification if opposition coalitions (military establishments, technologically advanced states) lose institutional leverage while beneficiary constituencies retain it. The measured rising extractiveness (0.65→0.82) and rising suppression (0.55→0.71) over the interval support this reading: the constraint becomes more extractive as it matures and enforcement hardens, suggesting it is shifting from coordination-plus-asymmetry (tangled rope) toward pure-extraction-with-legitimacy-cover (snare-adjacent). An omega variable documents whether this is the trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_as_deontological_bar_vs_empirical_boundary,
    'Is the prohibition on autonomous lethal weapons grounded in a deontological principle (human dignity is inherently violated by machine-decided killing, independent of outcomes) or is it an empirical claim about outcomes (machines cannot reliably achieve distinction/proportionality, so the practical effect of autonomy is indiscriminate harm)?',
    'Examine the reading''s legal arguments and humanitarian advocacy. If the core claim is that machine-decided killing violates dignity per se, the grounding is deontological and the prohibition would remain even if autonomous systems demonstrably outperformed humans. If the core claim is that machines cannot meet IHL standards, the grounding is empirical and outcomes-based evidence could undermine the prohibition.',
    'If deontological, the constraint persists regardless of technological improvement and foreclosure of outcomes-based readings becomes structural. If empirical, strong evidence of autonomous systems'' superior targeting accuracy could shift the reading toward the outcomes_based_reading or trigger constraint reformulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_as_deontological_bar_vs_empirical_boundary, conceptual, 'Deontological vs. empirical grounding of the dignity claim').

omega_variable(
    founder_coalition_stability,
    'Is the categorical prohibition sustained by a stable coalition of non-aligned states and civil society, or is its persistence dependent on technological advantage remaining concentrated in signatory states (i.e., does the coalition dissolve if non-signatory states develop superior autonomous systems)?',
    'Track coalition composition over time; monitor whether non-signatory states'' autonomous weapons capability correlates with defection pressure or treaty opt-out attempts. If the coalition holds despite capability asymmetries reversing, the prohibition has normative stability; if defections accelerate, it is power-dependent.',
    'If power-dependent, the constraint is vulnerable to technological disruption and may collapse into outcomes-based regulation as competitive pressure mounts. If normatively stable, the categorical prohibition persists as an institutional fact independent of military balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_coalition_stability, empirical, 'Whether the prohibition''s stability is normative or power-contingent').

omega_variable(
    enforcement_credibility_and_non_signatory_proliferation,
    'Can enforcement mechanisms prevent meaningful autonomous weapons proliferation to non-signatories, or do export controls and inspection regimes fail against distributed AI development and non-state actors?',
    'Post-treaty monitoring: do non-signatory states acquire or develop autonomous systems at rates that undermine the signatory bloc''s military advantage? Does enforcement prevent technology transfer or does dual-use AI development enable end-runs around controls?',
    'If enforcement fails, the constraint becomes a coordination mechanism among signatories that does not prevent proliferation; it shifts from a global ban to a club good. If successful, it persists as a categorical rule. Failure would likely shift pressure toward outcomes-based readings as a fallback position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_credibility_and_non_signatory_proliferation, empirical, 'Whether enforcement mechanisms prevent meaningful proliferation').

omega_variable(
    civilian_autonomous_systems_and_legal_scope_creep,
    'Does the categorical prohibition apply only to military autonomous lethal weapons systems, or does its logic extend to autonomous systems in civilian law enforcement, border control, and infrastructure protection? If the scope extends, does boundary enforcement become impossible or is the rule stable at military domain only?',
    'Monitor treaty language and case law: does the ICRC or international courts interpret the prohibition narrowly (military lethal only) or broadly (any autonomous system that can cause death)? Do civilian applications of autonomous systems face legal challenges under the same Martens Clause principle?',
    'If scope creeps to civilian applications, the constraint''s extraction increases and resistance intensifies from non-military constituencies. If boundary holds at military-only, the constraint remains domain-specific and may be more stable. Scope creep could trigger coalition dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_autonomous_systems_and_legal_scope_creep, conceptual, 'Whether the categorical prohibition scope-creeps to civilian autonomy').

omega_variable(
    kernel_reading_foreclosure_conditional,
    'Does the categorical prohibition reading foreclose the outcomes_based_reading logically, or do they coexist as live positions held by different state factions? The core question: is there a coherent legal framework that holds both categorical prohibition and outcomes-based legitimacy simultaneously?',
    'Examine treaty interpretation and dispute resolution: if courts or treaty bodies treat outcomes-based arguments as inherently invalid (foreclosed), the readings foreclose. If courts treat outcomes-based arguments as live but applicants must still meet the categorical bar, the readings coexist. If courts permit outcomes-based exceptions, the categorical prohibition erodes.',
    'If forecloses (rare), the categorical reading is structurally secure against outcomes-based pressure. If coexists, the constraint is vulnerable to reinterpretation through cumulative case law. If exceptions emerge, the categorical prohibition devolves into a rebuttable presumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_conditional, conceptual, 'Whether categorical prohibition logically forecloses outcomes-based reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(ihl__tr_t25, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(ihl__tr_t40, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(ihl__be_t25, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(ihl__be_t40, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(ihl__su_t25, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(ihl__su_t40, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__categorical_prohibition_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the contested ihl_distinction_proportionality kernel. All three readings interpret the same foundational IHL commitment to humanity and proportionality but reach different conclusions about the legality of autonomous lethal weapons systems. The categorical_prohibition_reading claims categorical unlawfulness per Martens Clause; the human_agency_reading grounds prohibition in the need for irreducible human moral judgment (a narrower claim that may permit human-supervised autonomy); the outcomes_based_reading permits autonomous systems if they satisfy distinction/proportionality requirements (broadest position). The three readings have different ε values (categorical = highest, human_agency = moderate, outcomes_based = lowest), different beneficiary/victim structures, and compete for institutional adoption. They are linked by kernel identity and by reading_relations in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, institutional, 0.55).
constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
