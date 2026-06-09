% ============================================================================
% CONSTRAINT STORY: war_normalization_autonomous_weapons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_normalization_autonomous_weapons, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_normalization_autonomous_weapons
 *   human_readable: War Normalization Through Autonomous Weapons Systems
 *   domain: political_theology/technology_ethics/international_law
 *
 * SUMMARY:
 *   The normalization of autonomous weapons systems represents a structural
 *   constraint where AI technology lowers the threshold for lethal violence,
 *   automates targeting decisions, and erodes ethical and legal constraints
 *   on the use of force. This constraint operates at the intersection of
 *   military strategy, international law, and political theology. From a
 *   Catholic Social Teaching perspective (as articulated in recent papal
 *   encyclicals on AI and technology), autonomous weapons violate the
 *   principle of human dignity by removing moral agency from life-and-death
 *   decisions and reducing human beings to algorithmic targets. The
 *   constraint exhibits high extractiveness (0.78) because the benefits
 *   (tactical advantage, reduced risk to military personnel, defense
 *   contractor profits) flow to powerful institutional actors while the costs
 *   (civilian casualties, erosion of IHL, loss of moral agency) are borne by
 *   powerless populations. Suppression (0.82) is high because alternatives to
 *   deployment are systematically foreclosed: states face strategic
 *   competition pressure, treaty efforts are blocked by great powers, and the
 *   technological trajectory creates path dependence. Theater ratio (0.65)
 *   reflects that compliance with international humanitarian law is
 *   increasingly performative: states assert 'meaningful human control'
 *   through definitional manipulation while deploying systems that violate
 *   the principle in practice. The constraint's temporal trajectory shows
 *   accelerating extraction, rising theater, and intensifying suppression as
 *   deployment normalizes and legal frameworks atrophy.
 *
 * KEY AGENTS:
 *   - Civilian Populations in Conflict Zones: Primary victim (powerless/trapped) — bear maximum extraction through increased casualty rates, reduced warning time, algorithmic targeting errors, and removal of human hesitation from kill chains
 *   - Military Personnel (Operators/Commanders): Secondary victim (moderate/constrained) — experience erosion of professional judgment, transfer of moral responsibility to algorithms, legal liability gaps, and loss of discretion that previously provided ethical guardrails
 *   - National Military Institutions: Mixed beneficiary-victim (institutional/constrained) — gain tactical advantages while losing institutional autonomy to algorithmic systems and defense contractor lock-in
 *   - Defense Contractors and AI Developers: Primary beneficiary (institutional/arbitrage) — capture revenue from development, maintenance, and upgrades; can exit to commercial markets; experience constraint as pure coordination
 *   - International Humanitarian Law Advocates: Organized victim (organized/constrained) — face systematic violation of IHL principles and suppression of treaty efforts through great power veto
 *   - International Treaty Regime: Institutional actor experiencing atrophy (institutional/arbitrage) — maintains performative compliance theater while functional constraint on state behavior erodes
 *   - Catholic Social Teaching / Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination problem (how to govern AI in warfare) and substantial extraction (technocratic paradigm's reduction of human dignity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_normalization_autonomous_weapons, 0.78).
domain_priors:suppression_score(war_normalization_autonomous_weapons, 0.82).
domain_priors:theater_ratio(war_normalization_autonomous_weapons, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_normalization_autonomous_weapons, extractiveness, 0.78).
narrative_ontology:constraint_metric(war_normalization_autonomous_weapons, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(war_normalization_autonomous_weapons, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_normalization_autonomous_weapons, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(war_normalization_autonomous_weapons, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_normalization_autonomous_weapons, snare).
narrative_ontology:human_readable(war_normalization_autonomous_weapons, "War Normalization Through Autonomous Weapons Systems").
narrative_ontology:topic_domain(war_normalization_autonomous_weapons, "political_theology/technology_ethics/international_law").

domain_priors:requires_active_enforcement(war_normalization_autonomous_weapons).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_normalization_autonomous_weapons, 'd68c822d-43cc-4637-a95c-4c6ca40fc2b1').
narrative_ontology:cs_kernel_codification('d68c822d-43cc-4637-a95c-4c6ca40fc2b1', formalized).
narrative_ontology:cs_authority_grounding('d68c822d-43cc-4637-a95c-4c6ca40fc2b1', lineage).
narrative_ontology:cs_interpretation_layer_present('d68c822d-43cc-4637-a95c-4c6ca40fc2b1').
narrative_ontology:cs_created_at('d68c822d-43cc-4637-a95c-4c6ca40fc2b1', '2025-01-09T00:00:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_normalization_autonomous_weapons, defense_contractors).
narrative_ontology:constraint_beneficiary(war_normalization_autonomous_weapons, military_institutional_hierarchies).
narrative_ontology:constraint_beneficiary(war_normalization_autonomous_weapons, states_with_technological_advantage).
narrative_ontology:constraint_victim(war_normalization_autonomous_weapons, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(war_normalization_autonomous_weapons, international_humanitarian_law_framework).
narrative_ontology:constraint_victim(war_normalization_autonomous_weapons, human_dignity_principle).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_normalization_autonomous_weapons, national_military_institutions).
narrative_ontology:constraint_beneficiary(war_normalization_autonomous_weapons, defense_contractors_ai_developers).
narrative_ontology:constraint_victim(war_normalization_autonomous_weapons, civilian_populations_conflict_zones).
narrative_ontology:constraint_victim(war_normalization_autonomous_weapons, military_operators_commanders).
narrative_ontology:constraint_victim(war_normalization_autonomous_weapons, national_military_institutions).
narrative_ontology:constraint_victim(war_normalization_autonomous_weapons, international_humanitarian_law_advocates).
narrative_ontology:constraint_vindicates(war_normalization_autonomous_weapons, technocratic_paradigm_inevitability).
narrative_ontology:constraint_vindicates(war_normalization_autonomous_weapons, strategic_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trapped in conflict zones with no exit options. Bear the direct costs of autonomous weapons deployment: increased strike frequency due to lowered decision thresholds, reduced warning time due to algorithmic speed, higher casualty rates from targeting errors, and removal of human hesitation that previously provided a margin of safety. Cannot organize resistance or exit the threat environment. The automation makes violence cheaper and faster, which translates directly to increased vulnerability.
narrative_ontology:constraint_stakeholder(war_normalization_autonomous_weapons, civilian_populations_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Constrained by military hierarchy and operational doctrine. Experience erosion of professional judgment as autonomous systems transfer targeting decisions to algorithms. Face legal liability gaps when systems cause civilian casualties but lack authority to override algorithmic recommendations. Cannot exit military service without career destruction. The coordination benefit (reduced risk to own forces) is real but asymmetric: operators become executors of decisions they did not make and cannot fully understand.
narrative_ontology:constraint_stakeholder(war_normalization_autonomous_weapons, military_operators_commanders, payer,
    moderate, biographical, constrained, national).

% Constrained by strategic competition and arms race dynamics. Gain tactical advantages from autonomous systems: speed, scale, reduced personnel risk. But also bear costs: doctrinal lock-in to algorithmic warfare, budget capture by defense contractors, erosion of command authority as systems become black boxes, legal liability for algorithmic failures. Cannot unilaterally abstain from deployment without strategic disadvantage. Dual-positioned: benefit from tactical edge while losing institutional autonomy.
narrative_ontology:constraint_stakeholder(war_normalization_autonomous_weapons, national_military_institutions, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_normalization_autonomous_weapons, national_military_institutions, payer).

% Primary beneficiaries with full exit options. Capture revenue from development contracts, system maintenance, upgrades, and training programs. Can exit to commercial AI markets if defense spending shifts. Experience the constraint as pure coordination: autonomous weapons solve the legitimate problem of maintaining military advantage in strategic competition. The systems work as designed for contractor purposes. Low extraction: the constraint subsidizes this agent.
narrative_ontology:constraint_stakeholder(war_normalization_autonomous_weapons, defense_contractors_ai_developers, beneficiary,
    institutional, immediate, arbitrage, global).

% Organized through NGOs and treaty campaigns but constrained by state sovereignty and enforcement gaps. Bear the cost of systematic IHL violations: autonomous weapons violate distinction, proportionality, and precaution principles through algorithmic speed that makes meaningful human control impossible. Treaty efforts face suppression through great power veto and non-participation. Cannot exit the legal framework being eroded. Resistance is substantial but structurally ineffective against deployment momentum.
narrative_ontology:constraint_stakeholder(war_normalization_autonomous_weapons, international_humanitarian_law_advocates, payer,
    organized, generational, constrained, global).

% The international legal framework nominally sets rules for autonomous weapons but functions largely as theater. CCW Group of Governmental Experts meetings produce non-binding political declarations while deployment accelerates. Geneva Conventions' meaningful human control requirement is honored in rhetoric but violated in practice. The regime persists through institutional inertia and diplomatic performance, not functional constraint on state behavior. Compliance is asserted through definitional manipulation rather than substantive adherence.
narrative_ontology:constraint_stakeholder(war_normalization_autonomous_weapons, international_treaty_regime_ccw_geneva, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Analytical observer position grounded in magisterial teaching authority. Sees both the genuine coordination problem (how to govern AI in warfare to protect human dignity and common good) and the substantial extraction (technocratic paradigm's reduction of human life to algorithmic calculation, erosion of moral agency in lethal decisions). Provides normative framework for evaluating the constraint but lacks enforcement mechanism. The encyclical's 'urgent need' language reveals tension between aspirational principles and structural reality.
narrative_ontology:constraint_stakeholder(war_normalization_autonomous_weapons, catholic_social_teaching_magisterium, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Autonomous weapons systems solve the operational problem of maintaining military advantage in an era of strategic competition where speed, scale, and reduced personnel risk are decisive factors. The systems enable faster targeting cycles, broader surveillance coverage, and reduced exposure of military personnel to direct combat risk.
% TRANSFER_FUNCTION: The arrangement transfers tactical military advantage and defense revenue to states with technological superiority and to defense contractors, while transferring increased vulnerability, casualty risk, and erosion of legal protections to civilian populations in conflict zones. It also transfers moral agency away from human operators to algorithmic systems, creating accountability gaps.
% ABSENT_VOICES: Civilian populations in conflict zones are systematically excluded from autonomous weapons governance decisions. They have no representation in CCW negotiations, no voice in military procurement decisions, and no mechanism to consent to or refuse algorithmic targeting. Their absence from the decision-making process is structural: international law treats warfare as a matter of state sovereignty, and civilians in conflict zones lack state representation. IHL advocates attempt to represent civilian interests but lack enforcement authority.
% DISAPPEARANCE_RATIONALE: If autonomous weapons disappeared overnight, military institutions would need to restructure tactical doctrine, defense contractors would lose revenue streams, states would face altered strategic calculations, and civilian populations would experience reduced strike frequency and restored human judgment in targeting decisions. The arrangements are organized around the technology's existence: procurement budgets, operational doctrine, legal interpretations of 'meaningful control,' and strategic planning all depend on autonomous systems. The world would rearrange itself significantly.
% FOUNDING_PROBLEM: The founding problem was the operational challenge of maintaining military effectiveness in an era of strategic competition where adversaries were developing similar capabilities. Autonomous weapons were presented as necessary to match peer competitors' technological advances, reduce personnel casualties, and achieve decision-speed advantages in contested environments. The problem was framed as: how to maintain tactical superiority when human decision-making becomes the bottleneck in high-speed warfare.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested between different seats. Defense contractors and military institutions assert the problem remains live: strategic competition persists, peer adversaries are deploying autonomous systems, and unilateral restraint creates disadvantage. IHL advocates and CST observers argue the problem was misframed: the real problem is how to govern warfare consistent with human dignity and legal constraints, not how to automate killing more efficiently. The corroboration comes from opposed structural positions: beneficiaries assert liveness to justify continued deployment; critics assert the problem was never the right question. No neutral corroborating source exists outside these opposed interests.
narrative_ontology:disappearance_verdict(war_normalization_autonomous_weapons, world_rearranges).
narrative_ontology:founding_problem_status(war_normalization_autonomous_weapons, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Trapped in conflict zones with no exit options. Bear maximum extraction: autonomous weapons lower the threshold for lethal force, increase strike frequency, and remove human hesitation from kill chains. No agency to resist deployment, no alternatives to vulnerability. The speed advantage that benefits military operators translates to reduced warning time and increased civilian casualties. Theater component: precision strike rhetoric masks indiscriminate effects.
constraint_indexing:constraint_classification(war_normalization_autonomous_weapons, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MILITARY OPERATORS (SNARE) — Constrained by institutional hierarchy and operational doctrine. Experience the constraint as extraction: autonomous systems erode professional judgment, transfer moral responsibility to algorithms, and create legal liability gaps. Cannot exit military service without career destruction. The automation removes human discretion that previously provided ethical guardrails. Some coordination benefit (reduced risk to own forces) but asymmetric extraction dominates: operators become executors of algorithmic decisions they cannot override.
constraint_indexing:constraint_classification(war_normalization_autonomous_weapons, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITARY INSTITUTIONS (TANGLED ROPE) — Constrained by strategic competition and technological arms race dynamics. Experience genuine coordination function: autonomous systems solve real operational problems (speed, scale, reduced personnel risk). But also bear extraction: doctrinal lock-in, budget capture by defense contractors, erosion of command authority, legal liability for algorithmic failures. Cannot exit without strategic disadvantage. Mixed beneficiary-victim: gain tactical advantage while losing institutional autonomy.
constraint_indexing:constraint_classification(war_normalization_autonomous_weapons, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE CONTRACTORS (ROPE) — Primary beneficiaries with arbitrage exit options. Experience the constraint as pure coordination: autonomous weapons solve the legitimate problem of maintaining military advantage in an era of strategic competition. Capture revenue from development contracts, maintenance, upgrades, and training. Can exit to commercial AI markets if defense contracts dry up. Low effective extraction: the constraint subsidizes this agent. Theater component negligible from this perspective: the systems work as designed for the contractor's purposes.
constraint_indexing:constraint_classification(war_normalization_autonomous_weapons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: IHL ADVOCATES (SNARE) — Organized but constrained by state sovereignty and enforcement gaps. Experience high extraction: autonomous weapons systematically violate distinction, proportionality, and precaution principles. The speed of algorithmic decisions makes meaningful human control impossible. Treaty efforts (Campaign to Stop Killer Robots, CCW negotiations) face suppression through great power veto and non-participation. Cannot exit the legal framework being eroded. Resistance is substantial but structurally ineffective against deployment momentum.
constraint_indexing:constraint_classification(war_normalization_autonomous_weapons, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TREATY REGIME (PITON) — The international legal framework for regulating autonomous weapons is largely performative. CCW Group of Governmental Experts meetings produce non-binding political declarations while deployment accelerates. The Geneva Conventions' meaningful human control requirement is honored in rhetoric but violated in practice. The regime persists through institutional inertia and diplomatic theater, not functional constraint on state behavior. High theater ratio: compliance is asserted through definitional manipulation (redefining 'meaningful control' to accommodate automation) rather than substantive adherence.
constraint_indexing:constraint_classification(war_normalization_autonomous_weapons, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: CST ANALYTICAL (TANGLED ROPE) — From the civilizational analytical perspective grounded in Catholic Social Teaching, autonomous weapons present a genuine coordination problem (how to govern AI in warfare to protect human dignity and the common good) layered with substantial extraction (the technocratic paradigm's reduction of human life to algorithmic calculation, the erosion of moral agency in lethal decision-making). The constraint requires active enforcement of ethical principles against deployment momentum. Beneficiaries (defense contractors, militaries) and victims (civilians, human dignity principle) are clearly identifiable. The analytical perspective sees both the coordination need and the extractive reality, making this a tangled rope rather than pure snare or rope.
constraint_indexing:constraint_classification(war_normalization_autonomous_weapons, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_normalization_autonomous_weapons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(war_normalization_autonomous_weapons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(war_normalization_autonomous_weapons, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_normalization_autonomous_weapons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(war_normalization_autonomous_weapons, TR),
    TR >= 0.70.

:- end_tests(war_normalization_autonomous_weapons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. Defense contractors and militaries with technological advantage capture substantial benefits (revenue, tactical superiority, reduced personnel risk) while civilian populations bear severe costs (increased casualties, algorithmic targeting errors, loss of human judgment in kill chains). The extraction is asymmetric and structural: beneficiaries have exit options and agency; victims are trapped. The value reflects that autonomous weapons lower the threshold for violence (making strikes cheaper and faster) while diffusing moral responsibility (creating accountability gaps). Suppression (0.82): High. Alternatives to deployment are systematically foreclosed through multiple mechanisms: strategic competition creates arms race pressure; great power veto blocks treaty efforts; technological path dependence makes reversal costly; defense contractor lobbying suppresses regulatory efforts; classification and secrecy prevent public accountability. Civilian populations have no exit from conflict zones; military personnel face career destruction if they refuse deployment; states face strategic disadvantage if they unilaterally abstain. Theater ratio (0.65): Moderate-high. International humanitarian law compliance is increasingly performative: states assert 'meaningful human control' through definitional manipulation (redefining control to accommodate automation) while deploying systems that violate the principle in practice. CCW negotiations produce non-binding declarations while deployment accelerates. Precision strike rhetoric masks indiscriminate effects. The theater has risen over the interval as the gap between asserted compliance and actual practice has widened. Accessibility collapse (0.35): Low-moderate. Alternatives to autonomous weapons remain conceptually accessible: human-in-the-loop systems, treaty prohibitions, ethical AI governance frameworks, and CST principles all provide alternative pathways. The constraint has not collapsed alternatives completely — resistance is substantial and organized. The low value reflects that the normalization is contested, not inevitable. Resistance (0.72): High. Substantial organized resistance from IHL advocates, arms control NGOs, religious institutions (including Catholic Church), AI ethics researchers, and some military professionals. The Campaign to Stop Killer Robots has mobilized significant opposition. The resistance is structurally ineffective against deployment momentum but remains active and visible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a wide perspectival gap driven by structural position. Defense contractors see pure coordination (rope): autonomous weapons solve legitimate operational problems and generate revenue. Military institutions see mixed coordination-extraction (tangled rope): tactical advantages layered with loss of autonomy and contractor lock-in. Military operators see extraction (snare): erosion of professional judgment and moral agency. Civilian populations see maximum extraction (snare): increased vulnerability with no exit. IHL advocates see extraction (snare): systematic violation of legal principles with suppressed treaty efforts. The treaty regime sees its own atrophy (piton): performative compliance theater maintained through inertia. The analytical observer grounded in CST sees tangled rope: genuine coordination problem (how to govern AI in warfare) layered with substantial extraction (technocratic paradigm's violation of human dignity). The gap reveals that 'strategic necessity' and 'human dignity' are not reconciled but contested — what appears as inevitable technological progress from one perspective appears as extractive normalization from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Civilian populations are victims with trapped exit → high d → high chi (maximum experienced extraction). Military operators are victims with constrained exit → high d → high chi (substantial extraction despite some coordination benefit). Military institutions are mixed beneficiary-victim with constrained exit → moderate d → moderate chi (tangled rope: genuine coordination layered with extraction). Defense contractors are beneficiaries with arbitrage exit → low d → low/negative chi (experience constraint as subsidy). IHL advocates are victims with constrained exit → high d → high chi (organized but structurally ineffective). Treaty regime is institutional with arbitrage exit but experiencing functional atrophy → piton classification derives from theater gate rather than high chi. Analytical observer sees both coordination and extraction → tangled rope classification reflects the mixed structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the classification depends on the observer's structural position and the time horizon examined. From the immediate perspective of defense contractors, autonomous weapons are pure coordination (rope): they solve real operational problems. From the biographical perspective of military operators, they are extraction (snare): erosion of moral agency. From the generational perspective of IHL advocates, they are extraction (snare): systematic violation of legal principles. From the civilizational perspective of CST, they are tangled rope: genuine coordination problem layered with substantial extraction. The mandate (to govern AI in warfare consistent with human dignity and IHL) has not outlived its function — the function was never fulfilled. The constraint is not a degraded coordination mechanism (piton) but an active extraction mechanism (snare from most perspectives) with a coordination cover story. The analytical perspective's tangled rope classification reflects that both the coordination problem and the extraction are real: states face genuine strategic dilemmas, AND the current trajectory violates human dignity principles. The mandatrophy question 'is this coordination or extraction?' is resolved by 'both, and the extraction dominates for most agents.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meaningful_human_control_threshold,
    'What constitutes ''meaningful human control'' over autonomous weapons systems — is human-in-the-loop sufficient, or does meaningful control require human-on-the-loop with veto authority, or must humans make every individual targeting decision?',
    'Empirical analysis of decision timelines in deployed systems; legal scholarship on IHL compliance; case studies of algorithmic failures and human override capacity. CST principle of human dignity requires that humans retain moral agency in lethal decisions, but the technical implementation threshold remains contested.',
    'If human-in-the-loop suffices: many current systems comply, extraction is lower. If human-on-the-loop required: most current systems violate IHL, extraction is higher. If individual decision authority required: all autonomous systems are non-compliant, extraction is maximal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_human_control_threshold, conceptual, 'Threshold for meaningful human control in autonomous weapons').

omega_variable(
    technocratic_paradigm_reversibility,
    'Is the technocratic paradigm''s reduction of warfare to algorithmic optimization reversible through doctrinal development and treaty law, or has the technological trajectory created irreversible path dependence?',
    'Historical analysis of arms control successes and failures; assessment of whether AI governance can follow the chemical/biological weapons prohibition model or whether autonomous weapons are structurally different. CST''s ''civilization of love'' alternative requires that reversal is possible, but the encyclical''s own language (''urgent need,'' ''grave concern'') suggests doubt.',
    'If reversible: scaffold perspective gains validity, sunset logic applies, extraction is temporary. If irreversible: snare perspective is structural, extraction is permanent, CST principles become aspirational rather than actionable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technocratic_paradigm_reversibility, empirical, 'Whether technocratic paradigm trajectory in warfare is reversible').

omega_variable(
    civilian_casualty_attribution,
    'When autonomous weapons cause civilian casualties, is the moral and legal responsibility attributable to the deploying commander, the system developer, the training data curator, the military institution, or the state — and does diffusion of responsibility itself constitute extraction from accountability?',
    'Legal precedent from algorithmic harm cases; IHL scholarship on command responsibility; CST analysis of moral agency and culpability. The responsibility gap is a known problem in AI ethics but lacks resolution in international law.',
    'If responsibility remains with commanders: existing IHL framework applies, extraction is moderate. If responsibility diffuses across multiple actors: accountability gap enables impunity, extraction is severe. If developers bear liability: deployment slows, extraction decreases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_casualty_attribution, conceptual, 'Attribution of moral and legal responsibility for autonomous weapons casualties').

omega_variable(
    strategic_necessity_vs_dignity,
    'When strategic military necessity conflicts with human dignity principles (as in autonomous weapons deployment), does CST provide a resolution mechanism, or does the tension reveal an irreducible omega between security and ethics?',
    'Magisterial teaching on just war doctrine; analysis of whether CST''s principle hierarchy (dignity as foundational) provides clear guidance or whether prudential judgment leaves the question open. The encyclical asserts dignity primacy but does not specify enforcement mechanisms against state security claims.',
    'If dignity is absolute: autonomous weapons are categorically impermissible, CST provides clear constraint. If prudential judgment applies: states retain discretion, CST becomes advisory, extraction continues. The omega''s resolution determines whether CST functions as binding constraint or aspirational ideal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_necessity_vs_dignity, preference, 'Resolution mechanism for strategic necessity vs. human dignity conflicts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_normalization_autonomous_weapons, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_norm_aws_theater_2010, war_normalization_autonomous_weapons, theater_ratio, 0, 0.35).
narrative_ontology:measurement(war_norm_aws_theater_2013, war_normalization_autonomous_weapons, theater_ratio, 3, 0.45).
narrative_ontology:measurement(war_norm_aws_theater_2016, war_normalization_autonomous_weapons, theater_ratio, 6, 0.55).
narrative_ontology:measurement(war_norm_aws_theater_2019, war_normalization_autonomous_weapons, theater_ratio, 9, 0.62).
narrative_ontology:measurement(war_norm_aws_theater_2022, war_normalization_autonomous_weapons, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(war_norm_aws_extract_2010, war_normalization_autonomous_weapons, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(war_norm_aws_extract_2013, war_normalization_autonomous_weapons, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(war_norm_aws_extract_2016, war_normalization_autonomous_weapons, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(war_norm_aws_extract_2019, war_normalization_autonomous_weapons, base_extractiveness, 9, 0.75).
narrative_ontology:measurement(war_norm_aws_extract_2022, war_normalization_autonomous_weapons, base_extractiveness, 12, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(war_norm_aws_suppress_2010, war_normalization_autonomous_weapons, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(war_norm_aws_suppress_2013, war_normalization_autonomous_weapons, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(war_norm_aws_suppress_2016, war_normalization_autonomous_weapons, suppression_requirement, 6, 0.73).
narrative_ontology:measurement(war_norm_aws_suppress_2019, war_normalization_autonomous_weapons, suppression_requirement, 9, 0.79).
narrative_ontology:measurement(war_norm_aws_suppress_2022, war_normalization_autonomous_weapons, suppression_requirement, 12, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_normalization_autonomous_weapons, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of truth_democracy_disinformation (disinformation enables war normalization by degrading public accountability for military decisions) but represents a distinct structural constraint with its own extractiveness value. The upstream constraint's erosion of epistemic commons creates conditions for autonomous weapons deployment to proceed with reduced public scrutiny.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_normalization_autonomous_weapons, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
