% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_categorical_prohibition, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   The Martens Clause categorical prohibition reading claims that autonomous
 *   lethal weapons systems (LAWS) are categorically prohibited under
 *   international humanitarian law regardless of technical performance,
 *   because crossing the threshold of machine-decided killing violates human
 *   dignity per se. This reading instantiates one interpretation of a
 *   contested kernel—the scope and meaning of the Martens Clause and IHL
 *   distinction/proportionality principles. This story models the categorical
 *   prohibition reading alone, not the alternative readings
 *   (human-agency-centered framing, outcomes-based framing). The claim is
 *   authored as a mountain (natural law grounded in human dignity
 *   principles), but the structural data reveal high extraction and
 *   suppression, triggering false-summit evaluation (FSM). The beneficiary
 *   structure (anti-militarist civil society, militarily disadvantaged
 *   states) identifies who collects from the constraint, marking this reading
 *   as extractive from the standpoint of military-technologically advanced
 *   states, even though it claims the status of natural law. The constraint's
 *   persistence depends on active suppression of alternative framings and of
 *   military deployment of systems already developed.
 *
 * KEY AGENTS:
 *   - anti_militarist_civil_society — primary political beneficiary, organized globally
 *   - states_without_autonomous_capability — states locked in at current tech level by the prohibition
 *   - military_technological_advantage_holders — institutional payers, trapped exit
 *   - military_technologists — professional payers, career paths blocked
 *   - international_humanitarian_law_bodies — agenda_setter, interprets and enforces
 *   - battlefield_combatants and civilian_populations_in_conflict — powerless beneficiaries protected by the constraint
 *   - outcomes_based and human_agency advocates — structurally excluded, alternatives foreclosed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.92).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.78).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, mountain).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Martens Clause Categorical Prohibition of Autonomous Lethal Weapons Systems").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, 'a3682990-03f1-48f3-abf0-7426433d62ea').
narrative_ontology:cs_kernel_codification('a3682990-03f1-48f3-abf0-7426433d62ea', formalized).
narrative_ontology:cs_authority_grounding('a3682990-03f1-48f3-abf0-7426433d62ea', lineage).
narrative_ontology:cs_interpretation_layer_present('a3682990-03f1-48f3-abf0-7426433d62ea').
narrative_ontology:cs_reading_relation('a3682990-03f1-48f3-abf0-7426433d62ea', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3682990-03f1-48f3-abf0-7426433d62ea', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('a3682990-03f1-48f3-abf0-7426433d62ea', foundational, machine_decided_killing_violates_dignity_per_se).
narrative_ontology:cs_axiom_status(machine_decided_killing_violates_dignity_per_se, holdable).
narrative_ontology:cs_axiom_grounding('a3682990-03f1-48f3-abf0-7426433d62ea', machine_decided_killing_violates_dignity_per_se, deontological).
narrative_ontology:cs_axiom('a3682990-03f1-48f3-abf0-7426433d62ea', foundational, categorical_prohibition_regardless_technical_performance).
narrative_ontology:cs_axiom_status(categorical_prohibition_regardless_technical_performance, holdable).
narrative_ontology:cs_axiom_grounding('a3682990-03f1-48f3-abf0-7426433d62ea', categorical_prohibition_regardless_technical_performance, deontological).
narrative_ontology:cs_reference_frame('a3682990-03f1-48f3-abf0-7426433d62ea', human_moral_agency_in_lethal_judgment).
narrative_ontology:cs_drift_state('a3682990-03f1-48f3-abf0-7426433d62ea', contemporary_autonomous_systems_development, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a3682990-03f1-48f3-abf0-7426433d62ea', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_without_autonomous_capability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, battlefield_combatants).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, civilian_populations_in_conflict).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage_holders).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_technologists).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_inviolability).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, irreducible_human_moral_judgment).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, martens_clause_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the categorical prohibition as a matter of human dignity and the sanctity of life. Collects political and moral legitimacy from the position that machine-decided killing is categorically incompatible with humanitarian principles. Can shift advocacy frames if the principle is rejected; not structurally locked to this reading.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, mobile, global).

% Benefit from a regime that bans a weapons technology they cannot develop or afford. The prohibition locks in their current technological position relative to advanced militaries. A categorical ban protects them from a capability advantage they cannot replicate. Exit would require developing autonomous systems, which is resource-constrained.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_without_autonomous_capability, beneficiary,
    moderate, generational, constrained, global).

% States and military establishments with advanced autonomous systems capability bear the cost of the prohibition: they cannot deploy systems they have invested in developing, cannot compete on a dimension they lead in, and face enforcement pressure to destroy or mothball the technology. Their exit is blocked by the global legal regime; domestic use violates international humanitarian law and treaty obligations.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage_holders, payer,
    institutional, generational, trapped, global).

% Research communities and defense contractors who have built autonomous systems research programs face the constraint as a regulatory regime preventing commercialization, publication of dual-use findings, and continued funding. They can transition to other domains, but the constraint blocks the primary application they engineered for. Career trajectories in autonomous military systems research become professionally untenable.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_technologists, payer,
    powerful, biographical, constrained, global).

% International Committee of the Red Cross (ICRC), UN bodies, and treaty bodies interpret and enforce the constraint. They adjudicate whether specific systems violate the prohibition, coordinate state compliance, and produce guidance documents. They can reinterpret the constraint through doctrinal evolution, but a categorical prohibition leaves narrower interpretive room than principles-based standards.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, international_humanitarian_law_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Individual soldiers and their adversaries benefit from a prohibition that ensures targeting decisions remain subject to human moral judgment and accountability. They cannot refuse the constraint; it is law. The constraint protects them by requiring human decision-makers to account for their status as persons, not targets for algorithmic processing.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, battlefield_combatants, beneficiary,
    powerless, immediate, trapped, local).

% Protected persons under IHL benefit from the constraint insofar as it requires human operators to make distinction and proportionality judgments that might better account for civilian status. They cannot negotiate their protected status; it derives from the law. The constraint's force depends on human judgment about who counts as civilian.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, civilian_populations_in_conflict, beneficiary,
    powerless, immediate, trapped, local).

% States and legal scholars who argue that autonomous systems satisfying distinction and proportionality standards empirically should be permitted are structurally excluded from setting the agenda under this reading. They dispute the foundational premise that machine-decided killing violates human dignity per se. Their position is not represented in the constraint's authority structure.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, outcomes_based_reading_advocates, excluded,
    powerful, generational, mobile, global).

% International humanitarian law scholars and military ethicists who frame the prohibition in terms of irreducible human moral agency (rather than categorical human dignity) are structurally excluded from this reading's authority structure. Their framing would permit different remedies (enhanced human-in-the-loop systems) that this reading forecloses. The excluded advocates can shift readings but not change this one's core premise.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, human_agency_reading_advocates, excluded,
    powerful, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__categorical_prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal legal regime that coordinates states around a shared commitment to preserve human moral agency in lethal force decisions. Solves the prisoners' dilemma of an autonomous weapons arms race by setting a categorical legal boundary rather than a technological performance standard. Creates common ground: all states agree that regardless of military advantage, crossing the human-decision threshold violates the law.
% TRANSFER_FUNCTION: Transfers the technological advantage that would accrue to states with autonomous systems capability to the anti-militarist civil society coalition and to militarily disadvantaged states. Moves research funding and professional legitimacy from autonomous military systems development to alternative applications. Transfers the cost of compliance to military establishments and defense technology sectors in advanced states.
% ABSENT_VOICES: Battlefield combatants and civilian populations are the primary beneficiaries but are not represented in the legal consensus process—treaties are negotiated by diplomats and military strategists, not soldiers or affected civilians. Military technologists and states with autonomous capability are present but voting as rule-takers, not rule-makers. Outcomes-based and human-agency-centered scholars are excluded by the reading's core structure—their alternatives are foreclosed by the categorical framing.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition disappeared overnight, military establishments in advanced states would resume autonomous systems deployment within months, creating a new arms race tier. Non-capable states would face a widening technology gap. The constraint's disappearance would reorganize the battlefield decisional hierarchy—whether human judgment actually returns to lethal targeting or is replaced by algorithmic routing is contested. Anti-militarist civil society would reframe as humanitarian opposition to specific deployments rather than categorical legal prohibition.
% FOUNDING_PROBLEM: The founding problem is twofold: (1) technological trajectory toward lethal autonomous weapons systems that would remove human moral agents from the kill decision, (2) the perceived violation of human dignity inherent in allowing machines to decide who dies. The Martens Clause principle that actions of a belligerent must remain subject to the requirements of humanity and public conscience is claimed as the foundational law preventing this trajectory.
% FOUNDING_PROBLEM_CORROBORATION: The International Committee of the Red Cross and UN bodies formally attest that the founding problem is live and the categorical prohibition is the appropriate legal response (aligned with this reading). Military technologists and outcomes-based scholars attest that the problem is overstated—that empirical distinction/proportionality performance is the real issue, not the categorical removal of human decision. States without autonomous capability generally attest to the founding problem; states with capability dispute whether the problem justifies the categorical approach. No corroboration from outside the contested parties exists; the disagreement is fundamentally about whether the principle applies.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, contested).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, ExtMetricName, E),
    domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.92) because the constraint transfers military technological advantage away from advanced states and toward anti-militarist coalition; it is not extraction in the sense of a parasite extracting from hosts, but rather a redistributive constraint that locks in disadvantage for some actors and advantage for others. Suppression is high (0.78) because the constraint's persistence requires active enforcement against: (1) military establishments' incentive to deploy autonomous systems, (2) technologists' incentive to continue development and publish, (3) states' incentive to defect if military advantage is sufficiently large. Theater is low-moderate (0.22) because the constraint operates with genuine doctrinal force (ICRC interpretation, UN mechanisms), not primarily through performative activity—but some enforcement activity is theatrical maintenance of the boundary (legal review processes that serve to reassert the principle's authority rather than detect novel violations). The measurement series shows stable high extraction and suppression with very modest theatrical growth—the constraint's function hardens rather than shifts toward performance. Accessibility of alternatives collapses at 0.88 because once the principle is accepted, the alternative (machine-decided killing) is legally inaccessible; the collapse is not as complete as a mathematical theorem (0.95+) because the categorical premise itself remains contested. Resistance is 0.71 because military establishments and technologists mount substantial resistance through counter-readings (outcomes-based, human-agency frames) and through deployment pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the anti-militarist civil society and non-capable states' seat, this reads as a natural law grounded in human dignity—a mountain. From the military-technologically advanced state seat and technologist seat, the same constraint reads as an extractive regime that locks in technological disadvantage and blocks professional development. The engine's per-seat computation should reflect this divergence: the beneficiary seats will compute mountain-like stability (high accessibility collapse, low resistance), while the payer seats will compute snare-like structure (high resistance, suppression requirements). The authored claim (mountain) and the metrics (high extraction and suppression) do not reconcile; this is precisely where FSM evaluation should trigger, identifying false-summit candidates where natural-law framing masks extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-militarist civil society and non-capable states are beneficiaries (d near 0.0, low effective extraction, subsidized by the constraint). Military-technologically advanced states and technologists are targets (d near 1.0, high effective extraction, trapped by the constraint). The ICRC and IHL bodies are agenda-setters (d near 0.5, neither substantially harmed nor benefited by the constraint itself, but hold interpretive power). The directionality derivation chain operates as: beneficiary/victim declarations → directionality → effective extraction scaling. Anti-militarist civil society has mobile exit (can shift advocacy frames) so d should be pulled downward from baseline; states without autonomous capability have constrained exit (cannot develop the technology) so d should be held firmly as beneficiary. Military technologists have constrained exit (alternative research domains exist but career paths are blocked) so d should be high. No directionality overrides are needed; the derivation captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing autonomous weapons systems from removing human moral judgment from kill decisions) is structurally live—advanced military development continues, systems exist, and deployment pressure is real. The constraint persists because the anti-militarist coalition and treaty bodies enforce the prohibition, not because the founding problem is resolved. The mandatrophy test asks: does the arrangement persist past the point where its original function is accomplished? Here the founding problem is NOT accomplished; the constraint persists because enforcement holds, not because the problem is solved. This is not mandatrophic (no deadwood function with inertial persistence). The constraint is actively functional—it blocks real military deployment that would otherwise occur. The theater ratio is low because the constraint's authority is substantive (doctrinal, legal, enforced through compliance review) rather than primarily performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_prohibition,
    'Is the categorical prohibition a natural law grounded in human dignity as an inviolable principle, or is it a constructed legal regime whose stability depends on continued enforcement by the anti-militarist coalition?',
    'Test: if states with autonomous capability defected and deployed systems without facing significant enforcement (sanctions, military response, diplomatic isolation), would the prohibition persist? If persistence depends on enforcement, it is constructed; if the principle holds despite defection, it has natural-law status.',
    'If constructed, the constraint reclassifies from mountain to tangled_rope or snare (coordination function for anti-militarist coalition with enforcement against defectors). If natural law, the high extraction and suppression scores indicate false summit (beneficiaries gaming natural-law framing). Either way, the claim/metric gap signals that this reading''s natural-law status is contingent on sustained enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_prohibition, empirical, 'Whether the categorical prohibition is grounded in natural principles or sustained by coalition enforcement.').

omega_variable(
    outcomes_based_alternative_foreclosure,
    'Does the categorical prohibition logically foreclose the outcomes-based reading, or do the two readings represent genuinely coexisting alternatives within the same legal framework?',
    'If the Martens Clause principle and IHL distinction/proportionality rules can accommodate both categorical prohibition and outcomes-based compliance standards, the readings coexist; if accepting outcomes-based reading requires rejecting the human-dignity premise that grounds this reading, they foreclose each other.',
    'If foreclosure: the constraint''s stability depends on suppressing the outcomes-based reading''s legal validity; if coexistence: both readings remain live and the constraint''s enforcement pressure is directed against defection rather than against alternative framings. Coexistence supports the piton hypothesis (performative maintenance of categorical stance against a live alternative); foreclosure supports active extraction regime (suppression of alternative framing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcomes_based_alternative_foreclosure, conceptual, 'Whether the categorical prohibition logically rules out outcomes-based compliance or coexists with it as a live alternative.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.78) primarily structural (active enforcement, legal sanctions, removal of systems) or internalized (military establishments accept the principle as legitimate and self-enforce)?',
    'Observe military establishments that have developed autonomous systems: do they continue development in secret, deploy them despite legal prohibition, or genuinely cease development? If covert continuation or deployment, suppression is structural (requires active enforcement). If voluntary cessation, suppression is partly internalized (the principle has been accepted as legitimate).',
    'If structural suppression, the constraint is actively maintained against resistance; if internalized, the constraint has become normalized and requires less active enforcement—indicating evolution from snare (coerced) to rope (accepted coordination). Internalized suppression would also indicate that the reading''s natural-law framing has achieved legitimacy beyond the beneficiary coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether military suppression of autonomous systems is externally enforced or internalized as legitimate principle.').

omega_variable(
    human_agency_reading_structural_relationship,
    'Does the human_agency_reading (which frames prohibition in terms of irreducible human moral agency) foreclose this categorical_prohibition_reading''s human-dignity premise, or do the two readings coexist as different framings of the same outcome?',
    'Examine whether the human_agency_reading permits human-in-the-loop systems where the machine generates targeting suggestions but humans make final lethal decisions. If human-agency reading permits loopholes that this reading forecloses, they are distinct; if both reach identical legal conclusions, they coexist with different justifications.',
    'If they foreclose each other, then the three readings (categorical, human-agency, outcomes-based) partition the policy space; if they coexist, then human-agency is a sub-principle compatible with either categorical or outcomes-based application. The structural relation affects the constraint''s stability: foreclosure means three-way competition; coexistence means a two-way split between categorical and outcomes-based.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_agency_reading_structural_relationship, conceptual, 'Whether human-agency framing of the prohibition is structurally distinct from or compatible with the categorical human-dignity premise.').

omega_variable(
    militarily_disadvantaged_state_lock_in,
    'Do states without autonomous capability genuinely benefit from the prohibition as a permanent technological lock-in (conferring advantage by freezing relative capability), or do they view it as temporary protection until they develop the capability themselves?',
    'Survey non-capable states'' internal strategic documents and diplomatic statements: do they advocate for permanent prohibition or for development timelines that would permit their own autonomous systems? If they seek permanent prohibition, they are genuinely locked in; if they seek timelines, the prohibition is transitional.',
    'If lock-in is genuinely permanent, non-capable states are beneficiaries of an extractive regime that locks in their disadvantage—the constraint redistributes power by freezing capability differentials. If they view it as transitional, the constraint is a temporary coordination mechanism, and its character changes as development timelines approach. This affects the classification of the constraint and the stability of the beneficiary coalition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militarily_disadvantaged_state_lock_in, empirical, 'Whether non-capable states perceive the prohibition as permanent lock-in or temporary protection.').

omega_variable(
    false_summit_flag_fsl_reading_extraction,
    'This mountain-claimed constraint carries beneficiaries (anti-militarist_civil_society, states_without_autonomous_capability) and scores high on extraction (0.92) and suppression (0.78). Is this a false summit—a constructed constraint disguised as natural law to benefit identifiable parties?',
    'FSM evaluation: mountain claims that declare beneficiaries trigger re-examination. The constraint''s persistence depends on active suppression against military deployment and alternative readings. The beneficiary structure shows clear winners (anti-militarist coalition, non-capable states) and clear losers (military-technologically advanced states, technologists). If the constraint persists only because enforcement holds, and would collapse without enforcement, it is not a natural law. If the natural-law frame serves to delegitimize alternative readings (outcomes-based, human-agency), the frame may be strategic rather than empirical.',
    'FSM reclassification would move the constraint from mountain to tangled_rope (coordination for anti-militarist coalition with extraction from military-technological states) or snare (pure extraction of military advantage, with humanitarian framing as cover). The classification divergence between the claim (mountain) and the metrics (high extraction/suppression/high beneficiary presence) is diagnostic: the gap is where false-summit evaluation should occur.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_flag_fsl_reading_extraction, conceptual, 'False-summit evaluation: is the natural-law framing justified by the principle''s universality or is it strategic framing by a beneficiary coalition?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(ihl__tr_t0, observed).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(ihl__tr_t5, observed).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(ihl__tr_t10, observed).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement_basis(ihl__tr_t15, observed).
narrative_ontology:measurement(ihl__tr_t25, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement_basis(ihl__tr_t25, observed).
narrative_ontology:measurement(ihl__tr_t35, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement_basis(ihl__tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement_basis(ihl__be_t0, observed).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 5, 0.89).
narrative_ontology:measurement_basis(ihl__be_t5, observed).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 10, 0.9).
narrative_ontology:measurement_basis(ihl__be_t10, observed).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 15, 0.91).
narrative_ontology:measurement_basis(ihl__be_t15, observed).
narrative_ontology:measurement(ihl__be_t25, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 25, 0.92).
narrative_ontology:measurement_basis(ihl__be_t25, observed).
narrative_ontology:measurement(ihl__be_t35, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 35, 0.92).
narrative_ontology:measurement_basis(ihl__be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(ihl__su_t0, observed).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement_basis(ihl__su_t5, observed).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(ihl__su_t10, observed).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(ihl__su_t15, observed).
narrative_ontology:measurement(ihl__su_t25, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement_basis(ihl__su_t25, observed).
narrative_ontology:measurement(ihl__su_t35, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 35, 0.78).
narrative_ontology:measurement_basis(ihl__su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__categorical_prohibition_reading, 0.25).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (ihl_distinction_proportionality). The kernel is the Martens Clause principle and IHL distinction/proportionality rules. Three structurally distinct constraints instantiate three readings: (1) categorical_prohibition_reading (this story) claims the threshold of machine-decided killing violates human dignity per se; (2) human_agency_reading claims IHL requires irreducible human moral judgment at the moment of lethal force (permits enhanced human-in-the-loop, not autonomous); (3) outcomes_based_reading claims IHL is satisfied by empirical distinction/proportionality performance (technology-neutral). Each reading has different beneficiaries, different extraction patterns, and different enforcement implications. They share the kernel and the authority structure but diverge on what the law permits. This story captures only the categorical reading; the siblings are separate constraint stories linked by network.affects_constraints in both directions. The ε-invariance principle requires each reading to have its own ε value measured against the standing arrangement under contest (autonomous weapons systems operation), assessed by that reading's lights. The categorical reading measures ε for the standing arrangement as it would apply the categorical principle (high: complete ban, maximum extraction from military-technological states). The outcomes-based reading would measure ε differently (lower: permitting compliant systems, accepting military technological advantage). The human-agency reading measures ε for irreducible-judgment violations, which yields intermediate values (some autonomous systems violate, others do not). Each reading is a separate story because each has a distinct structural relationship to the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
