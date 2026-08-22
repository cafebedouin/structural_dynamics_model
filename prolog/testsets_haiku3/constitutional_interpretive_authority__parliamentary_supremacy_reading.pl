% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint represents one reading of how constitutional authority is
 *   distributed: the claim that an elected legislature possesses final
 *   interpretive authority over constitutional meaning and that no judicial
 *   body may void parliamentary acts. This is a kernel reading instantiating
 *   one pole of a contested constitutional question present across multiple
 *   democratic jurisdictions. The reading is doctrinally defended by Diceean
 *   legal theory, exemplified in traditional Westminster systems (UK,
 *   Australia, New Zealand), and continues to motivate resistance to
 *   supraconstitutional courts in some polities. The constraint is authored
 *   as a Tangled Rope: it coordinates a governance function (decisive
 *   resolution of constitutional meaning disputes) while extracting from
 *   those subject to parliamentary interpretive latitude without judicial
 *   remedy. The measurement series track the accretion of extractiveness over
 *   four decades as majoritarian legislatures increasingly exploit the
 *   supremacy to override minority constitutional claims, while suppression
 *   rises as alternative interpretive avenues (judicial appeal,
 *   constitutional challenge) are foreclosed.
 *
 * KEY AGENTS:
 *   - Elected legislature (Institutional): The sole authority for constitutional interpretation; benefits from discretionary power; defends supremacy through electoral mandate.
 *   - Judicial branch (Institutional): Stripped of nullification power; constrained to narrow statutory interpretation; bears institutional subordination.
 *   - Constitutional minorities (Powerless-class): No veto point in interpretation; depend on legislative self-restraint or electoral redress; identity-locked by citizenship.
 *   - Majority political faction (Powerful): Commands interpretive supremacy when in legislative control; mobile exit through electoral contestation.
 *   - Individual rights holders (Moderate): Dual-positioned: benefit when majorities respect rights through self-restraint; suffer when majorities legislate against them.
 *   - Extra-parliamentary movements (Organized): Structurally excluded from judicial remedy; confined to electoral mobilization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.71).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'd13630f4-31de-4193-9fdb-aa047ce4c7d6').
narrative_ontology:cs_kernel_codification('d13630f4-31de-4193-9fdb-aa047ce4c7d6', formalized).
narrative_ontology:cs_authority_grounding('d13630f4-31de-4193-9fdb-aa047ce4c7d6', lineage).
narrative_ontology:cs_interpretation_layer_present('d13630f4-31de-4193-9fdb-aa047ce4c7d6').
narrative_ontology:cs_reading_relation('d13630f4-31de-4193-9fdb-aa047ce4c7d6', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d13630f4-31de-4193-9fdb-aa047ce4c7d6', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('d13630f4-31de-4193-9fdb-aa047ce4c7d6', foundational, legislative_supremacy_indivisible).
narrative_ontology:cs_axiom_status(legislative_supremacy_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('d13630f4-31de-4193-9fdb-aa047ce4c7d6', legislative_supremacy_indivisible, deontological).
narrative_ontology:cs_axiom('d13630f4-31de-4193-9fdb-aa047ce4c7d6', foundational, electoral_mandate_constitutional_authority).
narrative_ontology:cs_axiom_status(electoral_mandate_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('d13630f4-31de-4193-9fdb-aa047ce4c7d6', electoral_mandate_constitutional_authority, deontological).
narrative_ontology:cs_reference_frame('d13630f4-31de-4193-9fdb-aa047ce4c7d6', parliamentary_constitutional_supremacy).
narrative_ontology:cs_drift_state('d13630f4-31de-4193-9fdb-aa047ce4c7d6', contemporary_judicial_activism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d13630f4-31de-4193-9fdb-aa047ce4c7d6', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, majority_political_faction).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, individual_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, individual_rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises final interpretive authority over constitutional questions and legislative boundaries. Enacts laws without exposure to judicial nullification. Justifies supremacy through direct electoral accountability: legislatures claim their interpretation carries the mandate of the people in a way unelected courts cannot. Collects discretionary power over constitutional meaning directly.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Stripped of final interpretive authority and power to nullify parliamentary legislation. Can interpret statutes narrowly, but cannot void Acts on constitutional grounds. Bears the institutional cost of subordination to a coordinate branch. Their exit would require constitutional amendment or regime change; constrained to working within the supremacy framework.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_branch, payer,
    institutional, generational, constrained, national).

% Have no veto point in the interpretive process once the legislature acts. Their constitutional claims (minority religious rights, non-majoritarian identity protections, speech claims against majoritarian suppression) depend entirely on legislative self-restraint or electoral vindication. Identity locked by citizenship in a polity whose fundamental law they cannot judicially contest; exit means permanent departure.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minorities, payer,
    powerless, biographical, identity_locked, national).

% Commands the legislature and benefits from interpretive supremacy when aligned with the governing coalition. Can enact constitutional interpretations into law without judicial review. When out of power, they lose the benefit but retain the structural right to supremacy if they regain the legislature; electoral mobility gives them exit to electoral contestation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, majority_political_faction, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from parliamentarism when legislative majorities protect rights through self-restraint or fear of electoral sanction; suffer when majorities legislate against them. Their rights stand on legislative grace, not judicial-enforced constitutional limits. Constrained exit: cannot leave the jurisdiction without extraordinary cost; cannot mount a legal challenge that nullifies the legislation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, individual_rights_holders, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, individual_rights_holders, beneficiary).

% Implements parliamentary legislation and constitutional interpretation as handed down. Has no interpretive voice in the supremacy framework but navigates competing claims from legislature and courts in the interval before the supremacy rule settles disputes. Observes from the standpoint of administrative implementation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, unelected_bureaucracy, observer,
    organized, generational, analytical, national).

% Cannot contest parliamentary interpretation through judicial channels. Their access to constitutional contestation runs entirely through electoral mobilization or revolutionary change. Structurally excluded from the interpretive process; would argue for judicial checks but cannot raise that claim in the forum that would resolve it (constitutional courts are subordinated).
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, extra_parliamentary_political_movements, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__parliamentary_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves constitutional interpretive deadlock by vesting final authority in a single branch (the elected legislature) rather than allowing endless inter-branch contestation. One actor answers what the constitution means; that answer binds all others and ends the dispute.
% TRANSFER_FUNCTION: Transfers the power to define constitutional meaning from a coordinate inter-branch process (or judicial arbitration) to the elected legislature exclusively. Simultaneously transfers authority to override constitutional minority claims from a neutral veto point (the courts) to majoritarian political judgment. Moves the costs of constitutional uncertainty and interpretive contestation from legislatures (who had to seek judicial approval) to courts (who must accept legislative determinations) and to minorities (who lose veto power).
% ABSENT_VOICES: Judicial interpreters (courts), constitutional minorities, advocates of rights-based constitutional floors, and extra-parliamentary movements have no voice in the core interpretive forum under this rule. They can petition the legislature, but the legislature judges its own constitutional claims. Extra-parliamentary actors cannot mount a constitutional challenge through courts because courts lack final authority. The supremacy rule itself forecloses their primary avenue for contestation.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy vanished — if judicial review of parliamentary acts were suddenly restored — constitutional authority would migrate to courts. Constitutional minorities would immediately gain a new veto point and would likely challenge legislation they had accepted under supremacy. Rights protections would shift from legislative self-restraint to judicially-enforced constitutional limits. The entire political equilibrium would reorganize around the new veto point. Legislatures would face constraints they had not faced; courts would face caseloads and political pressure they had escaped.
% FOUNDING_PROBLEM: The founding problem was inter-branch constitutional deadlock: if both legislature and courts claim final interpretive authority, who settles disputes? Parliamentary supremacy solved this by declaring the legislature supreme: it settles its own constitutional bounds, eliminating the deadlock.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary supremacy advocates (legal theorists working in Westminster traditions, contemporary practitioners in UK, Australia, New Zealand) attest the problem is live: judicial supremacy systems face activist courts blocking popular legislation; inter-branch balance requires a tiebreaker. Judicial supremacy and coordinate construction advocates attest the problem is displaced: modern parliamentary systems have evolved procedural solutions (ombudspersons, constitutional conventions, legislative review committees, supermajority requirements for constitutional change) that prevent deadlock without concentrating power in the legislature. Empirical evidence is mixed: inter-branch deadlock occurs in some judicial-review systems but not others; parliamentary systems have experienced abuses of supremacy (majoritarian overreach) in others. No consensus corroboration from outside the competing factions; the dispute is structural to constitutional design. Both traditions can cite stable, functioning democratic systems on their side.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 at interval end, having risen from 0.45 at interval start. This rise models the observed pattern in parliamentary supremacy systems: the coordination function (decisive resolution of constitutional disputes) is genuine at the outset, but over decades, legislatures increasingly exercise interpretive discretion to override minority rights without judicial check. The majority faction benefits directly from this discretion. Suppression is high (0.71) because the veto points for contesting parliamentary constitutional interpretation are severely constrained: minorities cannot mount a judicial challenge, cannot appeal to a constitutional court, and are confined to electoral remedies against legislative majorities. Accessibility collapse is high (0.79) because once the supremacy rule is internalized, the alternatives to legislative interpretation vanish institutionally — courts cannot offer remedy, constitutional challenge is foreclosed. Theater ratio is moderate (0.42) because parliament maintains rhetorics of constitutional fidelity and self-restraint, but a growing share of interpretive activity (shown in the grid's rising stakes_inflation for individual and class levels) is protecting majoritarian policy rather than coordinating shared constitutional understanding. The coercion grid shows rising stakes_inflation at the individual level (0.38 to 0.48) as minorities face increasing costs for acting on constitutional claims; rising accessibility_collapse as alternatives foreclose; and remarkably flat resistance at the structural and organizational levels (systems defending the supremacy rule are stable), contrasting with declining individual resistance (minorities' unvoiced acceptance that judicial remedy is unavailable).
 *
 * PERSPECTIVAL GAP:
 *   The legislature's seat and the constitutional minority's seat should diverge sharply in their experienced types. From the legislative seat (institutional power, arbitrary exit options, national scope), the constraint appears as a Rope — a coordination mechanism that solves the genuine problem of constitutional deadlock while allowing self-interested legislatures to interpret their own bounds. From the constitutional minority's seat (powerless, identity-locked, confined to electoral remedy), the same structure is experienced as a Snare — extractive coercion with no veto point and no meaningful exit. The engine should compute this divergence from the structural data (beneficiary vs. victim, power differential, exit options) without author tuning. The judicial branch's seat is intermediate: it perceives Snare (coerced subordination), yet its institutional nature and generational time horizon grant it more resilience than individual minorities face.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature enters the beneficiary set because it collects interpretive discretion — the power to define constitutional meaning without exposure to nullification. It bears no cost from the supremacy rule; it only benefits. The majority political faction collects this benefit when it holds the legislature; when out of power, it retains the right to supremacy if it regains legislative control (mobile exit to electoral contestation). The judicial branch is a clear payer: it loses interpretive authority and is constrained to narrow statutory reading; its exit options are extremely constrained — constitutional amendment or regime change are the only paths to restoration of judicial review. Constitutional minorities are victims: they lose veto power over majoritarian constitutional interpretation; they are identity-locked (cannot leave the polity without extraordinary cost); they are powerless (no independent electoral or institutional base). Individual rights holders are dual-positioned: they benefit when legislatures protect rights through self-restraint but suffer when majorities legislate against them; their directionality depends on which majority controls the legislature and what that majority's constitutional reading permits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving constitutional deadlock through decisive supremacy) is declared 'contested' in status and 'world_rearranges' in disappearance verdict. This pairing creates a mandatrophy alert: the founding problem is no longer universally acknowledged as a genuine coordination gap. If the constraint disappeared — if judicial review were suddenly restored — the world would rearrange (majorities would lose unilateral interpretive authority; minorities would gain a veto; rights protections would shift from legislative grace to constitutional floors). This rearrangement suggests the constraint's function has shifted: it is no longer primarily solving inter-branch deadlock (arguably a solved problem in modern parliamentary procedure) but rather concentrating interpretive power in the majority for its own benefit. The measured rise in extractiveness (0.45 to 0.68) over the interval substantiates this drift: the coordination function is stable or declining, while the extraction function is rising. Theater ratio climbing (0.25 to 0.42) suggests that parliamentary constitutional discourse is increasingly performative — defending supremacy through rhetoric of judicial activism and tyranny of the bench, rather than engaging genuine coordination gaps. A true Rope (pure coordination) should show stable metrics; the Tangled Rope classification is correct: there is genuine coordination here, but it is increasingly weaponized for asymmetric extraction from powerless minorities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_empirical_vs_normative,
    'Does the legitimacy of parliamentary interpretive supremacy rest empirically on electoral accountability producing better constitutional outcomes, or normatively on the claim that democracy requires it regardless of outcomes?',
    'Comparative constitutional law evidence: (a) Do parliamentary supremacy systems demonstrably protect constitutional minorities better or worse than judicial-review systems? (b) Do legislators cite electoral accountability as justification, or do they invoke inherent democratic authority? (c) How do post-election behavior and rights protection track?',
    'If empirically grounded, the constraint''s persistence depends on showing that electoral remedy actually protects minorities; if normatively grounded, outcomes are irrelevant to legitimacy, and the constraint persists even when majorities systematically override minorities. This determines whether extractiveness is stable or accumulating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_empirical_vs_normative, conceptual, 'Whether parliamentary supremacy legitimacy is consequentialist (electoral accountability works) or deontological (democracy requires it regardless of outcomes).').

omega_variable(
    structural_necessity_of_supremacy,
    'Is parliamentary supremacy structurally necessary to solve a genuine coordination problem (inter-branch deadlock, governance paralysis), or is it a mechanism for concentrating power that could be replaced by alternative dispute-resolution structures?',
    'Natural experiments: jurisdictions that adopted judicial review or moved from supremacy to coordinate construction; examination of whether constitutional deadlock is empirically higher in supremacy systems vs. judicial-review systems; analysis of whether alternative mechanisms (supermajority requirements, constitutional conventions, citizen juries) can resolve interpretive disputes without single-branch supremacy.',
    'If replacement mechanisms exist and prevent deadlock without concentrating power, the supremacy rule is pure extraction riding a solved coordination problem — reclassification to Snare. If alternative mechanisms fail and deadlock results, supremacy remains Tangled Rope. This omega addresses the most contestable claim: whether the founding problem (constitutional deadlock) is actually live in modern parliaments or has been displaced by procedural evolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_necessity_of_supremacy, empirical, 'Whether parliamentary supremacy is structurally indispensable or a contingent choice among viable alternatives.').

omega_variable(
    electoral_mandate_as_constitutional_constraint,
    'Does an electoral mandate to govern (a legislative majority) constitute an implicit mandate to redefine constitutional meaning, or is the mandate limited to policy within existing constitutional boundaries?',
    'Historical analysis of legislative campaign rhetoric and platforms: do electoral platforms explicitly contest constitutional interpretation, or do they campaign on policy? Analysis of whether legislators treat constitutional meaning as mandated by election results or as bound by prior constitutional settlement. Comparative study of how judicial supremacy systems vs. parliamentary supremacy systems frame the electoral mandate.',
    'If the mandate is limited to policy, then parliamentary exercise of interpretive supremacy is extra-mandated and extractive (legislatures are overreaching the democratic instruction they received). If the mandate extends to constitutional meaning, then supremacy has stronger democratic grounding and is less extractive. This determines whether the majority_political_faction truly benefits from the supremacy or whether the benefit is primarily institutional (the office, not the electorate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_mandate_as_constitutional_constraint, conceptual, 'Whether electoral mandates authorise constitutional reinterpretation or are confined to policy governance within fixed constitutional boundaries.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of constitutional minority claims structural (law forecloses judicial remedy, institutional design prevents appeal) or internalized (minorities have internalized legislative supremacy as legitimate and do not mobilize for constitutional change)?',
    'Post-regime-change evidence: if judicial review is suddenly restored, do suppressed minorities immediately mobilize legal challenges, or do they continue accepting supremacy (sign of internalization)? Comparative analysis of how quickly minority rights movements escalate from legislative petition to constitutional demands when new veto points open. Psychological/sociological evidence of identity-lock and normative acceptance.',
    'If structural, the suppression will dissipate with institutional change; minorities will mount immediate legal challenges if courts regain authority. If internalized, suppression persists even after the mechanism is removed — minorities have incorporated parliamentary supremacy into their self-conception and may not perceive alternatives as available. Internalization would indicate the constraint''s suppressive force is deeper and more durable than the measured suppression_requirement (0.71) suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the measured suppression reflects institutional barriers (structural) or absorbed legitimacy (internalized).').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the parliamentary supremacy reading logically foreclose the judicial supremacy reading within a single constitutional framework, or can both be held (e.g., parliamentary supremacy for ordinary constitutional questions, judicial supremacy for fundamental rights)?',
    'Logical analysis: is there a way to hold that legislatures are supreme on most questions but courts are supreme on a limited set of entrenched rights? If yes, the readings coexist (not foreclosed). If no (supremacy is indivisible), then one reading forecloses the other. Examination of hybrid systems (e.g., constitutional courts with limited veto power, legislative override of court decisions with supermajority requirements).',
    'If the readings foreclose each other, the kernel cannot stably support both; the system must settle on one. If they coexist, the system might stabilize with partial supremacy or in a state of contestation. This determines the long-term stability of the parliamentary supremacy constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Whether parliamentary and judicial supremacy readings foreclose each other or can coexist in hybrid forms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cons_tr_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(cons_tr_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(cons_tr_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cons_be_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(cons_be_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cons_be_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cons_su_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(cons_su_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(cons_su_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 40, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(cons_grid_01, constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(cons_grid_02, constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse(class), 40, 0.73).
narrative_ontology:measurement(cons_grid_03, constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(cons_grid_04, constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse(individual), 40, 0.62).
narrative_ontology:measurement(cons_grid_05, constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(cons_grid_06, constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse(organizational), 40, 0.77).
narrative_ontology:measurement(cons_grid_07, constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse(structural), 0, 0.75).
narrative_ontology:measurement(cons_grid_08, constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse(structural), 40, 0.79).
narrative_ontology:measurement(cons_grid_09, constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance(class), 0, 0.65).
narrative_ontology:measurement(cons_grid_10, constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance(class), 40, 0.64).
narrative_ontology:measurement(cons_grid_11, constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance(individual), 0, 0.48).
narrative_ontology:measurement(cons_grid_12, constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance(individual), 40, 0.46).
narrative_ontology:measurement(cons_grid_13, constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance(organizational), 0, 0.6).
narrative_ontology:measurement(cons_grid_14, constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance(organizational), 40, 0.62).
narrative_ontology:measurement(cons_grid_15, constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance(structural), 0, 0.55).
narrative_ontology:measurement(cons_grid_16, constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance(structural), 40, 0.58).
narrative_ontology:measurement(cons_grid_17, constitutional_interpretive_authority__parliamentary_supremacy_reading, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(cons_grid_18, constitutional_interpretive_authority__parliamentary_supremacy_reading, stakes_inflation(class), 40, 0.59).
narrative_ontology:measurement(cons_grid_19, constitutional_interpretive_authority__parliamentary_supremacy_reading, stakes_inflation(individual), 0, 0.38).
narrative_ontology:measurement(cons_grid_20, constitutional_interpretive_authority__parliamentary_supremacy_reading, stakes_inflation(individual), 40, 0.48).
narrative_ontology:measurement(cons_grid_21, constitutional_interpretive_authority__parliamentary_supremacy_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(cons_grid_22, constitutional_interpretive_authority__parliamentary_supremacy_reading, stakes_inflation(organizational), 40, 0.64).
narrative_ontology:measurement(cons_grid_23, constitutional_interpretive_authority__parliamentary_supremacy_reading, stakes_inflation(structural), 0, 0.62).
narrative_ontology:measurement(cons_grid_24, constitutional_interpretive_authority__parliamentary_supremacy_reading, stakes_inflation(structural), 40, 0.68).
narrative_ontology:measurement(cons_grid_25, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(cons_grid_26, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression(class), 40, 0.64).
narrative_ontology:measurement(cons_grid_27, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression(individual), 0, 0.42).
narrative_ontology:measurement(cons_grid_28, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression(individual), 40, 0.52).
narrative_ontology:measurement(cons_grid_29, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression(organizational), 0, 0.65).
narrative_ontology:measurement(cons_grid_30, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression(organizational), 40, 0.7).
narrative_ontology:measurement(cons_grid_31, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression(structural), 0, 0.68).
narrative_ontology:measurement(cons_grid_32, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression(structural), 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel constitutional_interpretive_authority. Three constraint stories decompose the kernel: parliamentary_supremacy_reading (this file), judicial_supremacy_reading (alternate), and coordinate_construction_reading (alternate). Each reading instantiates a different authority distribution, different beneficiary/victim structure, and different ε value. The three stories share the kernel but differ in core premise (source of constitutional legitimacy), axioms (foundational normative claims), and reading_relations (how this reading relates to siblings). All three stories are linked via network.affects_constraints to enable kernel-level analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
