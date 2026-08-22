% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Sovereignty — Categorical Non-Interference Rule (Westphalian Kernel, Absolute Reading)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The categorical non-interference rule: every state holds final and
 *   unconditional authority over its domestic affairs, and external
 *   interference — armed intervention, coercive conditionality, external
 *   judgment of a regime's internal legitimacy — is illegitimate per se,
 *   regardless of the character of the domestic regime. The rule is codified
 *   in UN Charter Article 2(7), the Montevideo Convention's
 *   juridical-equality articles, and customary international law, and is
 *   maintained through diplomatic recognition practice, Security Council veto
 *   politics, and the state-only membership of international institutions.
 *   This story instantiates one reading of the westphalian_sovereignty kernel
 *   (see kernel_context and the kernel_reading omega); the ε authored here is
 *   a property of this reading's standing arrangement — a categorical shield
 *   whose protection accrues to state leaderships and whose costs accrue to
 *   domestic populations — assessed by this reading's own lights and not
 *   hedged across readings. The claim/metric gap is deliberate where present:
 *   the rule is CLAIMED by its defenders as the cornerstone of peaceful
 *   order, while the authored metrics describe substantially extractive,
 *   actively enforced operation; the engine measures that divergence. KEY
 *   AGENTS (by structural relationship): authoritarian regime leaderships
 *   (institutional/arbitrage) — principal collectors of the non-interference
 *   shield; great power governments (institutional/arbitrage) — shaped the
 *   codification, invoke the rule selectively; democratic state governments
 *   (institutional/mobile) — collect the shield but pay when it blocks
 *   response to atrocities; populations under repressive regimes
 *   (powerless/trapped) — primary targets, bear what the rule removes from
 *   external reach; persecuted minority groups (powerless/trapped) — targets
 *   with no recourse by right; humanitarian intervention advocates
 *   (organized/constrained) — the excluded voice; the UN Security Council
 *   (institutional/constrained) — agenda-setter for the rule's exception;
 *   international law scholars (analytical) — observers.
 *
 * KEY AGENTS:
 *   - authoritarian_regime_leaderships: Primary beneficiary (institutional/arbitrage) — principal collectors of the non-interference shield; defend the categorical form as existential
 *   - great_power_governments: Agenda-setter and secondary beneficiary (institutional/arbitrage) — shaped the 1945 codification; invoke the rule selectively, shield own conduct, breach when projecting power
 *   - democratic_state_governments: Mixed beneficiary (institutional/mobile) — collect the shield for their own domestic affairs; pay when it blocks response to atrocities abroad
 *   - populations_under_repressive_regimes: Primary target (powerless/trapped) — bear the domestic conduct the rule removes from external reach; no seat in the system
 *   - persecuted_minority_groups: Target (powerless/trapped) — face repression with no external recourse by right; flight is the only exit
 *   - humanitarian_intervention_advocates: Excluded voice (organized/constrained) — the categorical rule defines their project as illegitimate per se; no decision rights
 *   - un_security_council: Agenda-setter for the exception (institutional/constrained) — administers Chapter VII; P5 vetoes determine when the categorical rule yields
 *   - international_law_scholars: Analytical observer (analytical/analytical) — document the gap between the categorical claim and state practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.58).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.7).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Sovereignty — Categorical Non-Interference Rule (Westphalian Kernel, Absolute Reading)").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6').
narrative_ontology:cs_kernel_codification('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', formalized).
narrative_ontology:cs_authority_grounding('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', lineage).
narrative_ontology:cs_interpretation_layer_present('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6').
narrative_ontology:cs_reading_relation('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', westphalian_sovereignty__conditional_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', westphalian_sovereignty__graduated_sovereignty, forecloses).
narrative_ontology:cs_axiom('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', foundational, domestic_jurisdiction_categorical).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_categorical, holdable).
narrative_ontology:cs_axiom_grounding('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', domestic_jurisdiction_categorical, conventional).
narrative_ontology:cs_axiom('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', foundational, juridical_equality_of_states).
narrative_ontology:cs_axiom_status(juridical_equality_of_states, holdable).
narrative_ontology:cs_axiom_grounding('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', juridical_equality_of_states, conventional).
narrative_ontology:cs_reference_frame('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', westphalian_exclusive_jurisdiction).
narrative_ontology:cs_drift_state('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', contemporary_post_r2p_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c5b63dcb-e48f-40ee-9b5e-fe3830ebdaa6', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regime_leaderships).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, great_power_governments).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, democratic_state_governments).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, populations_under_repressive_regimes).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, persecuted_minority_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, democratic_state_governments).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, non_intervention_principle).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, juridical_equality_of_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, domestic_jurisdiction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold unrestricted domestic authority and invoke the categorical rule against every form of external criticism, sanction proposal, or intervention discussion. The rule's protection is worth most to leaderships with the most to conceal, and they defend its categorical form in UN forums, assemble coalitions of fellow invokers, and treat any erosion toward exceptions as an existential threat. They do not seek exit from the rule; their exposure is limited to reputational campaigns they can outlast.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regime_leaderships, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, authoritarian_regime_leaderships, agenda_setter).

% Shaped the rule's modern codification and hold permanent vetoes over its principal exception. They invoke non-interference to protect their own conduct — covert operations, security practices, treatment of territories — and set it aside when projecting power abroad. Each invocation is selective; the rule protects them at home while their capacity lets them breach it abroad at limited cost to themselves.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, great_power_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, great_power_governments, beneficiary).

% Collect the same protection for their own domestic arrangements and rely on the rule for stable diplomatic and commercial relations, but pay when it blocks response to atrocities abroad and when rivals invoke it against legitimate criticism. They can defect at cost — unilateral intervention, sanctions coalitions, advocacy for exception-bearing reform — which keeps their position inside the rule's maintenance rather than against it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, democratic_state_governments, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, democratic_state_governments, payer).

% Live under governments whose domestic conduct the categorical rule places beyond external reach. They hold no seat in the international system, no petition right against their own government's invocation of the rule, and no remedy short of flight — an exit their governments increasingly close. What the rule protects from external reach is the conduct they bear.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, populations_under_repressive_regimes, payer,
    powerless, biographical, trapped, national).

% Face targeted repression inside states that invoke non-interference against any external protection. International mechanisms that could reach them operate only where their government consents or where no veto blocks action, which under the categorical form is never by right. Flight is the only exit, and receiving states handle them through the same discretionary domestic authority the rule protects.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, persecuted_minority_groups, payer,
    powerless, biographical, trapped, regional).

% Organize for external response to atrocity — NGOs, human rights movements, cross-border solidarity campaigns. The categorical rule defines their project as illegitimate per se: they hold no decision rights in the forums where the rule is maintained, and their advocacy reaches the system only as lobbying directed at the governments that hold the vetoes over their proposals.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, humanitarian_intervention_advocates, excluded,
    organized, generational, constrained, global).

% Administers the rule's principal exception through Chapter VII enforcement authority. Five members hold vetoes that determine when the categorical rule yields; each veto cast in the rule's defense is also an exercise of the protection the rule provides. The Council's authorization practice — some interventions permitted, most blocked — is where the categorical claim meets application.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Study the rule's doctrine and practice from outside its operation: documenting the gap between the categorical claim and state behavior, tracing the norm's history from the 1648 settlement through the 1945 codification, and mapping the interpretive disputes. They hold no decision power over the rule's maintenance and collect nothing from its operation.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, authoritarian_regime_leaderships).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of coexistence among radically different domestic regimes: by making domestic arrangements categorically off-limits to external judgment and force, it removes the standing incentive for states and blocs to war over, sanction, or subvert one another's internal order, and underwrites diplomatic recognition, treaty-making, and diplomatic immunity.
% TRANSFER_FUNCTION: Moves immunity from external accountability to incumbent state leaderships — worth most to those with the most to conceal — and moves the corresponding burden (repression, persecution, and crisis without external recourse) onto the domestic populations those leaderships govern.
% ABSENT_VOICES: Populations under repressive regimes and persecuted minorities were never seated: Westphalia was negotiated among princes, the Charter among governments, and the UN's membership is states only. The people whose external protection the categorical rule trades away have no vote in its maintenance and no petition right against its invocation by their own government. Humanitarian advocates hold voice only as accredited NGOs without decision rights.
% DISAPPEARANCE_RATIONALE: If the categorical rule vanished overnight, every state's domestic conduct would become contestable: intervention doctrines would proliferate, recognition and treaty practices would unravel, and leaderships now shielded would lose their principal protection. The interstate order would reorganize around an exception-bearing or graded form of the rule rather than persist as-is — arrangements demonstrably depend on the categorical form holding.
% FOUNDING_PROBLEM: The Wars of Religion and the Thirty Years' War: interstate war prosecuted to remake other states' domestic religious and constitutional arrangements, with no agreed rule limiting it. The 1648 settlement — codified for the modern system in Charter Article 2(7) after two world wars — built the categorical rule to make domestic arrangements categorically off-limits.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians and international-relations scholars outside every beneficiary seat attest both the founding problem and its persistence: the historiography of the Peace of Westphalia and the post-1945 codification corroborates that the rule was built to end confessional and ideological intervention, while the same scholarship (e.g., work documenting sovereignty's organized hypocrisy) disputes the founding myth's cleanliness without disputing the problem. That wars over other states' domestic orientation persist is attested in the public record of contemporary conflicts. No beneficiary party's attestation is relied on.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.58 at interval end: high but not maximal, because the rule retains a real coordination function (it demonstrably suppresses interstate war over domestic arrangements) while its costs — unremedied repression, persecution without recourse — fall on populations who hold no seat in the system that maintains the rule. Suppression is 0.70 and is authored as a raw structural property (the engine scales only extractiveness): the rule is enforced through Security Council vetoes, recognition politics, and the categorical delegitimization of intervention proposals, which makes external-response advocacy structurally voiceless rather than merely outvoted; it is not total — Kosovo, Iraq 2003, and R2P-authorized actions breach it at real cost. Theater is 0.48: a large and growing share of the rule's maintenance is ritual — sovereignty invoked most loudly by the regimes with the least domestic legitimacy — while diplomatic immunity, recognition, and war-prevention work continues underneath. Accessibility collapse is 0.45: the categorical claim does not collapse its alternatives — the violation-triggered and capacity-graded forms remain fully live and were formally admitted into the system's vocabulary in 2005 — so alternatives persist at real strength. Resistance is 0.60: R2P adoption, humanitarian intervention practice, the ICC, sanctions coalitions, and human rights treaty bodies constitute organized, continuing push against the categorical form. The measurement series run on one shared time grid (1945–2025 at decade points) so every tracked metric is authored at every examined time point; the drift narrative is decolonization-era peak utility (ε dips at t=10 as the shield protects new states from recolonization), Cold War client-shielding accumulation, post-Cold War visibility of the atrocity cost (Rwanda under the shield), and post-2005 formal contestation raising both the cost of categorical persistence and the enforcement effort required to hold it. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change: the rule moved from broad postwar consensus (largely self-enforcing) through Cold War bloc discipline to the contemporary regime of active defense — veto use against atrocity response, counter-mobilization against exception-bearing reform. Coalition note: the payer seats' natural coalition route — cross-border solidarity and diaspora advocacy aimed at external protection — is precisely what the categorical rule delegitimizes, so the powerless seats' coalition potential is structurally blunted by the constraint itself rather than by their own dispersion.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats experience the rule as the foundation of order: from the leadership seat, the categorical form is what makes coexistence among radically different regimes possible, and every proposed exception is a door to universal intervention. From the payer seats — populations under repressive rule — the same categorical form is the removal of all external recourse: the wall that keeps the world out while the government does as it likes. The Security Council seat sees the rule through its exception-carving role, where each veto in the rule's defense is also an exercise of the shield. The observer seat sees the gap between the categorical claim and practice as the primary datum. The engine computes per-seat classifications from the structural data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map directly onto the seats: authoritarian regime leaderships and great-power governments collect the rule's protection (d near the beneficiary end), and both hold arbitrage-grade exit — they invoke the rule selectively and breach it when projecting power — which damps their effective extraction further. Democratic governments collect the same shield but bear real costs when it blocks response to atrocities and when rivals invoke it against them, so their derived d sits moderately above the pure beneficiary end. Populations under repressive regimes and persecuted minorities bear the rule's costs with no exit (trapped, powerless), putting them near the full-target end, where effective extraction is amplified. No directionality overrides are authored: the beneficiary/victim plus exit data differentiates the seats sufficiently, and the available override granularity (power-atom keyed) would flatten genuinely different institutional directionalities — the three institutional beneficiary seats do not share one d.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the rule as pure coordination (its defenders' claim) would erase the asymmetric structure: the coordination benefit is collective, but the costs fall on a seatless population. Reading it as pure extraction would erase the founding function: interstate war prosecuted to remake other states' domestic arrangements was the problem the rule was built to solve, and the categorical form has measurably suppressed it. Tangled rope holds both: the same structure that prevents interstate war shields domestic atrocity, and the enforcement machinery (veto, recognition politics) exists to hold exactly that combination in place. The founding problem remains live — wars over other states' domestic orientation persist — so this is not a mandate outliving its function; the mandate is intact and double-edged. The mandatrophy risk to watch is displacement: if the violation-triggered reading completes its institutionalization, this reading's persistence would become increasingly theatrical (defended by its worst violators alone), and the theater_ratio series is the leading indicator of that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (absolute_sovereignty) of the westphalian_sovereignty kernel; what structurally changes under the sibling readings (conditional_sovereignty, graduated_sovereignty)?',
    'The sibling stories'' own classifications: conditional_sovereignty authors a violation-triggered intervention structure with its own ε and victim set; graduated_sovereignty authors a capacity/legitimacy spectrum likewise. Cross-reading comparison is valid only between stories, never within this one.',
    'If a sibling reading displaces this one as the operative norm, the operative story changes — this story''s ε and classification remain fixed to this reading''s standing arrangement; the corpus tracks displacement through the network edges, not by revising this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Kernel-reading indexicality: one reading of westphalian_sovereignty; siblings are separate constraints.').

omega_variable(
    interference_trigger_location,
    'Where is the kernel disagreement located — which structural element of the arrangement do the readings differ on?',
    'Comparative authoring of the sibling stories: the trigger structure (none / systematic-violation threshold / capacity-legitimacy spectrum) and its adjudication seat (none / international bodies / graded assessment) is the differing element; each story authors its own trigger and adjudication structure.',
    'The victim set is the downstream delta: this reading''s victims are all populations under repressive rule; the conditional reading''s victims shrink to the below-threshold remainder; the graduated reading''s victims are the below-cutoff remainder.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interference_trigger_location, conceptual, 'The readings disagree on the trigger and adjudication structure of the non-interference rule.').

omega_variable(
    net_violence_prevention_balance,
    'Does the categorical rule prevent more external (interstate) violence than it licenses internally (domestic atrocity without external recourse)?',
    'Comparative conflict data: intervention frequency and atrocity rates under categorical-rule regimes versus trigger-based regimes; natural experiments from R2P-authorized interventions and from periods of norm erosion.',
    'If prevention dominates, the coordination function dominates and the constraint''s computed classification trends toward rope; if atrocity-shielding dominates, toward snare. The tangled_rope classification is stable only while both components are substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_violence_prevention_balance, empirical, 'The net-balance question underlying the coordination/extraction split.').

omega_variable(
    norm_vs_veto_attribution,
    'Is the measured extraction attributable to the categorical norm itself or to the P5 veto structure that enforces it (blocking intervention even where broad consensus for it exists)?',
    'Counterfactual and comparative analysis: atrocity-shielding in norm-invoking states outside Security Council reach (General Assembly-only condemnation cases) versus cases where the veto alone blocked action that other organs and majorities supported.',
    'If the veto is the principal extractor, part of the measured extraction belongs to an enforcement-structure constraint rather than the norm-level constraint, and the family decomposition should split the veto story out as its own file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_vs_veto_attribution, empirical, 'Attribution of extraction between the norm and its enforcement machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(absolute_sovereignty_tr_t0, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0, 0.18).
narrative_ontology:measurement(absolute_sovereignty_tr_t10, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 10, 0.2).
narrative_ontology:measurement(absolute_sovereignty_tr_t20, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 20, 0.23).
narrative_ontology:measurement(absolute_sovereignty_tr_t30, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 30, 0.27).
narrative_ontology:measurement(absolute_sovereignty_tr_t40, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 40, 0.31).
narrative_ontology:measurement(absolute_sovereignty_tr_t50, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 50, 0.36).
narrative_ontology:measurement(absolute_sovereignty_tr_t60, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 60, 0.41).
narrative_ontology:measurement(absolute_sovereignty_tr_t70, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 70, 0.45).
narrative_ontology:measurement(absolute_sovereignty_tr_t80, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 80, 0.48).

% Extraction over time
narrative_ontology:measurement(absolute_sovereignty_be_t0, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(absolute_sovereignty_be_t10, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(absolute_sovereignty_be_t20, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(absolute_sovereignty_be_t30, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(absolute_sovereignty_be_t40, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(absolute_sovereignty_be_t50, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(absolute_sovereignty_be_t60, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(absolute_sovereignty_be_t70, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 70, 0.55).
narrative_ontology:measurement(absolute_sovereignty_be_t80, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 80, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(absolute_sovereignty_su_t0, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(absolute_sovereignty_su_t10, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(absolute_sovereignty_su_t20, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(absolute_sovereignty_su_t30, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(absolute_sovereignty_su_t40, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(absolute_sovereignty_su_t50, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(absolute_sovereignty_su_t60, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(absolute_sovereignty_su_t70, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 70, 0.68).
narrative_ontology:measurement(absolute_sovereignty_su_t80, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 80, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).

% DUAL FORMULATION NOTE:
% The colloquial label 'Westphalian sovereignty' covers three structurally distinct claims: categorical non-interference (this file), violation-triggered intervention legitimacy, and capacity/legitimacy-graded sovereignty. Each reading is authored as a separate constraint story with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle; this file instantiates the absolute reading. ε differs across the family because the victim sets differ: the categorical reading's victims are all populations under repressive rule; the conditional reading's are the below-threshold remainder; the graduated reading's are the below-cutoff remainder. The upstream reading (this file, highest historical entrenchment) influences the siblings' operating environment: every exception-bearing proposal is argued against the categorical baseline this reading maintains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
