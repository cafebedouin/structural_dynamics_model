% ============================================================================
% CONSTRAINT STORY: procedural_due_process__goldberg_hearing_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_procedural_due_process__goldberg_hearing_rights_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: procedural_due_process__goldberg_hearing_rights_reading
 *   human_readable: Goldberg Hearing Rights Reading: Welfare Subsistence Conditioned on Prior Hearing
 *   domain: legal/constitutional/administrative
 *
 * SUMMARY:
 *   Goldberg v. Kelly (1970) established that welfare recipients have a
 *   property interest in public assistance and that the Due Process Clause
 *   mandates an evidentiary hearing before the state can terminate benefits.
 *   The decision reframed welfare from discretionary grace to entitlement,
 *   making subsistence receipt conditional on prior procedural process. This
 *   constraint story instantiates one reading of the contested
 *   procedural_due_process kernel — the reading that privileges the 'brutal
 *   need' of recipients over fiscal convenience and administrative speed. The
 *   Goldberg reading creates a structural tension: the state must feed the
 *   citizen (through continuation pending hearing) even when it proposes to
 *   terminate. This is a tangled rope because it contains both genuine
 *   coordination (the hearing process permits correction of errors and
 *   provides legitimacy) and real extraction (the state's fiscal and
 *   administrative discretion is constrained; it cannot unilaterally remove
 *   subsistence to manage budget crisis). The constraint exhibits all six
 *   classification types across different observer positions, making it an
 *   ideal exemplar for how the same doctrinal choice can be read as
 *   coordination, extraction, natural law, or ritual depending on the
 *   observer's structural position. Extractiveness is moderate (0.35) because
 *   while the constraint does constrain state action, it does not eliminate
 *   it — the state retains the authority to terminate after proper hearing;
 *   the extraction is temporal (delay) and procedural (cost) rather than
 *   absolute. Theater ratio has risen over the interval (0.25 to 0.42)
 *   because the hearing process, while initially functional, has become
 *   increasingly routinized and pro forma as jurisdictions adapted to the
 *   requirement through institutional inertia and capacity exhaustion.
 *
 * KEY AGENTS:
 *   - Welfare Recipients Facing Termination: Primary beneficiary (powerless/trapped) — subject to subsistence cutoff; structured into the process through the hearing right; gain from delay and from opportunity to contest termination grounds
 *   - Administrative Bureaucracy: Secondary beneficiary and victim (moderate/constrained) — benefits from legitimacy and error-correction of hearings; bears costs of delay and administrative expense; active enforcement burden falls on this agent
 *   - State Fiscal Authority / Legislature: Victim (institutional/arbitrage, immediate horizon) — prevented from unilateral termination; subsistence obligation extended through hearing period; arbitrage theoretically available but politically closed
 *   - Due Process Coalition / Legal Services: Organized beneficiary (organized/constrained, generational horizon) — advocacy organizations enforce the reading through litigation and administrative pressure; maintain coalition pressure for compliance; perceive vulnerability to retreat
 *   - Supreme Court / Doctrine Authority: Institutional author (institutional/arbitrage, civilizational horizon) — holds power to revise the reading through subsequent doctrine; currently constrained by precedent stare decisis but maintains ultimate authority to rebalance
 *   - Analytical Observer: Position (analytical/analytical, civilizational/universal) — risks naturalizing the contingent doctrinal choice as constitutional immutability; engine will flag as false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(procedural_due_process__goldberg_hearing_rights_reading, 0.35).
domain_priors:suppression_score(procedural_due_process__goldberg_hearing_rights_reading, 0.68).
domain_priors:theater_ratio(procedural_due_process__goldberg_hearing_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(procedural_due_process__goldberg_hearing_rights_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(procedural_due_process__goldberg_hearing_rights_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(procedural_due_process__goldberg_hearing_rights_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(procedural_due_process__goldberg_hearing_rights_reading, tangled_rope).
narrative_ontology:human_readable(procedural_due_process__goldberg_hearing_rights_reading, "Goldberg Hearing Rights Reading: Welfare Subsistence Conditioned on Prior Hearing").
narrative_ontology:topic_domain(procedural_due_process__goldberg_hearing_rights_reading, "legal/constitutional/administrative").

domain_priors:requires_active_enforcement(procedural_due_process__goldberg_hearing_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(procedural_due_process__goldberg_hearing_rights_reading, 'ea48bf8b-5df0-4078-93b1-9b13eb9418f3').
narrative_ontology:cs_kernel_codification('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', fixed_text).
narrative_ontology:cs_authority_grounding('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', lineage).
narrative_ontology:cs_interpretation_layer_present('ea48bf8b-5df0-4078-93b1-9b13eb9418f3').
narrative_ontology:cs_reading_relation('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', procedural_due_process__mathews_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', procedural_due_process__new_property_reading, influences).
narrative_ontology:cs_axiom('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', foundational, subsistence_entitlement_beats_fiscal_convenience).
narrative_ontology:cs_axiom_status(subsistence_entitlement_beats_fiscal_convenience, holdable).
narrative_ontology:cs_axiom_grounding('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', subsistence_entitlement_beats_fiscal_convenience, deontological).
narrative_ontology:cs_axiom('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', foundational, hearing_prerequisite_to_deprivation_of_entitlement).
narrative_ontology:cs_axiom_status(hearing_prerequisite_to_deprivation_of_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', hearing_prerequisite_to_deprivation_of_entitlement, deontological).
narrative_ontology:cs_reference_frame('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', due_process_as_procedural_fairness_baseline).
narrative_ontology:cs_drift_state('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', contemporary_retrenchment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ea48bf8b-5df0-4078-93b1-9b13eb9418f3', '').
narrative_ontology:cs_kernel_id(procedural_due_process__goldberg_hearing_rights_reading, procedural_due_process).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(procedural_due_process__goldberg_hearing_rights_reading, welfare_recipients_facing_termination).
narrative_ontology:constraint_victim(procedural_due_process__goldberg_hearing_rights_reading, administrative_efficiency).
narrative_ontology:constraint_victim(procedural_due_process__goldberg_hearing_rights_reading, state_fiscal_discretion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WELFARE RECIPIENT FACING TERMINATION (ROPE) — The recipient experiences the hearing requirement as genuine coordination: the state must communicate the termination ground, and the recipient has structural opportunity to contest factual errors or propose alternatives. The 'brutal need' framing (subsistence entitlement) transforms what could be unilateral state action into a mutual process. Despite material powerlessness and no exit option (trapped), the reading grants the recipient a structural voice in the determination. This is Rope from the recipient's perspective because the hearing mechanism itself constitutes coordination — not merely consultation, but a structure that can change the outcome.
constraint_indexing:constraint_classification(procedural_due_process__goldberg_hearing_rights_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADMINISTRATIVE BUREAUCRACY (TANGLED ROPE) — The agency experiences the Goldberg reading as both coordination and extraction. Coordination: hearings provide administrative legitimacy, permit identification of errors, and create an evidentiary record for judicial review. Extraction: the requirement delays termination, multiplies administrative cost, and constrains the agency's summary discretion. The agency must bear the hearing process before it can act — this is active enforcement cost. The beneficiary (recipient) is not the only beneficiary here; the agency benefits from having defensible, reviewable procedures. But the extraction burden is real: speed is sacrificed for process.
constraint_indexing:constraint_classification(procedural_due_process__goldberg_hearing_rights_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL STATE / LEGISLATIVE BUDGET AUTHORITY (SNARE) — From the legislator's immediate fiscal horizon, Goldberg imposes a pure extraction: the state cannot unilaterally terminate benefits to manage fiscal stress. The hearing requirement forces continuation of subsistence during the process, extending aggregate benefit outlays. The state has nominal arbitrage (it can change the rule), but doing so requires legislative action that reverses settled constitutional doctrine — arbitrage is theoretically available but politically closed. The Goldberg reading creates permanent fiscal constraint: the subsistence entitlement is no longer terminable at will.
constraint_indexing:constraint_classification(procedural_due_process__goldberg_hearing_rights_reading, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM ADVOCATE / LEGAL COMMUNITY (TANGLED ROPE) — Legal scholars and advocates see Goldberg as both coordination and extraction. Coordination: the reading establishes a structural baseline for fair process, shifts the burden of proof onto the state, and creates a template for due process across multiple benefit regimes (food stamps, housing, medical). Extraction: the reading freezes a particular procedural form (live hearing before termination) that may not optimally balance all interests. The reform perspective is generational — across multiple cases and policy cycles, the Goldberg framework becomes both the legal infrastructure and a constraint on revisiting the balance. The powerful position (legal expertise, political influence) permits some mobility: advocates can argue for Mathews-style balancing or for deeper property-rights readings, but they are constrained by Goldberg's established form.
constraint_indexing:constraint_classification(procedural_due_process__goldberg_hearing_rights_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: DUE PROCESS COALITION / ADVOCACY ORGANIZATIONS (SCAFFOLD) — Legal services organizations, welfare rights groups, and civil rights advocates see Goldberg as a temporary protective structure with a hidden sunset clause. The reading works so long as courts enforce it and legislatures do not repeal it. The organized agents (Legal Services Corporation, American Civil Liberties Union, National Welfare Rights Organization in the historical moment) experience the constraint as coalition-enforced: the hearing requirement persists because advocacy maintains pressure for compliance. But the coalition perceives vulnerability — the structure depends on continued judicial support and legislative acquiescence. As fiscal pressure mounts and courts turn skeptical of entitlements (post-2010), the sunset becomes visible: Goldberg can be eroded through statutory modification, regulatory narrowing, or doctrinal retreat. Theater ratio is moderate (hearings are real processes, not pure performance), but the constraint's lifespan is bounded by coalition endurance and judicial backing.
constraint_indexing:constraint_classification(procedural_due_process__goldberg_hearing_rights_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: BUREAUCRATIC RITUAL / INSTITUTIONAL EXHAUSTION (PITON) — From a civilizational horizon looking at contemporary administrative practice, Goldberg's hearing requirement has become largely performative. Many jurisdictions conduct rapid, pro forma hearings that rarely reverse initial termination decisions. The process theater persists (hearings are scheduled, records are kept, applicants appear) but the functional efficacy has atrophied. The constraint remains in place through institutional inertia — repealing it would require legislative action and would face political opposition from established advocacy coalitions — but the actual impact on termination outcomes has diminished. Theater ratio is high because the process is maintained as ritual rather than functional safeguard. This perspective reveals that Goldberg's mandate, after decades of implementation, has become a vestigial constraint: the hearing happens, but real agency control over termination outcomes has been restored through other mechanisms (speed of process, complexity of documentation, inadequate representation, exhaustion of appellants).
constraint_indexing:constraint_classification(procedural_due_process__goldberg_hearing_rights_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a universal/civilizational analytical position, the Goldberg reading can be framed as expressing an immutable constitutional principle: due process is simply what procedural fairness requires, unchangeable in its core demand (a hearing before deprivation of a justified entitlement). The principle appears self-evident and inescapable. However, the structural data contradicts this framing: Goldberg is a reading of a contested kernel, beneficiaries and victims are clearly identifiable, and the constraint's force depends on ongoing judicial and institutional enforcement. This perspective instantiates a false summit — the naturalization of a contingent doctrinal choice as a timeless constitutional truth. The engine will flag this as naturalization rather than genuine immutability.
constraint_indexing:constraint_classification(procedural_due_process__goldberg_hearing_rights_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_due_process__goldberg_hearing_rights_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(procedural_due_process__goldberg_hearing_rights_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(procedural_due_process__goldberg_hearing_rights_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(procedural_due_process__goldberg_hearing_rights_reading, TR),
    TR >= 0.70.

:- end_tests(procedural_due_process__goldberg_hearing_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): The constraint imposes moderate extraction on the state by delaying termination and requiring process, but does not eliminate the state's ultimate authority to terminate. The 'brutal need' framing (subsistence as minimum necessity, not luxury benefit) justifies the extraction: feeding the person is prioritized over administrative convenience. The extraction value reflects the real fiscal and speed costs to the state, scaled down from initial estimates because the state retains termination authority after hearing. Suppression (0.68): High. The state's option to unilaterally terminate benefits is substantially suppressed — the state cannot exercise this option without first conducting a hearing and providing the recipient an opportunity to respond. Alternatives (administrative termination without process, fiscal adjustment through benefit reduction rather than termination) are legally unavailable. The suppression is not total (the state can still terminate post-hearing) but it is severe (immediate termination is foreclosed). Theater ratio (0.42): Moderate-high. Initially (t=0), Goldberg hearings were functionally real processes conducted by independent or semi-independent hearing officers. Over the interval (t=0 to t=15), as jurisdictions routinized compliance, hearings became increasingly pro forma — rapid, perfunctory, with high rates of initial agency decisions surviving challenge. Contemporary theater reflects institutional exhaustion: the hearing is conducted but rarely reverses the initial termination; the process theater persists but functional contestation has diminished. The rising trajectory (0.25 → 0.42) models this institutional routinization.
 *
 * PERSPECTIVAL GAP:
 *   The Goldberg reading produces six distinct classifications: Rope (recipient sees coordination), Tangled Rope (bureaucracy experiences both coordination and extraction), Snare (fiscal state sees pure extraction), Tangled Rope (reform advocate sees infrastructure and constraint), Scaffold (coalition sees temporary protection with sunset risk), Piton (institutional exhaustion sees vestigial ritual), and Mountain (analytical observer risks naturalizing as constitutional necessity). The gap between recipient (Rope) and fiscal state (Snare) is maximum — the same procedural mandate appears as enabling process to the vulnerable and as disabling constraint to the authority. The gap between initial Goldberg efficacy (Rope, genuine contestation) and contemporary institutional exhaustion (Piton, pro forma hearings) reveals that the constraint's functional character has degraded: the process persists but its force has atrophied. The analytical observer's Mountain reading is a false summit: the 'due process demands a hearing' framing naturalizes a doctrinal choice that could be rebalanced (Mathews) or regrounded (new property reading).
 *
 * DIRECTIONALITY LOGIC:
 *   The Goldberg reading privileges the recipient's structural position: the recipient is trapped (no exit from subsistence dependence) but is structured into the determination process through the mandatory hearing. This creates a perspectival gap. From the recipient's perspective (powerless/trapped/biographical), the hearing is coordination — it provides voice and opportunity for correction. From the state's perspective (institutional/immediate, thinking in budget cycles), the hearing is pure extraction — it delays action and extends fiscal obligation. From the coalition perspective (organized/generational), the constraint is a protective structure with sunset risk. The derivation chain maps beneficiary/victim declarations to directionality: recipients are primary beneficiary (d toward 0, benefiting from process); state fiscal discretion is victim (d toward 1, bearing the constraint). The institutional power atom for the state (institutional/arbitrage at immediate horizon) produces high d (state is target of extraction) and high f(d), making χ substantial despite moderate base ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The Goldberg reading resolves mandatrophy by exposing how the same doctrinal choice can be legitimately characterized as coordination (Rope from the recipient), as mixed coordination-extraction (Tangled Rope from bureaucracy), or as pure extraction (Snare from fiscal authority), depending on observer position and time horizon. The mandatrophy is not 'which type is correct?' but 'which perspective are you measuring from?'. The reading instantiates the presheaf structure: it is Rope for the trapped recipient (voice and opportunity matter more than speed because escape is impossible), Tangled Rope for the bureaucracy (real coordination functions coexist with real costs), Snare for the fiscal state (extraction without consent, at immediate budget-cycle horizon), and Piton for the institution watching its own process decay into ritual. The false-summit analytical perspective reveals that naturalizing Goldberg as constitutional necessity (Mountain) mystifies a contestable reading of the due-process kernel. The sibling readings (Mathews balancing, new property) are not ruled out by Goldberg; they coexist as live doctrinal alternatives that could gain authority through subsequent Supreme Court opinion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    brutal_need_vs_property_grounding,
    'Does Goldberg''s mandatory hearing protect subsistence as a consequence of brutal need (Lassiter framing: the individual''s desperate circumstance) or as a consequence of property status in the benefit (Reich framing: government largesse as modern estate)?',
    'Textual analysis of Goldberg opinion; comparison with contemporaneous property-based arguments (Reich v. New York, 1966); subsequent doctrine clarification (e.g., does the hearing requirement survive if the benefit is recharacterized as discretionary grace rather than earned entitlement?).',
    'If grounded in brutal need: the reading is consequentialist (justified by harm to the individual) and could apply to any termination causing severe deprivation (potentially broader scope). If grounded in property: the reading is formalist (justified by status classification) and depends on the benefit being classified as property (more fragile if legislatures redefine benefits as discretionary). The two groundings suggest different sibling readings and different vulnerability to mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brutal_need_vs_property_grounding, conceptual, 'Whether Goldberg protects subsistence based on recipient need or benefit property status').

omega_variable(
    hearing_sufficiency_empirical,
    'Do live hearings (as required by Goldberg) actually prevent wrongful terminations at meaningful rates, or has the procedure become vestigial after decades of institutional routinization?',
    'Empirical analysis of hearing outcomes: reversal rates for initial agency termination decisions; correlation between receipt of hearing and ultimate receipt of benefits; comparison across jurisdictions with varying hearing quality and accessibility.',
    'If hearings prevent wrongful terminations at significant rates (>15%): the piton classification is incorrect; the constraint retains functional force. If reversal rates are <5% and concentrated in jurisdictions with high-quality representation: the piton classification is confirmed; the constraint is largely theatrical, preserved by institutional inertia. This resolution would affect the theater_ratio measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hearing_sufficiency_empirical, empirical, 'Empirical efficacy of Goldberg hearings in preventing wrongful terminations').

omega_variable(
    reading_kernel_contestation_scope,
    'Which sibling reading (mathews_balancing_reading or new_property_reading) would be authorized by the same kernel-codifying authority (the Supreme Court) as the Goldberg reading if the kernel were revisited today?',
    'Doctrinal trajectory analysis: tracking subsequent Supreme Court opinions on due process in benefit termination (e.g., O''Bannon v. Town Court Nursing Center, 1979; Mathews v. Eldridge, 1976); assessment of contemporary Court composition''s attitude toward entitlement-as-property doctrine; hypothetical legal reasoning about which reading best fits current constitutional commitments.',
    'If mathews_balancing is more consonant with current doctrine: the Goldberg reading is vulnerable to revisionary Supreme Court action; the scaffold perspective''s sunset clause is shorter. If new_property reading gains authority: Goldberg may be reinforced through property-status protection (potentially increasing extractiveness of the constraint on the state). This affects perceived stability and the mandatrophy landscape.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contestation_scope, conceptual, 'Which sibling reading aligns with contemporary constitutional authority').

omega_variable(
    kernel_reading_overdetermination,
    'Is the Goldberg reading over-determined by the kernel (do multiple independent readings of the due-process kernel lead to the Goldberg conclusion), or is it under-determined (would alternative readings be equally valid from the same kernel)?',
    'Hypothetical natural-language reasoning: could a principled reader of the due-process kernel, starting from the same constitutional text, arrive at the Mathews optimization formula or the new-property reading as equally valid? Or does the kernel''s language and history more directly support the Goldberg prioritization of brutal need over administrative convenience?',
    'If over-determined: Goldberg is a robust reading with deep grounding in the kernel; harder to displace through doctrinal revision. If under-determined: the reading depends more on the particular Court majority that issued it; more vulnerable to subsequent Courts rebalancing the same kernel factors. This affects the perceived permanence of the constraint and the confidence in the scaffold sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_overdetermination, conceptual, 'Whether the Goldberg reading is over- or under-determined by the due-process kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(procedural_due_process__goldberg_hearing_rights_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(goldberg_theater_t0_functional_hearings, procedural_due_process__goldberg_hearing_rights_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(goldberg_theater_t8_routinization, procedural_due_process__goldberg_hearing_rights_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(goldberg_theater_t15_contemporary_ritual, procedural_due_process__goldberg_hearing_rights_reading, theater_ratio, 15, 0.42).

% Extraction over time
narrative_ontology:measurement(goldberg_extractiveness_t0_immediate_post_decision, procedural_due_process__goldberg_hearing_rights_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(goldberg_extractiveness_t8_mid_seventies, procedural_due_process__goldberg_hearing_rights_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(goldberg_extractiveness_t15_contemporary, procedural_due_process__goldberg_hearing_rights_reading, base_extractiveness, 15, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(procedural_due_process__goldberg_hearing_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(procedural_due_process__goldberg_hearing_rights_reading, procedural_due_process__mathews_balancing_reading).
narrative_ontology:affects_constraint(procedural_due_process__goldberg_hearing_rights_reading, procedural_due_process__new_property_reading).

% DUAL FORMULATION NOTE:
% The procedural_due_process kernel decomposes into three distinct constraint stories, each with different ε values, different beneficiary/victim structures, and different institutional vulnerabilities. Goldberg (this story, ε=0.35) privileges recipient need and mandates process before termination. Mathews (ε=0.28) treats due process as an optimization problem and reduces hearing requirements based on error-risk and burden calculations. New Property (ε=0.42) grounds the right in property status and potentially extends hearing rights beyond Goldberg's scope to any government benefit or license. All three interpret the same kernel but produce different constraints. They are linked through the network: Goldberg influences Mathews (Mathews revises Goldberg's framework by introducing balancing), and both influence New Property (property-status grounding could extend the obligation). The ε-invariance principle applies: if Goldberg were measured under Mathews's balancing formula, a different ε would result because the observable (what processes are required) would change. Each reading gets its own story, its own ε, and its own measurements. The constraint family captures the historical progression: Goldberg established that process matters; Mathews rebalanced the amount of process required; New Property grounded the right more deeply in status classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(procedural_due_process__goldberg_hearing_rights_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
