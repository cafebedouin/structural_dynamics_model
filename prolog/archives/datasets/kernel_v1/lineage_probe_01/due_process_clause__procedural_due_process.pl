% ============================================================================
% CONSTRAINT STORY: due_process_clause__procedural_due_process
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_due_process_clause__procedural_due_process, []).

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
 *   constraint_id: due_process_clause__procedural_due_process
 *   human_readable: Procedural Due Process: Notice, Hearing, and Neutral Decider
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The procedural due process reading of the Due Process Clause constrains
 *   the state's power to deprive individuals of liberty or property by
 *   requiring notice, an opportunity to be heard, and a neutral
 *   decision-maker before such deprivation occurs. This constraint operates
 *   at the intersection of legitimacy (the state's need to appear lawful) and
 *   protection (the individual's need for safeguards against arbitrary
 *   action). The Mathews v. Eldridge three-factor test (private interest,
 *   government interest, risk of error) aims to scale the process required by
 *   the stakes of deprivation. However, this balancing produces
 *   indeterminacy: courts have applied identical factors to reach different
 *   conclusions across domains, and the balancing itself becomes a site of
 *   extraction—high-cost litigation where procedural disputes displace
 *   substantive resolution. The constraint exhibits Tangled Rope structure:
 *   it genuinely coordinates by preventing summary deprivation and
 *   establishing legitimacy for state action, while simultaneously extracting
 *   through delay, cost burdens, and the asymmetric resource burden on
 *   individual challengers. The theater ratio has increased over 50 years as
 *   procedural complexity has grown (discovery rules, expert witness
 *   standards, appellate review standards) while the protective function of
 *   procedure has atrophied in high-speed domains (immigration, police stops,
 *   administrative benefits termination). The analytical observer risks
 *   seeing procedural due process as an immutable requirement of any
 *   legitimate legal system (mountain perspective), but the structural data
 *   reveals contingent institutional arrangements: the state's ability to
 *   suppress substantive challenges through procedural complexity, the
 *   judicial system's interest in maintaining legitimacy through procedure
 *   rather than substantive justice, and the civil rights movement's
 *   generational struggle to expand procedural protections despite resource
 *   constraints.
 *
 * KEY AGENTS:
 *   - Individual rights holders: Primary beneficiary (institutional/arbitrage) — benefit from procedural protections against arbitrary state deprivation; but realize benefit only with adequate counsel and resources
 *   - Accused defendants: Primary beneficiary and victim (powerless/trapped and moderate/constrained, depending on counsel availability) — nominally protected by procedure but often unable to exercise protections effectively
 *   - Judicial system: Beneficiary (institutional/arbitrage) — maintains legitimacy and authority through procedural compliance; experiences procedure as coordination mechanism that enables state power
 *   - Administrative agencies: Victim (institutional/mobile) — constrained by procedural requirement to provide hearings; extract from suppression (delay, complexity, resource burden); have arbitrage options (summary procedures, expedited removal, informal adjudication)
 *   - Civil rights organizations: Organized beneficiary/victim (organized/constrained) — use procedure to challenge state action but bear asymmetric cost burden compared to government counsel
 *   - Police and enforcement: Victim (institutional/mobile) — constrained by procedural requirements (warrant requirement, Miranda rule, suppression doctrine); have arbitrage options (summary stop-and-frisk justified by Terry, qualified immunity protecting from damages liability)
 *   - Analytical observer: Sees potential natural law (analytical/analytical) — risks naturalizing contingent arrangements as immutable requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(due_process_clause__procedural_due_process, 0.38).
domain_priors:suppression_score(due_process_clause__procedural_due_process, 0.48).
domain_priors:theater_ratio(due_process_clause__procedural_due_process, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(due_process_clause__procedural_due_process, extractiveness, 0.38).
narrative_ontology:constraint_metric(due_process_clause__procedural_due_process, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(due_process_clause__procedural_due_process, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(due_process_clause__procedural_due_process, tangled_rope).
narrative_ontology:human_readable(due_process_clause__procedural_due_process, "Procedural Due Process: Notice, Hearing, and Neutral Decider").
narrative_ontology:topic_domain(due_process_clause__procedural_due_process, "legal/constitutional").

domain_priors:requires_active_enforcement(due_process_clause__procedural_due_process).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(due_process_clause__procedural_due_process, 'da0834c7-1a99-4120-b28d-4124a1fda1b6').
narrative_ontology:cs_kernel_codification('da0834c7-1a99-4120-b28d-4124a1fda1b6', fixed_text).
narrative_ontology:cs_authority_grounding('da0834c7-1a99-4120-b28d-4124a1fda1b6', lineage).
narrative_ontology:cs_interpretation_layer_present('da0834c7-1a99-4120-b28d-4124a1fda1b6').
narrative_ontology:cs_reading_relation('da0834c7-1a99-4120-b28d-4124a1fda1b6', due_process_clause__incorporation_doctrine, coexists_with).
narrative_ontology:cs_reading_relation('da0834c7-1a99-4120-b28d-4124a1fda1b6', due_process_clause__substantive_due_process, coexists_with).
narrative_ontology:cs_axiom('da0834c7-1a99-4120-b28d-4124a1fda1b6', foundational, process_scales_with_stakes).
narrative_ontology:cs_axiom_status(process_scales_with_stakes, holdable).
narrative_ontology:cs_axiom_grounding('da0834c7-1a99-4120-b28d-4124a1fda1b6', process_scales_with_stakes, instrumental).
narrative_ontology:cs_axiom('da0834c7-1a99-4120-b28d-4124a1fda1b6', foundational, procedure_content_agnostic).
narrative_ontology:cs_axiom_status(procedure_content_agnostic, holdable).
narrative_ontology:cs_axiom_grounding('da0834c7-1a99-4120-b28d-4124a1fda1b6', procedure_content_agnostic, deontological).
narrative_ontology:cs_reference_frame('da0834c7-1a99-4120-b28d-4124a1fda1b6', due_process_requires_scaled_procedure).
narrative_ontology:cs_drift_state('da0834c7-1a99-4120-b28d-4124a1fda1b6', contemporary_high_volume_administration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da0834c7-1a99-4120-b28d-4124a1fda1b6', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(due_process_clause__procedural_due_process, due_process_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(due_process_clause__procedural_due_process, individual_rights_holders).
narrative_ontology:constraint_beneficiary(due_process_clause__procedural_due_process, accused_defendants).
narrative_ontology:constraint_victim(due_process_clause__procedural_due_process, administrative_speed).
narrative_ontology:constraint_victim(due_process_clause__procedural_due_process, state_resource_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCUSED WITHOUT COUNSEL (SNARE) — Individual facing state deprivation with minimal procedural protections. Trapped by dependency on state-provided counsel or inability to afford representation. The procedural requirement exists but the extraction mechanism (cost burden, delay, complexity) remains severe. Experiences the constraint as high-theater: notice received but incomprehensible, hearings conducted but outcome predetermined by resource asymmetry.
constraint_indexing:constraint_classification(due_process_clause__procedural_due_process, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REPRESENTED DEFENDANT (TANGLED ROPE) — Individual with counsel facing state deprivation. The procedural constraint genuinely coordinates: notice, hearing, and neutral adjudication are real protections that reduce arbitrary state action. Simultaneously, the constraint extracts: litigation costs, delay, and strategic disadvantage persist. Benefits from procedure (protection against summary deprivation) coexist with costs (time, money, emotional labor). Constrained exit because challenging procedure itself is structurally difficult.
constraint_indexing:constraint_classification(due_process_clause__procedural_due_process, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL SYSTEM (ROPE) — Administers procedural due process; experiences it as coordination mechanism. The requirement to provide notice, conduct hearings, and ensure neutral adjudication solves the collective action problem of legitimacy: the state needs to appear lawful to maintain authority. Procedural due process enables state power by legitimizing deprivation through process. The judicial system has substantial agency (arbitrage) — it can modify procedures within bounds (Mathews balancing), interpret notice requirements, shape hearing standards. Net beneficiary from the procedure's legitimating function.
constraint_indexing:constraint_classification(due_process_clause__procedural_due_process, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS ORGANIZATIONS (TANGLED ROPE) — Organized advocates for procedural expansion (discovery rights, appellate review, expert witness access). The constraint both coordinates (they use procedure to challenge deprivation) and extracts (the asymmetric resource burden on challengers vs. government counsel). Over generational time, they have changed procedural norms (discovery rules, Brady obligations), but each gain is limited by state resource capacity constraints and the procedural requirement's own theater. Constrained exit because alternative dispute resolution mechanisms (administrative abolition, summary state action) would be worse.
constraint_indexing:constraint_classification(due_process_clause__procedural_due_process, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ADMINISTRATIVE EXPEDIENCY NORM (PITON) — Institutional inertia toward summary deprivation persists despite procedural requirement. Police conduct warrantless stops (suppressed via exclusionary rule but theater remains), immigration agencies conduct expedited removal (with 'credible fear' hearing that is largely performative), administrative agencies conduct benefits termination (with oral hearing that is substantially formatted in advance). The performance of procedure masks de facto summary deprivation. Theater ratio high: procedure is observed but its protective function has atrophied in domains where state speed is prioritized. Maintained through institutional habit and perceived administrative necessity rather than through conscious coordination function.
constraint_indexing:constraint_classification(due_process_clause__procedural_due_process, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, procedural due process appears as an immutable structural requirement: any legitimate legal system requires notice, opportunity to be heard, and neutral adjudication before deprivation. This appears as a law of governance itself — irreducible to contingent institutional arrangements. However, the structural data contradicts this: beneficiaries exist (the state's legitimacy interest, judicial power maintenance), suppression is active (cost burdens, delay weaponization, complexity barriers), and the constraint requires active enforcement (exclusionary rule, appellate review, Brady obligations). The engine's false summit detection will identify this perspective as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(due_process_clause__procedural_due_process, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(due_process_clause__procedural_due_process_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(due_process_clause__procedural_due_process, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(due_process_clause__procedural_due_process, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(due_process_clause__procedural_due_process, TR),
    TR >= 0.70.

:- end_tests(due_process_clause__procedural_due_process_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint requires genuine process (notice, hearing, neutral adjudicator) which reduces pure arbitrary deprivation. However, the process itself becomes a site of extraction through cost, delay, and complexity. Mathews balancing gives courts discretion to reduce process requirements when government interests are high (e.g., termination of disability benefits, immigration proceedings), and courts consistently weight government speed and efficiency heavily. The extracted value is primarily borne by accused individuals with limited resources; institutional actors and repeat players extract through procedural complexity exploitation. Suppression (0.48): Moderate-high. Multiple barriers suppress the constraint's protective function: (1) cost of legal representation; (2) complexity of procedure that advantages sophisticated actors; (3) delay weaponization (lengthy pretrial proceedings, extended administrative reviews); (4) informality and theater in high-volume contexts (police warnings, benefits termination hearings). Suppression is not total because appellate review, exclusionary rule, and Brady obligations maintain some procedural enforcement. Theater ratio (0.55): Moderate-high. Procedural formality persists in many contexts but protective function has eroded: (1) police conduct stop-and-frisk encounters outside formal procedure then exclude evidence afterwards; (2) immigration agencies hold 'credible fear' hearings that are largely formatted in advance with high removal rates (~80-90% after hearing); (3) welfare agencies conduct hearings after benefits termination (post-deprivation process insufficient per Fuentes but used anyway); (4) discovery limitations in civil rights litigation transform the hearing into theater when plaintiffs cannot access evidence. The theater has increased as administrative volume has increased—the state cannot provide meaningful process at scale, so the process becomes performative.
 *
 * PERSPECTIVAL GAP:
 *   Different power levels and exit options produce radically different classifications. The powerless trapped defendant perceives snare (high extraction, no exit, no benefit). The moderate constrained defendant perceives tangled rope (both protection and extraction, constrained exit). The institutional judicial system perceives rope (coordination mechanism, arbitrage exit). The institutional administrative agency perceives constraints (victim perspective) but with mobile exit options. The organized civil rights sector perceives tangled rope with generational progress. The piton perspective shows degraded procedure maintained through institutional inertia. The mountain perspective risks naturalizing contingent arrangements. The perspectival gaps reveal the true structure: procedure is a contested good. Beneficiaries (individuals, the accused, rights holders) experience it as protection requiring enforcement. Victims (administrative speed, police speed, state efficiency) work to reduce it. The state's relationship to procedure is paradoxical: it benefits from procedure's legitimating function (rope perspective) while working to minimize procedure's protective function (suppression and theater ratio analysis). This paradox is the constraint's signature: the state needs procedure to appear legitimate but works to hollow out procedure to maintain speed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural relationship to deprivation. The individual defendant without counsel is trapped (no exit from criminal prosecution, civil judgment, or administrative deprivation) and is the deprivation's target (d ≈ 0.90, high extraction experienced). The represented defendant is constrained (can appeal, negotiate, or pursue civil remedy but at high cost) and is nominally protected but also constrained by procedure (d ≈ 0.65, moderate extraction). The judicial system benefits from procedural legitimacy and has substantial exit/entry options (can interpret procedures broadly or narrowly, can implement Mathews balancing favorably or unfavorably) (d ≈ 0.10, low extraction experienced). Administrative agencies are targets of procedural constraint (must provide hearings, follow notice rules) but have exit options (summary procedures, expedited processes, informal adjudication) and derive benefit from speed (d ≈ 0.40, moderate extraction). Civil rights organizations benefit from procedure as challenge mechanism but bear cost burden and are constrained by resource limitations (d ≈ 0.50, balanced extraction). The analytical observer has analytical exit (no institutional stake in the outcome) (d ≈ 0.72, moderate extraction experienced as uncertainty about natural law status).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mathews_balancing_indeterminacy,
    'Does Mathews v. Eldridge balancing produce determinate procedural requirements or does it collapse into ad hoc judicial preference masquerading as constitutional necessity?',
    'Analysis of Mathews balancing across 40+ years of doctrine: correlation between stated balancing factors and actual outcomes; examination of whether private interest weight is independently applied or post-hoc rationalization; comparison of outcomes when identical factors appear across different doctrinal domains',
    'If determinate: extractiveness drops to 0.28–0.32 (rope or scaffold dominates). If indeterminate: extractiveness rises to 0.48–0.62 (snare/tangled rope dominates) because procedure becomes theater masking state discretion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mathews_balancing_indeterminacy, empirical, 'Whether Mathews balancing produces determinate procedural requirements').

omega_variable(
    kernel_identity_under_contest,
    'Is this reading''s core claim—that due process is PROCEDURAL and distinct from substance—a stable doctrinal distinction or does it collapse when substantive rights (parental control, marriage, bodily autonomy) are invoked?',
    'Historical analysis of doctrinal stability: cases where courts honored procedure while denying substantive protection (forced sterilization, custody deprivation, bodily searches); cases where courts recognized procedure as inadequate to protect fundamental interests (Cruzan, Obergefell). Examine whether procedural sufficiency can be coherently maintained across all domains of liberty.',
    'If the procedure/substance distinction is stable: this reading remains coherent (Tangled Rope with clear beneficiary/victim sets). If the distinction collapses: this reading is instrumentally used to deny substantive protection (morphs into Piton or Snare from the powerless perspective); the substantive reading foreclosed this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_under_contest, conceptual, 'Whether procedural/substance distinction is stable or collapses under doctrinal stress').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is procedural suppression (high cost, delay, complexity) structural (external barriers: filing fees, attorney costs, discovery limitations) versus internalized (the accused believes they deserve expedited deprivation, internalizes the state''s time-pressure narrative)?',
    'Post-deprivation interview data: do defendants perceive procedure as protection or as prolonged punishment? Comparative analysis of suppression effects in domains with strong counsel (capital punishment with appellate review) versus weak counsel (civil rights deprivation with summary adjudication). Exit analysis: when procedure is genuinely available, what proportion of eligible individuals invoke it versus waive it?',
    'If predominantly structural: the 0.48 suppression figure is accurate (external barriers). If internalized: suppression is higher than the metric suggests because the accused carry the barrier with them after procedural resolution. If mixed: the constraint''s extractiveness varies by counsel quality and cognitive capture state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    sibling_reading_foreclosure_status,
    'Does this procedural reading genuinely coexist with the substantive reading, or does the substantive reading logically foreclose this one within a unified framework?',
    'Doctrinal analysis: can a single constitutional theory hold both that (1) procedure + stakes balancing determines due process AND (2) some liberties are so fundamental that no process suffices? If a liberty is fundamental, does Mathews balancing become merely ceremonial? Does recognizing ''fundamental'' interests collapse back into substantive criteria that render procedure secondary?',
    'If coexists: two independent readings of the kernel, both live (expected for contested constitutional doctrine). If foreclosed: the substantive reading dominates at the fundamental liberty boundary, and this procedural reading is narrowed to non-fundamental deprivations. This omega routes to the axioms section as the foreclosure condition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'Whether procedural and substantive due process readings coexist or foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(due_process_clause__procedural_due_process, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proc_due_tr_t0, due_process_clause__procedural_due_process, theater_ratio, 0, 0.4).
narrative_ontology:measurement(proc_due_tr_t25, due_process_clause__procedural_due_process, theater_ratio, 25, 0.5).
narrative_ontology:measurement(proc_due_tr_t50, due_process_clause__procedural_due_process, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(proc_due_be_t0, due_process_clause__procedural_due_process, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(proc_due_be_t25, due_process_clause__procedural_due_process, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(proc_due_be_t50, due_process_clause__procedural_due_process, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(proc_due_su_t0, due_process_clause__procedural_due_process, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(proc_due_su_t25, due_process_clause__procedural_due_process, suppression_requirement, 25, 0.45).
narrative_ontology:measurement(proc_due_su_t50, due_process_clause__procedural_due_process, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(due_process_clause__procedural_due_process, enforcement_mechanism).
narrative_ontology:affects_constraint(due_process_clause__procedural_due_process, due_process_clause__incorporation_doctrine).
narrative_ontology:affects_constraint(due_process_clause__procedural_due_process, due_process_clause__substantive_due_process).

% DUAL FORMULATION NOTE:
% The due_process_clause kernel decomposes into three independent constraint stories reflecting three distinct readings of the constitutional text. Procedural due process (this story) focuses on process scaling by stakes. Substantive due process (sibling story) focuses on content constraints on state authority. Incorporation doctrine (sibling story) focuses on the federalism mechanism binding the states. Each reading has its own epsilon value (procedural: 0.38, substantive: 0.55+, incorporation: 0.30), its own perspectival structure, and its own dominating type. The network links show how the readings affect each other: incorporation doctrine provides the structural mechanism by which due process constrains states; substantive due process creates pressure on procedural reading at fundamental liberty boundaries; procedural reading attempts to remain neutral on what liberties are fundamental.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(due_process_clause__procedural_due_process, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
