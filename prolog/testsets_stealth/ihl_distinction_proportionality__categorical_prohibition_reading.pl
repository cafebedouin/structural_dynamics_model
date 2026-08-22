% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Categorical Prohibition of Lethal Autonomous Weapons (Martens Clause Reading)
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the
 *   ihl_distinction_proportionality kernel: the categorical prohibition
 *   reading, under which the Martens Clause's principles of humanity and
 *   dictates of public conscience prohibit lethal autonomous weapons as a
 *   class — regardless of technical performance — because crossing the
 *   threshold of machine-decided killing violates human dignity per se. The
 *   constraint under classification is the prohibition norm itself as this
 *   reading authors it. It possesses a genuine coordination function
 *   (preventing an autonomous arms race and preserving a human locus for
 *   killing decisions, a collective good no state could secure unilaterally)
 *   AND an asymmetric burden distribution: states and industries holding
 *   advanced autonomy programs surrender the most option-space, while states
 *   without such programs and anti-militarist civil society gain without
 *   paying. The expected-delta phrase 'military technological advantage' is
 *   carried here by its actor seats, advanced_military_powers and
 *   defense_autonomy_industry. The sibling readings (human_agency_reading,
 *   outcomes_based_reading) are separate constraints in the same family with
 *   lower epsilon because they restrict less option-space; they are linked
 *   via network edges and are not described further here. Claim and metrics
 *   are authored independently: claimed_type is tangled_rope because the
 *   structure holds both coordination and asymmetric, enforcement-dependent
 *   burden; the metrics describe the norm's actual operation through 2026.
 *
 * KEY AGENTS:
 *   - advanced_military_powers: primary target (institutional/constrained) — bears the class-wide foreclosure of autonomous-weapons option-space
 *   - defense_autonomy_industry: secondary target (powerful/constrained) — product segment eliminated by the ban
 *   - anti_militarist_civil_society: primary beneficiary (organized/identity_locked) — mission and identity constituted by the ban cause
 *   - states_without_laws_capability: primary beneficiary (organized/mobile) — capability freeze acquired at zero development cost
 *   - icrc: agenda-setter with secondary beneficiary position (institutional/identity_locked) — frames the humanity/public-conscience authority
 *   - un_ccw_gge_process: agenda-setter (institutional/constrained) — administers the consensus-bound forum
 *   - fielded_military_personnel: dual-positioned (moderate/constrained) — loses the machine risk-absorption option, retains the killing-decision locus
 *   - civilian_populations_conflict_zones: excluded (powerless/trapped) — the claimed protectees hold no seat
 *   - ihl_legal_academy: analytical observer — authors the interpretive frameworks all readings draw on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.76).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.63).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Categorical Prohibition of Lethal Autonomous Weapons (Martens Clause Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '44cb47f6-3d4d-46e2-a13d-071186743f1a').
narrative_ontology:cs_kernel_codification('44cb47f6-3d4d-46e2-a13d-071186743f1a', fixed_text).
narrative_ontology:cs_authority_grounding('44cb47f6-3d4d-46e2-a13d-071186743f1a', distributed).
narrative_ontology:cs_reading_relation('44cb47f6-3d4d-46e2-a13d-071186743f1a', ihl_distinction_proportionality__human_agency_reading, influences).
narrative_ontology:cs_reading_relation('44cb47f6-3d4d-46e2-a13d-071186743f1a', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('44cb47f6-3d4d-46e2-a13d-071186743f1a', foundational, machine_decided_killing_dignity_violation_per_se).
narrative_ontology:cs_axiom_status(machine_decided_killing_dignity_violation_per_se, holdable).
narrative_ontology:cs_axiom_grounding('44cb47f6-3d4d-46e2-a13d-071186743f1a', machine_decided_killing_dignity_violation_per_se, deontological).
narrative_ontology:cs_axiom('44cb47f6-3d4d-46e2-a13d-071186743f1a', foundational, public_conscience_generates_binding_prohibitions).
narrative_ontology:cs_axiom_status(public_conscience_generates_binding_prohibitions, holdable).
narrative_ontology:cs_axiom_grounding('44cb47f6-3d4d-46e2-a13d-071186743f1a', public_conscience_generates_binding_prohibitions, conventional).
narrative_ontology:cs_reference_frame('44cb47f6-3d4d-46e2-a13d-071186743f1a', martens_public_conscience_unconditional_floor).
narrative_ontology:cs_drift_state('44cb47f6-3d4d-46e2-a13d-071186743f1a', contemporary_post_unga_resolution_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('44cb47f6-3d4d-46e2-a13d-071186743f1a', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_without_laws_capability).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_military_powers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, defense_autonomy_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, icrc).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, fielded_military_personnel).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, fielded_military_personnel).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, martens_clause_evolutionary_authority).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_per_se_in_warfare).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with active lethal-autonomy development and fielding programs (United States, Russia, China, Israel, and allies). The prohibition would foreclose an entire weapons class they are best positioned to dominate, and they carry the largest share of the burden. They resist through consensus blocking in the CCW and by organizing around technology-neutral language. They cannot leave the IHL normative order, and stigmatization costs follow them regardless of ratification, though they retain the practical ability to develop outside any future treaty's bounds.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_military_powers, payer,
    institutional, generational, constrained, global).

% Defense contractors and technology firms building target-selection and engagement autonomy. A class-wide ban eliminates this market segment entirely. Pivoting to civilian AI is possible in principle, but their clearances, revenue base, and institutional identity are bound to military autonomy programs, so the pivot is costly and partial.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, defense_autonomy_industry, payer,
    powerful, biographical, constrained, global).

% The NGO coalition behind the Campaign to Stop Killer Robots and allied arms-division organizations. Their funding, membership, and organizational purpose are constituted by the ban cause; abandoning it would dissolve the coalition rather than reposition it. They gain mission fulfillment and normative standing as the prohibition advances.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, identity_locked, global).

% Middle powers (Austria, Costa Rica, Brazil, and coalition partners) leading UN resolutions for a ban. They bear no development cost because they could not build the banned capability anyway; prohibition freezes a capability distribution in their favor. Their endorsement is diplomatically cheap and purchases humanitarian standing, and they can reposition within coalitions at low cost.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_without_laws_capability, beneficiary,
    organized, generational, mobile, global).

% Drives the principles-of-humanity and public-conscience framing and formally calls for prohibition of autonomous weapons. Its mandate authority and moral standing grow as the norm advances, and it cannot abandon the humanitarian position without dissolving the institutional identity that constitutes it.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, icrc, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, icrc, beneficiary).

% The Convention on Certain Conventional Weapons Group of Governmental Experts, which administers the state conversation on autonomous weapons. It operates by consensus, so any major power can block outcomes. It has met for roughly a decade without producing a binding instrument, which critics describe as a substitute for action rather than a path to it.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, un_ccw_gge_process, agenda_setter,
    institutional, biographical, constrained, global).

% Soldiers who would operate or face autonomous systems. The prohibition removes a force-protection option (machines absorbing lethal risk in their place) while preserving their position as the locus of killing decisions, which some experience as moral protection and others as imposed risk. They hold no seat in the negotiation that decides this.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, fielded_military_personnel, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, fielded_military_personnel, beneficiary).

% People in conflict zones whom the prohibition claims to protect and whom autonomous systems would target. They are represented in none of the negotiating forums; which reading of the law prevails determines what may be used against them, but they deliberate on none of the readings.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, civilian_populations_conflict_zones, excluded,
    powerless, immediate, trapped, regional).

% International lawyers and military ethicists who author the interpretive frameworks all three readings draw on, debate whether the Martens Clause generates binding prohibitions, and track state practice for evidence of crystallization. They collect no rents and bear no burdens from the outcome.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_legal_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, states_without_laws_capability).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__categorical_prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state restraint on lethal autonomy: prevents a competitive dynamic in which states shift killing decisions to machines to shed the costs and risks of human-decided killing, and preserves a shared boundary — that a human bears each lethal decision — which no state could maintain unilaterally while rivals automated.
% TRANSFER_FUNCTION: Moves technological option-space from states and firms holding advanced autonomy programs (who may not build or field the class) to states and movements without such programs (who gain a frozen capability distribution and normative standing); moves definitional authority over lawful killing toward humanitarian institutions.
% ABSENT_VOICES: Civilian populations in conflict zones — the people the prohibition claims to protect and the people autonomous systems would target — have no seat anywhere in the process. Technical communities able to testify to what verifying a development-stage ban would actually require are also largely outside the room.
% DISAPPEARANCE_RATIONALE: The parties dispute it. Humanitarian and laggard-state seats hold that the prohibition is the load-bearing moral floor: remove it and the performance-based framing faces no counterweight, autonomous arms-race restraint collapses, and machine-decided killing normalizes. Advanced-power seats hold that nothing rearranges: the norm has never bound them, their programs proceed regardless, and the campaign's disappearance would merely reveal the status quo. Both descriptions are internally coherent; the dispute is itself the constraint's contested character.
% FOUNDING_PROBLEM: Battlefield automation threatened to remove human judgment from lethal decisions faster than IHL's technology-neutral rules could respond: read as performance-contingent, the existing distinction and proportionality framework appeared to permit machine killing provided it worked, and no forum was addressing the threshold being crossed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the CCW guiding principles, adopted by consensus including the advanced military powers that oppose the categorical form, affirm that human responsibility for lethal decisions must be retained; joint ICRC-UN leadership calls in 2023 and public statements by former senior commanders attest the accountability gap is real. What no corroborating source outside the beneficiary coalition attests is the categorical form itself — the corroborating seats confirm the founding problem while disputing this reading's answer to it, which is the honest provenance for a contested reading.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, contested).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.76 at interval end) because the reading forecloses an entire technology class — the broadest option-space restriction of the three family readings — and its burden falls on the states best positioned to develop the banned capability. Suppression (0.63) is structural-diplomatic rather than coercive: the norm operates through stigmatization, resolution majorities, and alliance pressure rather than sanctions or force, and its persistence requires continuously reproduced public-conscience authority. Suppression is authored as a raw structural property; the engine, not this story, scales extractiveness by directionality and scope. Theater (0.45, below the Goodhart line but rising steadily) reflects the CCW process's decade of consensus-blocked sessions that maintain the appearance of progress while the binding outcome is deferred — the humanitarian core of the campaign remains sincere while the process shell grows performative. Accessibility collapse is moderate (0.40): within the reading's own frame, performance-based alternatives become unspeakable, but across the international system the human-agency and outcomes-based alternatives remain fully live. Resistance is high (0.72): every major military power blocks consensus and counter-organizes around technology-neutral language. All three measurement series share one time grid (2012-2026, biennial); the 2026 points are authored as projections. Suppression_requirement is tracked because this story's dynamic IS enforcement-capacity build-up: from no process (2012) through the GGE mandate (2017) to UNGA resolutions (2023-) with growing majorities.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the advanced-powers seat the reading operates as confiscation of a military option they are uniquely positioned to exploit, enforced by a normative order they did not consent to and cannot exit — maximal effective burden. From the laggard-state seat the same text is a free capability freeze: maximal gain at zero development cost, which is why their endorsement is cheap and near-unanimous. From the civil-society seat the prohibition is a moral floor whose value is intrinsic, not comparative. The dual-positioned soldier seat splits internally: denied machine risk-absorption (cost), preserved as the locus of killing decisions (benefit). Coalition note: the target seats partially coalition — joint major-power statements against a ban — which raises their effective resistance above what any single state could mount. The engine computes these per-seat types from power, exit, and declared position; this reading's own claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to real flows: anti_militarist_civil_society gains mission fulfillment and normative authority; states_without_laws_capability gain a frozen capability distribution they could not contest by development. The victim declarations map to the seats bearing the foreclosure: advanced_military_powers surrender the largest option-space; defense_autonomy_industry loses the market segment. Exit modulation: identity-locked civil society cannot abandon the cause without dissolving itself, holding its directionality near the full-beneficiary end; mobile laggard states reposition cheaply, holding theirs low; constrained advanced powers cannot exit the IHL order and cannot fully escape stigmatization, holding theirs near the full-target end despite their practical ability to develop outside treaty bounds. ICRC's dual position (agenda-setter with beneficiary secondary role) places it low-mid. No directionality overrides were authored: the declared beneficiary/victim structure plus exit options already produce the correct per-seat relationships, and the override key (power atom) is too coarse to separate the two institutional seats — advanced powers versus ICRC/UN process — that share it. Receipt surface: the material gain of the prohibition, the frozen capability distribution, demonstrably accrues to the laggard-state seat, so gain_flow names it; civil-society gains are real but non-material, and no single seat captures the normative-authority gain. Fixing cost is prohibitive: no seat can cheaply remove the norm once embedded in resolution practice and customary-law claims; even the advanced powers find full repudiation costlier than continued containment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — battlefield automation outrunning human judgment in lethal decisions — is live, so the mismatch consumer finds no dead-problem flag (status=live crossed with verdict=contested). The classification prevents mislabeling in both directions: reading the prohibition as pure coordination would erase the capability-freeze asymmetry that laggard states capture through it; reading it as pure extraction would erase the genuine arms-race-prevention and dignity-stigmatization functions that motivate its adherents independent of any capability distribution. The piton risk is real but prospective, not current: if lethal autonomy deploys at scale and the norm fails to bind, the annual resolution cycle could decay into theatrical maintenance of a mandate nothing enforces — the theater series (0.12 to 0.45) is the early-warning trace for exactly that drift. Mandatrophy is not declared: the mandate has not outlived its function; it has not yet fully exercised it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the ihl_distinction_proportionality kernel (categorical_prohibition_reading). Would instantiating a sibling reading — human_agency_reading (irreducible human judgment at the moment of force) or outcomes_based_reading (legality tracks demonstrated performance) — change the structural classification, and where exactly is the disagreement located?',
    'Treaty negotiation outcome or crystallized state practice: a binding instrument adopting categorical language hardens this reading; instruments adopting meaningful-human-control or performance language would make a sibling the operative constraint, requiring re-authoring of epsilon and victim sets against that sibling.',
    'The human_agency sibling bans only machine-decided decisions and permits supervised automation — lower epsilon, smaller victim set. The outcomes_based sibling bans no technology class at all — epsilon near coordination cost only, no class victims. The categorical reading carries the family''s highest epsilon because it forecloses the entire option-space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the IHL autonomous-weapons kernel is the operative constraint.').

omega_variable(
    martens_clause_normative_force,
    'Does the Martens Clause''s reference to principles of humanity and dictates of public conscience generate new binding prohibitions, or does it operate only as an interpretive principle for existing rules?',
    'ICJ or senior court treatment of the Clause as an independent source of obligation; systematic analysis of state practice and opinio juris invoking the Clause to ground duties not otherwise codified.',
    'If the Clause is interpretive only, the categorical reading lacks its claimed legal foundation and reduces to advocacy — its reach and effective suppression drop sharply; if generative, the prohibition can bind non-consenting states through custom and the victim seats'' constrained exit tightens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_normative_force, conceptual, 'Whether the reading''s legal foundation is a source of law or a hermeneutic rule.').

omega_variable(
    customary_law_crystallization,
    'Is the categorical prohibition crystallizing into customary international law binding all states, or does it remain soft law binding only endorsing states?',
    'Track UNGA resolution majorities, national moratoria and implementing legislation, military-doctrine statements, and the abstention patterns of advanced military powers across successive years.',
    'Customary status would extend the prohibition''s reach to non-consenting advanced powers, raising their effective burden without their consent; soft-law status confines the burden to endorsers and leaves the primary target seats formally outside.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_crystallization, empirical, 'Customary status of the categorical prohibition.').

omega_variable(
    capability_freeze_vs_protection,
    'Is the prohibition''s primary operative effect civilian protection, or a freeze of the military capability distribution that states without lethal-autonomy programs could not contest by development?',
    'Compare protection outcomes under restrictive regimes with deployment behavior of advanced powers; test whether endorsement intensity correlates with capability distance from the frontier rather than with conflict exposure.',
    'If the freeze dominates, the beneficiary structure is more distributional than the humanitarian framing suggests and part of the coordination function is cover; if protection dominates, the genuine-coordination half of the structure strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_freeze_vs_protection, empirical, 'Humanitarian protection versus capability-distribution freeze as the operative effect.').

omega_variable(
    rd_verification_feasibility,
    'Can a categorical ban on lethal-autonomy development be verified at all, given that the underlying research is dual-use and covertible?',
    'Arms-control verification analysis: what inspection, telemetry, or supply-chain regime could distinguish prohibited autonomy research from permitted conventional automation; historical base rates from chemical and biological weapons verification regimes.',
    'If verification is infeasible, enforcement concentrates on the already-compliant while capable states develop outside treaty bounds — the ban''s suppressive apparatus becomes performance aimed at its primary targets, and the effective burden shifts to domestic industries in endorsing states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rd_verification_feasibility, empirical, 'Verifiability of a development-stage ban on autonomous weapons.').

omega_variable(
    dignity_threshold_per_se_status,
    'Is the dignity violation of machine-decided killing per se — a threshold crossing regardless of system properties — or contingent on properties such as accountability gaps, discrimination failure rates, or human-control architecture?',
    'Philosophical and legal elaboration: whether dignity-based arguments survive the stipulation of a system that never errs, always attributes responsibility, and operates under continuous human veto; comparison with accepted automated defensive systems.',
    'If dignity violation is contingent, the categorical reading collapses toward the human_agency sibling and its epsilon falls substantially; if per se, the performance-invariant axiom holds and the full class-wide foreclosure stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_threshold_per_se_status, conceptual, 'Whether the per-se dignity threshold is conceptually stable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 2012, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2012, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2012, 0.12).
narrative_ontology:measurement_basis(ihl__tr_t2012, observed).
narrative_ontology:measurement(ihl__tr_t2014, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2014, 0.16).
narrative_ontology:measurement_basis(ihl__tr_t2014, observed).
narrative_ontology:measurement(ihl__tr_t2016, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement_basis(ihl__tr_t2016, observed).
narrative_ontology:measurement(ihl__tr_t2018, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement_basis(ihl__tr_t2018, observed).
narrative_ontology:measurement(ihl__tr_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2020, 0.33).
narrative_ontology:measurement_basis(ihl__tr_t2020, observed).
narrative_ontology:measurement(ihl__tr_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement_basis(ihl__tr_t2022, observed).
narrative_ontology:measurement(ihl__tr_t2024, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(ihl__tr_t2024, observed).
narrative_ontology:measurement(ihl__tr_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2026, 0.45).
narrative_ontology:measurement_basis(ihl__tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2012, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2012, 0.5).
narrative_ontology:measurement_basis(ihl__be_t2012, observed).
narrative_ontology:measurement(ihl__be_t2014, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2014, 0.54).
narrative_ontology:measurement_basis(ihl__be_t2014, observed).
narrative_ontology:measurement(ihl__be_t2016, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement_basis(ihl__be_t2016, observed).
narrative_ontology:measurement(ihl__be_t2018, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement_basis(ihl__be_t2018, observed).
narrative_ontology:measurement(ihl__be_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(ihl__be_t2020, observed).
narrative_ontology:measurement(ihl__be_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2022, 0.7).
narrative_ontology:measurement_basis(ihl__be_t2022, observed).
narrative_ontology:measurement(ihl__be_t2024, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2024, 0.73).
narrative_ontology:measurement_basis(ihl__be_t2024, observed).
narrative_ontology:measurement(ihl__be_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2026, 0.76).
narrative_ontology:measurement_basis(ihl__be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2012, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2012, 0.25).
narrative_ontology:measurement_basis(ihl__su_t2012, observed).
narrative_ontology:measurement(ihl__su_t2014, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2014, 0.3).
narrative_ontology:measurement_basis(ihl__su_t2014, observed).
narrative_ontology:measurement(ihl__su_t2016, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2016, 0.36).
narrative_ontology:measurement_basis(ihl__su_t2016, observed).
narrative_ontology:measurement(ihl__su_t2018, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement_basis(ihl__su_t2018, observed).
narrative_ontology:measurement(ihl__su_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement_basis(ihl__su_t2020, observed).
narrative_ontology:measurement(ihl__su_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2022, 0.54).
narrative_ontology:measurement_basis(ihl__su_t2022, observed).
narrative_ontology:measurement(ihl__su_t2024, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2024, 0.59).
narrative_ontology:measurement_basis(ihl__su_t2024, observed).
narrative_ontology:measurement(ihl__su_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2026, 0.63).
narrative_ontology:measurement_basis(ihl__su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'IHL governs autonomous weapons' decomposes into three readings of one kernel with distinct epsilon values and distinct beneficiary/victim structures, per the epsilon-invariance principle. This file is the categorical_prohibition_reading — the highest-epsilon member, banning the entire technology class. The human_agency_reading restricts only machine-decided decision points (permits supervised automation); the outcomes_based_reading restricts nothing but underperformance (no class victims). The shared upstream text (Geneva/Additional Protocol I plus the Martens Clause) is cited by all three; this reading reads the Clause as generating class-wide prohibition, the human-agency reading as requiring a human decision point, and the outcomes reading as subordinate to technology-neutral performance obligations. Edges run from this file to both siblings because categorical codification would structurally shrink both siblings' domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
