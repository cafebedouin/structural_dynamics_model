% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual-Practice Equilibrium: Partitioned Legitimacy (State Administrative / Traditional Ritual)
 *   domain: political/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the DUAL-PRACTICE-EQUILIBRIUM reading of the
 *   kernel 'legitimacy of practice standardization.' It claims that practice
 *   legitimacy can and does partition along domain lines: state authority
 *   governs public/administrative domains (Gregorian calendar for taxes,
 *   Western business dress for state employment, standardized metrics for
 *   law), while traditional authority governs private/ritual domains (lunar
 *   calendar for harvest festivals, traditional dress for home ceremonies,
 *   customary kinship rules). The reading asserts this partition is
 *   STRUCTURALLY STABLE—not a temporary compromise awaiting resolution, but a
 *   permanent equilibrium in which both authorities remain legitimate within
 *   their domains, compliance is STRATEGIC rather than internalized, and no
 *   party expects convergence. The constraint is TANGLED ROPE: it provides
 *   genuine coordination (state standardization solves large-scale problems;
 *   traditional authority solves community-cohesion problems) AND asymmetric
 *   extraction (citizens must learn both systems; minorities who do not fit
 *   either get no legitimacy seat). The sibling readings offer different
 *   accounts: the ENDOGENOUS-DISPLACEMENT reading claims practice change
 *   becomes legitimate when communities voluntarily adopt it (no partition,
 *   just evolution); the EXOGENOUS-OVERRIDE reading claims legitimate change
 *   requires state decree for modernization and collective benefit (no
 *   partition, state authority alone). This reading disagrees with both: it
 *   holds that NEITHER endogenous drift NOR state mandate alone confers
 *   legitimacy—legitimacy REQUIRES domain partition, and stability REQUIRES
 *   that neither authority tries to monopolize both domains.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus — institutional agenda-setter, monopolizes public-domain rule-making, benefits from standardized practice coordination
 *   - traditional_authority_gatekeepers — institutional agenda-setter, retains private-domain rule-making, benefits from state recognition of boundary
 *   - citizens_navigating_dual_regime — moderate-power payer and beneficiary, must code-switch between legitimacy systems, bear cognitive and social cost
 *   - cultural_minorities_with_non_aligned_practice — powerless payer, identity-locked, excluded from both legitimacy seats
 *   - merchants_and_professionals — powerful beneficiary, arbitrage exit options, exploit both domains for efficiency
 *   - religious_and_ethnic_minorities — moderate power excluded, would challenge the binary partition itself
 *   - reformist_intellectuals — organized observers, provide frameworks for stability or instability interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.51).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual-Practice Equilibrium: Partitioned Legitimacy (State Administrative / Traditional Ritual)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political/institutional").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '0fa5a483-1dad-492b-bf79-3120e486c2d9').
narrative_ontology:cs_kernel_codification('0fa5a483-1dad-492b-bf79-3120e486c2d9', distributed).
narrative_ontology:cs_authority_grounding('0fa5a483-1dad-492b-bf79-3120e486c2d9', practice).
narrative_ontology:cs_interpretation_layer_present('0fa5a483-1dad-492b-bf79-3120e486c2d9').
narrative_ontology:cs_reading_relation('0fa5a483-1dad-492b-bf79-3120e486c2d9', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fa5a483-1dad-492b-bf79-3120e486c2d9', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('0fa5a483-1dad-492b-bf79-3120e486c2d9', foundational, practice_legitimacy_is_domain_partitioned).
narrative_ontology:cs_axiom_status(practice_legitimacy_is_domain_partitioned, holdable).
narrative_ontology:cs_axiom_grounding('0fa5a483-1dad-492b-bf79-3120e486c2d9', practice_legitimacy_is_domain_partitioned, conventional).
narrative_ontology:cs_axiom('0fa5a483-1dad-492b-bf79-3120e486c2d9', foundational, dual_authority_coexistence_is_stable_equilibrium).
narrative_ontology:cs_axiom_status(dual_authority_coexistence_is_stable_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('0fa5a483-1dad-492b-bf79-3120e486c2d9', dual_authority_coexistence_is_stable_equilibrium, instrumental).
narrative_ontology:cs_reference_frame('0fa5a483-1dad-492b-bf79-3120e486c2d9', dual_legitimacy_partition_framework).
narrative_ontology:cs_drift_state('0fa5a483-1dad-492b-bf79-3120e486c2d9', contemporary_modernization_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fa5a483-1dad-492b-bf79-3120e486c2d9', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_gatekeepers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_navigating_dual_regime).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cultural_minorities_with_non_aligned_practice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_navigating_dual_regime).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, merchants_and_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces standardized practices (Gregorian calendar, Western business attire, state-approved metrics) within tax, legal, educational, and bureaucratic domains. Justifies standardization as necessary for coordinating large-scale societies, ensuring fiscal stability, and facilitating international commerce. Gains legitimacy from its claimed monopoly on public-domain ordering; loses coordination authority if private domains fracture into incompatible standards.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Maintain authority over ritual, festival, kinship, and agricultural practices within private/family/community domains (lunar calendar for harvest rites, traditional dress for ceremonies, customary inheritance rules). Preserve legitimacy by controlling spaces state authority claims not to govern. Benefit from the state's recognition of domain boundaries and from the coordination value of having two competing authorities rather than state monopoly.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_gatekeepers, agenda_setter,
    powerful, civilizational, mobile, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_gatekeepers, beneficiary).

% Must learn, maintain, and switch between two sets of legitimacy standards depending on domain: Gregorian calendar for taxes and school, lunar calendar for ancestral rites; Western business suits for government offices, traditional dress for home ceremonies. Gain the benefit of having both systems available (can honor tradition at home while participating in state coordination), but bear the cognitive and social cost of code-switching. Exit options are constrained—refusing either domain incurs penalties (tax non-compliance, social exclusion).
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_navigating_dual_regime, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_navigating_dual_regime, beneficiary).

% Possess ritual, calendar, or dress practices that do not align cleanly with either state or dominant-tradition authority (e.g., syncretic religions, immigrant communities, historically oppressed groups whose practices the traditional authority does not recognize as 'authentic'). Face enforcement pressure from state domain (must use Gregorian calendar for all documents) and delegitimization from traditional authority domain (told their practices are not truly traditional). Identity fusion with non-aligned practices makes exit impossible; no seat recognizes their legitimacy.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cultural_minorities_with_non_aligned_practice, payer,
    powerless, biographical, identity_locked, national).

% Gain efficiency gains from dual-system recognition: can use standardized (state) practices for commerce and contracts, reserving traditional practices for client relations and cultural signaling. High exit options—can relocate, switch business models, or operate across borders where different domains apply. Benefit from the regime without bearing suppression costs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, merchants_and_professionals, beneficiary,
    powerful, biographical, arbitrage, global).

% Are not represented in the negotiation between state and dominant traditional authority over domain boundaries. Would argue for recognition of their own ritual calendars, dress codes, and authority structures, but are treated as either state subjects (and thus required to adopt state-standard practices in public domains) or as not-yet-modern (and thus expected to abandon non-dominant-tradition practices entirely). Their voices would challenge the binary partition itself.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, religious_and_ethnic_minorities, excluded,
    moderate, biographical, constrained, national).

% Analyze the dual-regime stability and argue about whether the partition is legitimate. Some defend it as pragmatic pluralism; others argue it masks state domination of the public sphere and traditional patriarchy of the private sphere. Their role is analytical rather than operative—they do not control either domain's enforcement but provide frameworks through which stability or instability is interpreted.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, reformist_intellectuals, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes large-scale societies by creating a predictable public domain (standardized calendars, metrics, legal procedures enabling interstate commerce and centralized taxation) while preserving micro-level social cohesion through recognized private-domain authority (kinship, ritual, local custom remain authoritative for family and community affairs). Solves the problem that a state cannot simultaneously enforce universal practice standards everywhere AND preserve the local knowledge and legitimacy that communities draw from traditional authority.
% TRANSFER_FUNCTION: Moves authority and the rents of authority-exercise from traditional gatekeepers (in domains the state monopolizes) and from citizens (who lose capacity to choose practices) to the state in public domains, and from state oversight to traditional authorities in private domains. Transfers compliance cost from both groups to citizens who navigate both domains.
% ABSENT_VOICES: Minorities whose practices do not align with either state standardization or dominant-tradition recognition (syncretic religions, immigrant communities, historically suppressed traditions) would object to the partition as excluding them from legitimacy in both domains. Cosmopolitan citizens who reject the bifurcation would argue for either universal state standards or universal traditional freedom. Their exclusion is structural to the partition itself.
% DISAPPEARANCE_RATIONALE: If the dual-domain partition vanished, societies would face an immediate coordination crisis: either the state would attempt to monopolize all domains (citizens would face total state standardization, eroding community authority and local legitimacy), or fragmentation would occur (private domains would diverge too far for large-scale coordination, state capacity to collect taxes and enforce law would degrade). The partition is what allows both coordination and autonomy to coexist.
% FOUNDING_PROBLEM: Modernizing states sought to standardize practices (calendars, metrics, legal procedures) across large territories to enable taxation, commerce, and administrative control, but faced resistance from communities whose legitimacy sources (traditional authorities, ritual cycles, customary law) were embedded in non-standardized practices. The founding problem was how to modernize without destroying local social order.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of institutional change (Scott, Migdal, Pierson) document this as an ongoing dynamic in postcolonial and modernizing states. Anthropological and historical studies of Japan (Meiji era dual calendar use), India (secular-law vs. personal-law partition), China (state holiday vs. traditional festival), and Ottoman reformation all show the problem remaining live across centuries. Corroborated from outside benefiting parties: international development literature recognizes domain partitioning as a persistent strategy in states under pressure to modernize while preserving legitimacy.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the dual regime requires compliance with two legitimacy systems and penalizes those who cannot align with either, but it is not maximum extraction because both authorities claim genuine coordination functions (state standardization enables large-scale coordination; traditional authority enables community cohesion). Suppression is moderate (0.51) because the partition is maintained partly by enforcement (state penalties for tax non-compliance, community pressure for ritual conformance) but also by the internalized belief—in both authority-holders and many citizens—that the partition is LEGITIMATE rather than coercive. Theater is moderate (0.42) because both authorities genuinely provide coordination services, but enforcement increasingly defends the PARTITION ITSELF (keeping state out of private domains, keeping private authority out of public domains) rather than defending the substance of the rules. The measurement trajectory shows extractiveness and theater rising slowly to a plateau around t=25, then stabilizing—this is the maturation of the dual regime: initial enforcement costs drop as compliance becomes habitual, but the partition itself requires constant maintenance to prevent either authority from encroaching. Suppression requirement rises and stabilizes similarly: the system requires active enforcement of BOUNDARIES more than enforcement of rule-content once citizens internalize the partition. The time grid is shared across all three metrics at nine points.
 *
 * PERSPECTIVAL GAP:
 *   The state and traditional authorities view the constraint identically (as a legitimate, stable, mutually beneficial partition); citizens embedded in the partition mostly accept it (as pragmatic and culturally necessary); but minorities and reformists see extraction and exclusion. The engine should compute the same type (tangled rope) from all seats, but with dramatically different directionality: beneficiaries perceive coordination; payers perceive extraction; excluded parties perceive impossibility.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus has d near the beneficiary end (low d, ~0.2): it collects rents from monopolizing public-domain rule-making, has infinite exit options (it defines what exit even means), and faces no resistance from citizens who accept the partition as legitimate. Traditional gatekeepers have d symmetric-to-beneficiary (~0.3): they retain authority in private domains and benefit from state recognition of boundaries, but they have mobile exit (can shift their authority across families/communities), face moderate resistance (younger generations testing boundaries), and compete with state authority for legitimacy. Citizens navigating the dual regime sit near symmetric (~0.5): they gain coordination services from both authorities but bear code-switching costs and lose the option to choose a single legitimacy system. Minorities with non-aligned practices have d near the target end (high d, ~0.85): they bear suppression from BOTH authorities, have identity-locked exit (cannot abandon the practices that define them), face active resistance (delegitimization), and cannot arbitrage between domains because no seat recognizes their legitimacy. The structural asymmetry—between those embedded in the partition and those excluded by it—is what makes this tangled rope rather than rope: the coordination function is real, but who benefits and who pays is radically unequal.
 *
 * MANDATROPHY ANALYSIS:
 *   The dual-regime partition APPEARS to be a tangled rope that could be resolved (by either state monopoly or universal traditional freedom), but mandatrophy analysis suggests the founding problem (state modernization while preserving local legitimacy) is NOT dead—it remains live and unresolved across centuries. The constraint persists not because the problem is solved but because NEITHER sibling reading (endogenous drift or state override alone) can solve it without destroying either state capacity or community legitimacy. The partition is what allows both to coexist. Mandatrophy would be if the founding problem disappeared (all communities voluntarily standardized, or the state abandoned modernization) while the partition persisted—but the measurements show extractiveness and enforcement stabilizing at a non-trivial level, not degrading toward pure performance. The constraint is not a zombie; the founding problem is still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalized_vs_strategic_compliance,
    'Do citizens accept the dual-regime partition as LEGITIMATE (internalized), or do they comply STRATEGICALLY (because exit costs are too high)?',
    'Anthropological study of code-switching narratives: interview citizens in both domains about whether they experience the partition as natural/necessary (internalized) or imposed/constraining (strategic). Exit-scenario testing: offer citizens costless exit and measure defection.',
    'If compliance is internalized, the suppression metric understates the regime''s stability (the regime is self-perpetuating even without enforcement). If strategic, suppression understates the fragility (enforcement weakening would trigger rapid re-alignment). The classification as tangled rope holds either way; the trajectory changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internalized_vs_strategic_compliance, empirical, 'Whether the dual-regime partition is accepted as legitimate or endured as coerced.').

omega_variable(
    partition_stability_across_generations,
    'Does the partition reproduce itself across generations, or is each generation re-negotiating the boundary between state and traditional authority?',
    'Cohort analysis of practice alignment over time: measure the age distribution of code-switchers vs. single-domain citizens. Longitudinal tracking of individuals across cohorts.',
    'Stable reproduction suggests the partition is self-maintaining (genuine equilibrium). Generational drift suggests the partition is contested and fragile (boundary erosion is endemic). Either way, the extractiveness measurement holds, but piton dynamics (degraded function maintained theatrically) would appear if drift is unidirectional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability_across_generations, empirical, 'Whether dual-regime practice is generationally stable or subject to ongoing re-negotiation.').

omega_variable(
    kernel_reading_alternative_framing,
    'This reading assumes the partition is LEGITIMATE (domain-partitioned authority is a defensible equilibrium). The ENDOGENOUS-DISPLACEMENT reading frames the same empirical pattern as ILLEGITIMATE (the partition is temporary drag on inevitable practice evolution). Are these competing readings, or different empirical claims about the same phenomenon?',
    'Separating the readings requires asking: would adoption of the endogenous reading''s normative premise (legitimacy comes from voluntary evolution, not domain partition) change the empirical classification of this constraint''s operation? YES—the endogenous reading would reclassify the partition as SNARE (dominating traditional authority to force state standardization), not tangled rope (legitimate coexistence). This is a framework disagreement, not an empirical disagreement. The readings COEXIST because they inhabit different normative frameworks about what makes a partition legitimate.',
    'If the readings COEXIST, this constraint is part of a kernel family where both readings are live options held by different parties and traditions. If one FORECLOSES the other, the kernel is genuinely contested and resolution-directed. The coexist framing preserves the constraint as belonging to a family of readings rather than a single contested fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Whether this reading and its siblings coexist as different frameworks or foreclose each other.').

omega_variable(
    minority_exclusion_as_structural_feature,
    'Is the exclusion of minorities with non-aligned practices a BUG in the partition (a failure to extend domain boundaries to cover all practice types) or a FEATURE (the partition fundamentally cannot accommodate three or more legitimacy sources)?',
    'Policy experiments: test whether adding a third legitimacy seat (e.g., recognizing syncretic religions as having valid private-domain authority) destabilizes the equilibrium or extends it. Historical case studies of societies that attempted multi-source legitimacy partitions.',
    'If a bug, the constraint could be reformed to be less extractive for minorities while preserving the state/traditional partition. If a feature, the constraint is structurally EXCLUSIVE—the partition works BECAUSE it limits legitimacy sources to two. If a feature, fixing minority exclusion requires abandoning the partition itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_exclusion_as_structural_feature, conceptual, 'Whether minority exclusion is a failure of the partition or inherent to its structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dpe_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(dpe_tr_t0, observed).
narrative_ontology:measurement(dpe_tr_t5, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(dpe_tr_t5, observed).
narrative_ontology:measurement(dpe_tr_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(dpe_tr_t10, observed).
narrative_ontology:measurement(dpe_tr_t15, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(dpe_tr_t15, observed).
narrative_ontology:measurement(dpe_tr_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(dpe_tr_t20, observed).
narrative_ontology:measurement(dpe_tr_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(dpe_tr_t25, observed).
narrative_ontology:measurement(dpe_tr_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement_basis(dpe_tr_t30, observed).
narrative_ontology:measurement(dpe_tr_t35, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(dpe_tr_t35, observed).
narrative_ontology:measurement(dpe_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(dpe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(dpe_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(dpe_be_t0, observed).
narrative_ontology:measurement(dpe_be_t5, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(dpe_be_t5, observed).
narrative_ontology:measurement(dpe_be_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(dpe_be_t10, observed).
narrative_ontology:measurement(dpe_be_t15, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(dpe_be_t15, observed).
narrative_ontology:measurement(dpe_be_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(dpe_be_t20, observed).
narrative_ontology:measurement(dpe_be_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement_basis(dpe_be_t25, observed).
narrative_ontology:measurement(dpe_be_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(dpe_be_t30, observed).
narrative_ontology:measurement(dpe_be_t35, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement_basis(dpe_be_t35, observed).
narrative_ontology:measurement(dpe_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(dpe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(dpe_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(dpe_su_t0, observed).
narrative_ontology:measurement(dpe_su_t5, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement_basis(dpe_su_t5, observed).
narrative_ontology:measurement(dpe_su_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(dpe_su_t10, observed).
narrative_ontology:measurement(dpe_su_t15, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(dpe_su_t15, observed).
narrative_ontology:measurement(dpe_su_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement_basis(dpe_su_t20, observed).
narrative_ontology:measurement(dpe_su_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement_basis(dpe_su_t25, observed).
narrative_ontology:measurement(dpe_su_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement_basis(dpe_su_t30, observed).
narrative_ontology:measurement(dpe_su_t35, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 35, 0.51).
narrative_ontology:measurement_basis(dpe_su_t35, observed).
narrative_ontology:measurement(dpe_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement_basis(dpe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This is one reading of the kernel 'legitimacy of practice standardization.' The dual-practice-equilibrium reading claims practice legitimacy partitions along domain lines (state/administrative vs. traditional/ritual). The sibling endogenous-displacement reading claims legitimacy comes from voluntary practice evolution (no partition). The sibling exogenous-override reading claims legitimacy comes from state decree for modernization (no partition, state monopoly). All three constrain the same kernel (how societies legitimize practice change), but produce structurally different constraints because they disagree on what makes a change legitimate. The endogenous and exogenous readings would reclassify this constraint's operation as pure snare if their framings were adopted; this reading defends partition-based legitimacy as genuine coexistence. The three readings coexist in actual societies—different parties hold different readings—making the kernel contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
