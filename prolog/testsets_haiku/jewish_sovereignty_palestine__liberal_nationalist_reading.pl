% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Collective Self-Determination Right and Statehood (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint instantiates the liberal-nationalist reading of the
 *   Jewish sovereignty claim: the Jewish people, as a collective with
 *   historical territorial connection and shared identity, possess a
 *   self-determination right parallel to other nations, and statehood in the
 *   ancestral homeland (Palestine/Eretz Yisrael) is the legitimate exercise
 *   of that right. This reading accepts Palestinian co-equal
 *   self-determination claims and frames the constraint as requiring
 *   partition or binational governance rather than unilateral Jewish
 *   dominance. The expected structural delta places extractiveness at
 *   moderate levels (0.58) because territorial compromise is anticipated. The
 *   claim/metric tension is deliberate: the constraint is CLAIMED as rope
 *   (coordination of national self-determination principles) while the
 *   authored metrics describe moderately extractive, actively enforced
 *   operation — the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - Jewish collective: organized agent holding identity-locked commitment to statehood; beneficiary/agenda-setter; globally dispersed with civilizational time horizon
 *   - Palestinians (displaced or subordinate): moderate power, territorially trapped, co-equal claimants under this reading but materially subordinate in territorial allocation; payers of the constraint's territorial cost
 *   - Liberal democracies and international order: institutional beneficiaries; recognize and legitimize self-determination as binding principle
 *   - Jewish diaspora: organized beneficiaries with mobile exit; gain symbolic sovereignty and collective voice without bearing immediate territorial cost
 *   - Challenger readings (settler-colonial, post-zionist, religious-zionist): excluded from this reading's legitimacy narrative; non-agents in this formulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.58).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.47).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Collective Self-Determination Right and Statehood (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '4559cf9f-26d0-49d5-8dab-0b72a0715eba').
narrative_ontology:cs_kernel_codification('4559cf9f-26d0-49d5-8dab-0b72a0715eba', fixed_text).
narrative_ontology:cs_authority_grounding('4559cf9f-26d0-49d5-8dab-0b72a0715eba', lineage).
narrative_ontology:cs_interpretation_layer_present('4559cf9f-26d0-49d5-8dab-0b72a0715eba').
narrative_ontology:cs_reading_relation('4559cf9f-26d0-49d5-8dab-0b72a0715eba', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('4559cf9f-26d0-49d5-8dab-0b72a0715eba', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4559cf9f-26d0-49d5-8dab-0b72a0715eba', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4559cf9f-26d0-49d5-8dab-0b72a0715eba', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('4559cf9f-26d0-49d5-8dab-0b72a0715eba', foundational, national_self_determination_universal_principle).
narrative_ontology:cs_axiom_status(national_self_determination_universal_principle, holdable).
narrative_ontology:cs_axiom_grounding('4559cf9f-26d0-49d5-8dab-0b72a0715eba', national_self_determination_universal_principle, deontological).
narrative_ontology:cs_axiom('4559cf9f-26d0-49d5-8dab-0b72a0715eba', secondary, jewish_dispersal_negates_statehood_necessity).
narrative_ontology:cs_axiom_status(jewish_dispersal_negates_statehood_necessity, holdable).
narrative_ontology:cs_axiom_grounding('4559cf9f-26d0-49d5-8dab-0b72a0715eba', jewish_dispersal_negates_statehood_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('4559cf9f-26d0-49d5-8dab-0b72a0715eba', universal_self_determination_principle).
narrative_ontology:cs_drift_state('4559cf9f-26d0-49d5-8dab-0b72a0715eba', contemporary_post_oslo_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4559cf9f-26d0-49d5-8dab-0b72a0715eba', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinians_displaced_or_subordinate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_democracies_international_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A dispersed nation organized around shared identity, historical memory, and (in this reading) the claim to collective self-determination via statehood in the ancestral homeland. The constraint benefits the collective by securing territorial sovereignty, juridical authority, and reversal of historical statelessness. Identity is constituted through the return narrative; exit would mean abandoning the core identity claim itself. The collective sets the terms of statehood and its borders, though it must nominally accommodate Palestinian claims.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective, agenda_setter).

% A people claiming the same territorial space and self-determination rights as the Jewish collective. Under the liberal-nationalist reading, Palestinians are acknowledged as co-equal claimants requiring partition or binational governance; in practice, they bear the cost of territorial loss (1948 displacement, 1967 occupation), subordinate political status (Palestinian Authority autonomy rather than full sovereignty), and lack of control over return of refugees. They are territorially trapped (cannot leave en masse) and politically trapped (cannot exit the constraint without abandoning self-determination claim).
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinians_displaced_or_subordinate, payer,
    moderate, civilizational, trapped, regional).

% Dispersed Jewish communities worldwide who benefit from the symbolic sovereignty, political backing, and identity reinforcement provided by a Jewish state, without bearing the immediate territorial cost of displacement or occupation. They retain options to live elsewhere and are not trapped in the regional conflict, making them mobile beneficiaries. They gain collective political voice and backup security without the constraints of Palestinian populations.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora, beneficiary,
    organized, civilizational, mobile, global).

% Recognize Jewish self-determination as instantiating and vindicating the universal principle of national self-determination. They benefit from the precedent strength of the principle and from positioning themselves as correcting historical injustice via law and principle. They provide the legitimacy infrastructure (UN recognition, international legal status) that anchors the constraint, though they face pressure from readings that dispute whether the principle was correctly applied or has become dysfunctional.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_democracies_international_order, beneficiary,
    institutional, generational, analytical, global).

% Historical settler-colonial powers (European states, colonial empires) that might have competing interests in the Middle Eastern territorial order. Their exclusion from this reading's framing is notable: the reading claims universality and justice-based legitimacy, not settler-colonial precedent. They are excluded voices whose interests in maintaining colonial-era power arrangements would contradict the self-determination principle.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, settler_colonial_states, excluded,
    institutional, generational, analytical, global).

% The settler-colonial reading, post-zionist reading, and religious-zionist reading represent political and intellectual movements that contest this reading's legitimacy frame and offer alternative groundings (structural colonialism, ethnic nationalism critique, theological entitlement). These readings are structurally excluded from the negotiating table that instantiates this constraint, though they gain increasing institutional voice over time.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, challenger_political_movements, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(jewish_sovereignty_palestine__liberal_nationalist_reading, challenger_political_movements).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes the binding principle that dispersed peoples with shared identity and historical territorial connection possess a collective right to self-determination via territorial sovereignty. This solves the coordination problem of how a stateless people without territorial anchor gains juridical standing, protective capacity, and political voice in the international system. The reading treats self-determination as universalizable: if applied to the Jewish collective, it must also apply to Palestinians, requiring partition or binational governance rather than unilateral dominance.
% TRANSFER_FUNCTION: Transfers territorial control, juridical authority, and political sovereignty from external mandate or pre-existing arrangements to the Jewish collective organized as a nation-state. Simultaneously, transfers protection and backing to the global Jewish diaspora. Transfers to Palestinians the cost of territorial loss (1948 displacement, 1967 occupation), subordinate political status, and permanent displacement or second-class citizenship. Transfers to liberal democracies the benefit of a vindicated universal principle, though they also absorb diplomatic and political costs.
% ABSENT_VOICES: Settler-colonial readings that argue displacement is structurally inherent are excluded from the legitimacy narrative, which frames the constraint as liberal-universalist rather than colonial. Post-zionist readings that critique ethnic nationalism as the source of ongoing conflict are marginalized despite post-1967 visibility. Religious-zionist voices grounding the claim in theology rather than universal principle are excluded in favor of secular liberal-nationalist framing. Palestinian perspectives on co-equality are nominally present but structurally subordinate in territorial allocation.
% DISAPPEARANCE_RATIONALE: If this constraint's legitimacy framework vanished — if the international order ceased recognizing Jewish self-determination as a binding right or withdrew recognition of the state — the entire architecture of Middle Eastern geopolitics, diaspora-homeland relations, and regional conflict dynamics would reorganize. Diaspora Jews would lose state backing; Palestinians would face a fundamentally changed political landscape (no need to accommodate a Jewish state, new partition possibilities, or regional integration); the liberal international order's legitimacy would be challenged by the withdrawal of a principle it had endorsed.
% FOUNDING_PROBLEM: Jewish people faced permanent statelessness, diaspora vulnerability, and persecution culminating in industrial-scale genocide (Holocaust). The founding problem was: how does a dispersed people with historical territorial connection and shared identity recover collective self-governance, security, and protection from persecution?
% FOUNDING_PROBLEM_CORROBORATION: This reading and its beneficiaries (Jewish collective, diaspora, liberal democracies) attest the founding problem remains partially live: diaspora antisemitism persists, security threats exist, and the state provides protection and deterrence. Settler-colonial, post-zionist, and Palestinian readings attest the founding problem is analytically separable from statehood as the solution: persecution could be addressed through other means (international law, diaspora security cooperation, refugee protection) without requiring territorial sovereignty in a specific homeland, or argue the solution creates new problems (displacement, regional instability) that outweigh security gains. International humanitarian organizations and Palestinian self-determination advocates attest that while persecution is real, the state's founding role in solving it is now historically contingent; the constraint persists through institutional inertia and identity-lock rather than renewed problem-solving.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.42 at t0, rising to 0.58 by t40 where it plateaus) because the liberal-nationalist reading explicitly acknowledges Palestinian co-equal claims and frames a solution (partition or binationalism) that requires territorial compromise. The constraint is extractive rather than purely coordinative because territorial allocation remains asymmetric: Jewish statehood is secured while Palestinian statehood faces implementation barriers, and the Jewish diaspora benefits without bearing displacement cost. Suppression (0.47) is moderate because the reading grounds legitimacy in universal self-determination principles rather than theological or ethnic supremacy, but enforcement requires continuous legal and diplomatic effort to sustain the territorial and demographic boundaries against Palestinian resistance. Theater (0.22) is low-to-moderate: the self-determination principle is genuinely coordinative (solves the problem of how a dispersed people gains juridical standing), but a growing share of the constraint's operation is performative lip-service to Palestinian rights without substantive implementation. The plateau at t40 reflects the constraint reaching a stable institutional configuration (post-1967 occupation, Oslo framework, etc.) where further metric movement requires structural change (annexation, full binationalism, or partition implementation), which the constraint's current form does not provide.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's claim (universal self-determination principle, partition/binationalism as solution) and its measured operation (extractiveness rising from 0.42 to 0.58, suppression stable at 0.47, theater rising from 0.08 to 0.22) reveal a gap between the reading's legitimacy frame and its material distribution. Liberal-nationalist authors claim the self-determination principle is universalizable and Palestinian rights are accommodated; the measurement series suggests the constraint's operation is increasingly extractive and performative, with less actual material accommodation of Palestinian statehood as decades pass. This gap is exactly what the framework is designed to detect — a claimed rope that measures as moderately extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish collective: d ≈ 0.2–0.3 (beneficiary, though identity-locked exit means high commitment cost; identity-locked does not eliminate beneficiary directionality, it just anchors it). Palestinians: d ≈ 0.85–0.95 (target; territorially trapped, facing displacement or permanent subordination despite nominal co-equal status). Liberal democracies: d ≈ 0.15–0.25 (weak beneficiary; they gain legitimacy infrastructure and precedent but face diplomatic/political cost). Jewish diaspora: d ≈ 0.05–0.15 (beneficiary; mobile exit means low commitment cost, pure gain from collective voice and backing). Challenger readings: excluded from the directionality computation in THIS reading; they enter as separate constraints (other files) with their own beneficiary/victim structures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness, persecution, genocide) and the constraint's founding solution (statehood via self-determination) show a mandatrophy marker: at t80 (contemporary observation), post-zionist and settler-colonial readings increasingly assert that the founding problem is analytically separable from statehood as the solution, or that the solution now creates new problems (Palestinian displacement, regional instability, ethnic nationalism) that outweigh the security gain. The constraint persists because the Jewish collective remains identity-locked to the statehood claim and because institutional inertia (international recognition, security architecture, diaspora investment) makes reversal costly. But the readings that contest the founding problem's liveness or the solution's appropriateness are gaining purchase, which is a mandatrophy signal: the constraint's mandate (solve Jewish statelessness) is partially obsolete or contested, yet the constraint persists via institutional form rather than renewed legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_binationalism_implementation,
    'Is partition (two separate nation-states) or binationalism (one state with equal civic and national status for both peoples) the true reading of this constraint''s solution space? Can both be accommodated by the same liberal-nationalist principle, or do they entail structurally different constraints?',
    'Historical analysis of what liberal-nationalist theorists in the founding period actually endorsed (partition vs. binational state); comparison of actual territorial and governance outcomes with stated principle; examination of whether binational governance has ever succeeded for similarly structured national conflicts.',
    'If partition is the only coherent reading, then the constraint''s implementation has systematically failed Palestinian claims to equal territory and sovereignty, and extractiveness should be classified higher. If binationalism is viable and endorsed within this reading, the constraint''s extractiveness depends on degree of civic equality and shared governance actually achieved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_binationalism_implementation, conceptual, 'Whether partition and binationalism are structurally different solutions or alternative implementations of the same principle.').

omega_variable(
    diaspora_benefit_vs_displacement_cost_asymmetry,
    'The Jewish diaspora gains collective voice and symbolic sovereignty without bearing displacement cost; Palestinians bear displacement or permanent subordination while diaspora Jews remain mobile. Is this asymmetry structurally inherent to the liberal-nationalist principle, or a contingent choice about how to implement it?',
    'Examine alternative statehood models that do not require population displacement (e.g., federation, autonomy arrangements, or shared territorial sovereignty); assess whether these are consistent with the liberal-nationalist principle or whether the principle logically entails majority-rule nation-state sovereignty in a specific territory.',
    'If asymmetry is contingent, the constraint is more extractive than the principle alone requires, and remedies (return of refugees, territorial reconfiguration, or shared governance) are compatible with the reading. If asymmetry is structurally inherent, the constraint is correctly measured and the reading inherently produces extractive territorial allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_benefit_vs_displacement_cost_asymmetry, empirical, 'Whether the displacement/subordination asymmetry is necessary or contingent to the principle.').

omega_variable(
    liberal_nationalism_vs_ethnic_nationalism_boundary,
    'This reading claims to ground legitimacy in universal self-determination principle (liberal-nationalist), not ethnic or theological claim. But where is the boundary between liberal universalism (self-determination for all dispersed peoples) and ethnic nationalism (self-determination for THIS people because of blood, history, or religious connection)? Does the reading cross it in practice?',
    'Examine whether the reading extends the same statehood claim to other dispersed peoples with historical territorial connections (e.g., indigenous populations, diaspora Greeks, Armenians); examine whether the reading grounds legitimacy primarily in universal principle or in Jewish historical/theological connection to the territory. If different standards apply to different peoples, the reading has ethnic-nationalist rather than liberal-nationalist structure.',
    'If the reading is genuinely liberal-nationalist, it should be universalizable and should grant equivalent claims to Palestinians and other peoples. If it restricts universality (claims self-determination is a general principle but statehood in ancestral territory is a special Jewish entitlement), then it has crossed into ethnic nationalism, and extractiveness should be classified higher and the constraint reclassified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liberal_nationalism_vs_ethnic_nationalism_boundary, conceptual, 'Whether this reading''s legitimacy is genuinely universal or specifically tied to Jewish claims.').

omega_variable(
    territorial_compromise_expectation_vs_reality,
    'The expected structural delta states that ''extractiveness moderate (territorial compromise expected)''; does the actual territorial allocation (post-1967 occupation, settlements, Palestinian Authority autonomy) reflect genuine partition/compromise, or is it subordination disguised as compromise?',
    'Compare theoretical partition proposals (e.g., 1947 UN partition plan, Oslo two-state vision) with actual territorial control, settlement patterns, and Palestinian self-governance capacity; assess whether Palestinians have genuine territorial integrity, contiguous statehood, and sovereign control over resources, or whether the constraint''s operation has frozen them in perpetual subordination.',
    'If actual allocation is genuine compromise, extractiveness at 0.58 is justified by the constraint''s coordination function and shared costs. If actual allocation is subordination despite compromise language, extractiveness should be classified higher (0.70+) and the constraint should be reclassified as tangled rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(territorial_compromise_expectation_vs_reality, empirical, 'Whether territorial allocation reflects genuine partition or enforced Palestinian subordination.').

omega_variable(
    settler_colonial_foreclosure_or_coexistence,
    'Does the liberal-nationalist reading''s universal self-determination principle logically foreclose the settler-colonial reading (which argues displacement is inherent and non-negotiable), or do they coexist as two different framings of the same historical process?',
    'Examine whether settler-colonial structure can be ''solved'' or ''reformed'' via liberal-nationalist principles (e.g., by implementing genuine partition, return of refugees, or shared governance), or whether settler-colonial displacement is the logical outcome regardless of liberal framing. If liberal principles can reform settler structures, readings coexist; if not, one reading forecloses the other.',
    'If settler-colonial reading is foreclosed, this reading''s legitimacy is robust against that challenge. If they coexist, the readings are incompatible frameworks (liberal universalism vs. structural colonialism) and the constraint''s operation may satisfy one reading while falsifying another. The engine uses this to compute whether the constraint''s actual outcome aligns with the reading''s self-description.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_colonial_foreclosure_or_coexistence, conceptual, 'Whether settler-colonial analysis is logically incompatible with liberal-nationalist self-determination principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jewi_tr_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(jewi_tr_t20, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(jewi_tr_t40, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(jewi_tr_t60, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(jewi_tr_t80, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 80, 0.22).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jewi_be_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(jewi_be_t20, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(jewi_be_t40, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(jewi_be_t60, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(jewi_be_t80, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 80, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(jewi_su_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(jewi_su_t20, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(jewi_su_t40, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement(jewi_su_t60, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 60, 0.47).
narrative_ontology:measurement(jewi_su_t80, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 80, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel jewish_sovereignty_palestine, which decomposes into five structurally distinct constraints: liberal_nationalist_reading (universal self-determination principle, partition/binationalism), settler_colonial_reading (inherent displacement structure), post_zionist_reading (statehood achieved, ethnic nationalism now dysfunctional), religious_zionist_reading (theological entitlement), and cultural_zionist_reading (cultural center without political sovereignty requirement). Each reading has different beneficiary/victim structures, extractiveness values, and axioms. The epsilon-invariance principle requires separate stories because each reading's ε differs: liberal-nationalist measures as moderate extraction (0.58) because compromise is acknowledged; settler-colonial measures as high extraction (0.75+) because displacement is structural; post-zionist measures as moderate-high (0.65+) because the ethnic framework obstructs solutions; religious-zionist measures moderate (0.55) because theology grounds the claim differently; cultural-zionist measures as low (0.25–0.35) because political sovereignty is not required. They are linked via network.affects_constraints because each reading's legitimacy status and adoption affect the others' operational space and resistance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
