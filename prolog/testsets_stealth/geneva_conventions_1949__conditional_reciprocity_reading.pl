% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions 1949 — Conditional Reciprocity Reading
 *   domain: international humanitarian law / law of armed conflict / political philosophy
 *
 * SUMMARY:
 *   Under the conditional reciprocity reading, the Geneva Conventions operate
 *   as a compact between belligerents: full application is owed only to
 *   adversaries who comply, Article 4's criteria filter who counts as a
 *   lawful combatant, proportionality calculation administers civilian
 *   immunity, and detained irregulars fall outside the prisoner-of-war
 *   regime. The arrangement genuinely coordinates restraint between regular,
 *   complying forces — the interstate record of prisoner treatment and
 *   exchange is real. The same structure simultaneously transfers protection
 *   away from the parties least able to meet its conditions: irregular
 *   fighters, who are detained and prosecuted outside the protected class,
 *   and civilians, whose immunity survives only as far as the attacker's own
 *   proportionality arithmetic carries it. This file is ONE reading of the
 *   geneva_conventions_1949 kernel; the humanitarian_ceiling_reading and
 *   security_maximization_reading are separate constraints with their own
 *   epsilon values, linked through the network. KEY AGENTS (by structural
 *   relationship): - geneva_high_contracting_states: Agenda setter
 *   (institutional/constrained) — authored, administers, and interprets the
 *   conventions; decides when conditionality triggers -
 *   professional_state_militaries: Primary beneficiary
 *   (institutional/constrained) — collects assured reciprocal treatment plus
 *   degradation latitude - state_detention_authorities: Secondary beneficiary
 *   (institutional/constrained) — collects custody and prosecutorial
 *   discretion over irregular detainees - irregular_nonstate_fighters:
 *   Primary target (powerless/trapped) — bears loss of protected status -
 *   irregular_group_commanders: Target with bounded agency
 *   (moderate/constrained) — chooses between regularization and exposure -
 *   civilians_in_asymmetric_conflict_zones: Target with residual benefit
 *   (powerless/trapped) — immunity administered by the attacker's own
 *   calculations - icrc: Custodial observer (organized/analytical) — sees
 *   detention practice directly, contests conditionality doctrinally -
 *   international_humanitarian_tribunals: Judicial observer
 *   (institutional/analytical) — jurisprudence rejects reciprocity-dependence
 *   - human_rights_advocacy_organizations: Excluded voice (organized/mobile)
 *   — objects to the bargain's terms from outside it
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.58).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.62).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions 1949 — Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international humanitarian law / law of armed conflict / political philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, 'cde040bf-7540-4b12-9d3e-0fee8051ca18').
narrative_ontology:cs_kernel_codification('cde040bf-7540-4b12-9d3e-0fee8051ca18', fixed_text).
narrative_ontology:cs_authority_grounding('cde040bf-7540-4b12-9d3e-0fee8051ca18', lineage).
narrative_ontology:cs_interpretation_layer_present('cde040bf-7540-4b12-9d3e-0fee8051ca18').
narrative_ontology:cs_reading_relation('cde040bf-7540-4b12-9d3e-0fee8051ca18', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('cde040bf-7540-4b12-9d3e-0fee8051ca18', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('cde040bf-7540-4b12-9d3e-0fee8051ca18', foundational, full_application_conditional_on_adversary_compliance).
narrative_ontology:cs_axiom_status(full_application_conditional_on_adversary_compliance, holdable).
narrative_ontology:cs_axiom_grounding('cde040bf-7540-4b12-9d3e-0fee8051ca18', full_application_conditional_on_adversary_compliance, conventional).
narrative_ontology:cs_axiom('cde040bf-7540-4b12-9d3e-0fee8051ca18', secondary, irregular_noncompliance_permits_proportional_degradation).
narrative_ontology:cs_axiom_status(irregular_noncompliance_permits_proportional_degradation, holdable).
narrative_ontology:cs_axiom_grounding('cde040bf-7540-4b12-9d3e-0fee8051ca18', irregular_noncompliance_permits_proportional_degradation, instrumental).
narrative_ontology:cs_reference_frame('cde040bf-7540-4b12-9d3e-0fee8051ca18', reciprocal_restraint_compact).
narrative_ontology:cs_drift_state('cde040bf-7540-4b12-9d3e-0fee8051ca18', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cde040bf-7540-4b12-9d3e-0fee8051ca18', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, professional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_detention_authorities).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_nonstate_fighters).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_group_commanders).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, article_4_lawful_combatancy_criteria).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, reciprocity_principle_in_ihl).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, proportionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored and ratified the four 1949 Conventions and their Additional Protocols; implement them through domestic legislation, military manuals, and national courts, and interpret contested provisions through diplomatic protest, reservation, and practice. They decide when an adversary's conduct forfeits full protection and what response that forfeiture licenses. Leaving the treaty system is formally possible but practically closed: the core rules bind as custom, and denunciation mid-conflict would cost more than compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, geneva_high_contracting_states, agenda_setter,
    institutional, generational, constrained, global).

% Fight under the conventions' banner and collect two things from the conditional structure: assured reciprocal treatment for their own captured personnel when facing regular adversaries, and documented latitude to withhold prisoner-of-war status and widen targeting when an adversary fails the Article 4 tests. Their officers write the targeting policies that operationalize proportionality; their lawyers certify the compliance assessments that trigger degradation. Exiting the framework would mean fighting wholly outside the law, which no career officer contemplates.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, professional_state_militaries, beneficiary,
    institutional, biographical, constrained, global).

% Run the camps, commissions, and review boards that hold captured fighters. When a detainee is classed outside the prisoner-of-war regime, they gain interrogation latitude, extended custody without prisoner-of-war obligations, and discretion to prosecute the detainee for the act of fighting itself. Their institutional mandates and budgets expand with each cohort of irregular detainees.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_detention_authorities, beneficiary,
    institutional, biographical, constrained, global).

% Take up arms without uniforms, fixed bases, or a state sponsor — usually because they lack any other means of resisting a stronger state. Whether they receive prisoner-of-war status turns on criteria written for mass conscript armies: a distinctive sign visible at distance, open carriage of weapons, a responsible command. Failing any one of them, a captured fighter can be held for the duration without the conventions' protections, prosecuted for fighting at all, and cut off from the exchange and repatriation channels open to regular soldiers. There is no way to enter the protected class from outside a state's armed forces except through the very regularization the opposing state contests.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_nonstate_fighters, payer,
    powerless, biographical, trapped, regional).

% Lead armed groups and face a bounded choice: adopt the visible markers that qualify their fighters for prisoner-of-war status and expose the group to concentrated targeting, or fight dispersed and covert and accept that captured fighters fall outside the protected class. Some groups file adherence declarations under Additional Protocol I to claim convention coverage; capturing states routinely dispute the filings. Their decision shapes their fighters' fate but buys them no seat in the reciprocal bargain itself.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_group_commanders, payer,
    moderate, biographical, constrained, regional).

% Live where the fighting happens. The conventions promise them immunity from attack, but under the conditional reading that immunity is administered through proportionality calculations: each strike weighs anticipated military advantage against expected civilian harm, and the weighing is done by the attacking force. They receive whatever protection survives that arithmetic and have no channel to contest the valuation before the fact.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones, beneficiary).

% Holds the custodial mandate for the conventions: visits detention facilities, registers prisoners, brokers repatriations, and publishes the authoritative commentaries on the treaty text. It presses the position that the conventions' core protections do not depend on reciprocity, and its access agreements with belligerents give it a view of detention practice that neither party to a conflict controls.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, icrc, observer,
    organized, generational, analytical, global).

% Adjudicate war-crimes cases arising from the conventions. Their jurisprudence has repeatedly held that the conventions' fundamental protections apply irrespective of reciprocal performance, citing the conventions' own text and customary status — rulings that sit uneasily beside the conditionality that state practice continues to assert.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_humanitarian_tribunals, observer,
    institutional, generational, analytical, continental).

% Document degradation practices, litigate detention classifications, and campaign for absolute minimum protections. They hold no seat in the reciprocal arrangement whose terms they contest: belligerents consult them rarely, their objections register as external pressure rather than as terms of the bargain, and their leverage runs through publicity and domestic courts.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, human_rights_advocacy_organizations, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__conditional_reciprocity_reading, professional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__conditional_reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the wartime escalation problem between parties capable of mutual restraint: common, verifiable standards for treatment of the wounded, prisoners, and civilians let each side limit its violence in the credible expectation of returned restraint, replacing tit-for-tat atrocity cycles with stable rules of engagement.
% TRANSFER_FUNCTION: Moves protective status and bodily security from fighters who fail the Article 4 criteria and from civilians inside proportionality valuations, toward the capturing state: detention authority without prisoner-of-war obligations, prosecutorial discretion over the act of fighting, and widened targeting latitude. Moves assured reciprocal treatment toward the regular forces of complying parties.
% ABSENT_VOICES: Captured irregular fighters are the bargain's objects, never its parties: their consent was never solicited, and their standard objection — that they fight because no other means of resistance exists — surfaces only in post-hoc habeas and commission litigation. Civilians absorb proportionality valuations they cannot contest before the strike. Both would demand that protection turn on conduct toward civilians, not on uniform-and-insignia criteria.
% DISAPPEARANCE_RATIONALE: If the conditional structure vanished overnight, every detention classification in current asymmetric conflicts would reopen — thousands held outside prisoner-of-war status would fall back into either full convention protection or newly written detention law; targeting policies built on self-certified proportionality would need rewriting; and states would confront the choice the conditionality defers: absolute floors or open degradation. The interstate prisoner-exchange machinery built on the conventions would stall until rebuilt on explicit bilateral terms.
% FOUNDING_PROBLEM: The 1949 Conventions were drafted from the wreckage of the Second World War to break the atrocity cycle of unrestricted warfare: to guarantee the wounded medical care, prisoners humane treatment and eventual repatriation, and civilians immunity from attack, by binding belligerents into a common code each could trust the other to observe.
% FOUNDING_PROBLEM_CORROBORATION: All parties agree that atrocity in armed conflict remains a live problem; they dispute whether reciprocity-conditionality serves it. From outside the benefiting parties, the ICRC's Pictet commentaries and the records of the 1977 Diplomatic Conference attest that the original design contemplated application 'in all circumstances' (Common Article 1), and the Nuremberg-era record corroborates the founding atrocities themselves. State military manuals attest the problem's continuing liveness from the benefiting side. No neutral body attests that conditionality specifically was the founders' chosen solution — that attribution is precisely what divides this reading from the ceiling reading.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-to-substantial (0.58 at interval end) because the conditionality reliably strips protection from identifiable classes — captured irregulars and civilians under proportionality administration — while returning operational latitude to the capturing state; it is not higher because the same structure delivers real, verified restraint between regular forces. Suppression (0.62) reflects the coercive machinery the classification system requires: detention without prisoner-of-war status, military commissions, and the prosecution of fighting itself — suppression is authored as a raw structural property and is deliberately left unscaled; the engine scales only extractiveness. Theater ratio (0.36) captures a growing share of activity that defends the classification boundary (compliance determinations, status hearings, designation memos) rather than protecting anyone. Accessibility collapse is moderate (0.48): the alternative readings remain legally alive — Common Article 1's 'in all circumstances', Additional Protocol I, the tribunals' reciprocity-independent jurisprudence — so the conditional frame does not exhaust the space. Resistance (0.6) is sustained and institutionalized: ICRC advocacy, tribunal holdings, NGO litigation, and several states' official positions. The temporal series run on one shared grid. The trajectory is composition-driven rather than monotonic: extraction and suppression rise when the active-conflict mix shifts toward asymmetric wars (decolonization, Vietnam, the post-2001 campaigns), where conditionality bites, and ease when symmetric interstate wars dominate, where reciprocity performs as designed. The 1990 dip tracks the tribunal era's official rejection of reciprocity-dependence; the 2001–2006 spike tracks the full operationalization of the unlawful-combatant category. The oscillation is not itself the extraction mechanism — it is the signature of a constraint whose cost falls on whichever party type is currently fighting — but the 2001 spike shows the conditionality functioning as an extraction-enabling device when invoked opportunistically.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the state seat, the arrangement is a legitimate incentive structure it built: conditionality rewards regularization and punishes free-riding, and every degradation is certified by its own lawyers as proportionate response to adversary breach. From the irregular fighter's seat, the same structure is a trap written by the adversary: the criteria were drafted for mass conscript armies, compliance with them is suicidal against a superior air power, and the determination of non-compliance is made unilaterally by the party that benefits from making it. The tribunals occupy a third position — their jurisprudence holds the conventions' core protections reciprocity-independent, which makes the computed divergence between the state seat and the judicial seat a measurable feature of this corpus rather than an artifact of authorship.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to low directionality: professional_state_militaries and state_detention_authorities collect the arrangement's products (assured reciprocal treatment, custody latitude, prosecutorial discretion) and face no realistic exit from the framework that grants them. The victim declarations map to high directionality: irregular_nonstate_fighters and civilians_in_asymmetric_conflict_zones bear the transferred costs with trapped exit — a fighter cannot opt into protected status, and a civilian cannot leave the proportionality calculus that governs strikes on their neighborhood. The high-commanding states sit as agenda-setter with a beneficiary tilt: they bear compliance costs and retaliation risk, which keeps their derived directionality from reaching the pure-beneficiary pole. No directionality overrides are needed: the beneficiary/victim declarations plus exit options reproduce the structural relationships directly. Observers (ICRC, tribunals) take the analytical seat and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming tangled_rope keeps both halves of the structure visible and prevents two opposite mislabels. Mislabeling the arrangement as pure rope would erase its victims — the detained irregular class and the proportionality-administered civilians — and launder conditionality as costless coordination. Mislabeling it as pure snare would erase the verified interstate record: the prisoner regimes, exchange machinery, and restraint between regular forces that the conventions demonstrably produced from 1949 onward. On the genealogy interview, the founding problem (wartime atrocity) is live but the founding mechanism's original theater — symmetric armies capable of mutual verification — has thinned as conflict has gone asymmetric, which is what the theater_ratio series tracks. The status x verdict pair reads contested x world_rearranges: no dead-mandate zombie flag is asserted, but the arrangement persists partly by inertia of the classification apparatus it built. Coalition potential exists on the victim side and is noted for completeness: irregular groups can collectively regularize (Additional Protocol I adherence declarations, insignia adoption), a path that would move fighters into the protected class — but it is costly, exposes groups to concentrated targeting, and capturing states routinely dispute the filings, so the coalition door exists yet opens slowly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the geneva_conventions_1949 kernel; would the humanitarian_ceiling_reading or the security_maximization_reading change the constraint''s beneficiary and victim structure?',
    'Author the sibling stories and compare computed classifications: the ceiling reading removes the conditionality (irregular fighters regain protected status; the victim set shrinks toward direct-target civilians); the maximization reading expands degradation permissions (most protections become suspensible, widening the victim set to near-all detainees).',
    'If the ceiling reading better accounts for the conventions'' actual legal force (tribunal jurisprudence, customary status, Common Article 1), this reading''s extraction profile marks it as a contested overlay rather than the kernel''s operative content; if the maximization reading prevails in practice, this reading is the moderate midpoint of a wider degradation ratchet.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the Geneva kernel governs, and what each sibling would change structurally.').

omega_variable(
    unilateral_compliance_assessment,
    'Who determines that an adversary has failed to comply, and is that determination independent of the determining party''s interests?',
    'Comparative analysis of degradation episodes: whether third-party findings (ICRC reports, tribunal judgments, UN inquiries) preceded or followed the degrading state''s self-assessment, and whether any degradation decision was ever reversed by an adverse third-party finding.',
    'If compliance assessment is systematically self-judged, the conditionality operates as a unilateral license and the arrangement sits nearer pure extraction than reciprocal coordination; independent adjudication of breach would restore the coordination reading and lower effective extraction on the victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_compliance_assessment, empirical, 'Whether the reciprocity condition is adjudicated by anyone other than the party invoking it.').

omega_variable(
    article_4_criteria_attainability,
    'Are the Article 4 lawful-combatancy criteria (responsible command, distinctive sign visible at distance, open carriage of arms) attainable for contemporary irregular forces, or structurally biased toward the mass-conscript warfare of 1949?',
    'Historical and operational analysis: how often irregular groups have successfully regularized (Additional Protocol I adherence declarations, insignia adoption), at what targeting cost, and whether the criteria track genuine discriminability or state convenience.',
    'If the criteria are unattainable in practice for most insurgents, the conditionality filters protection by the character of the adversary rather than by conduct toward civilians — deepening the victim asymmetry and pushing the arrangement toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_4_criteria_attainability, empirical, 'Whether the conditions of protection are fairly attainable by non-state fighters.').

omega_variable(
    proportionality_narrowing_magnitude,
    'How much civilian immunity does proportionality calculation actually remove, relative to what the immunity grant promises?',
    'Strike-level casualty data compared against stated collateral-damage estimates across campaigns; post-strike investigations measuring the gap between pre-strike valuations and realized harm.',
    'A wide promise-versus-delivery gap would mean the preserved civilian immunity is largely nominal under this reading, raising effective extraction on the civilian seat and weakening the coordination half of the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_narrowing_magnitude, empirical, 'Magnitude of civilian-immunity erosion under proportionality administration.').

omega_variable(
    reciprocity_genuine_or_opportunistic,
    'Is the reciprocity condition a genuine coordination device that sustains mutual restraint, or a cover invoked selectively to license degradation the invoking party would pursue regardless?',
    'Within-conflict comparison: whether degrading states extend equivalent treatment when they hold a non-compliant adversary''s personnel, and whether restraint resumes when adversaries regularize or comply.',
    'Selective invocation converts the coordination function into extraction cover and would justify movement toward snare; consistent application across roles supports the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_genuine_or_opportunistic, conceptual, 'Whether conditionality tracks actual reciprocity or opportunistic invocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_conditional_reciprocity_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.12).
narrative_ontology:measurement(geneva_conditional_reciprocity_tr_t1955, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1955, 0.16).
narrative_ontology:measurement(geneva_conditional_reciprocity_tr_t1970, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1970, 0.26).
narrative_ontology:measurement(geneva_conditional_reciprocity_tr_t1990, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(geneva_conditional_reciprocity_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.37).
narrative_ontology:measurement(geneva_conditional_reciprocity_tr_t2006, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2006, 0.4).
narrative_ontology:measurement(geneva_conditional_reciprocity_tr_t2015, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(geneva_conditional_reciprocity_tr_t2025, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2025, 0.36).

% Extraction over time
narrative_ontology:measurement(geneva_conditional_reciprocity_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.34).
narrative_ontology:measurement(geneva_conditional_reciprocity_be_t1955, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1955, 0.38).
narrative_ontology:measurement(geneva_conditional_reciprocity_be_t1970, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(geneva_conditional_reciprocity_be_t1990, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1990, 0.46).
narrative_ontology:measurement(geneva_conditional_reciprocity_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.6).
narrative_ontology:measurement(geneva_conditional_reciprocity_be_t2006, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2006, 0.63).
narrative_ontology:measurement(geneva_conditional_reciprocity_be_t2015, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement(geneva_conditional_reciprocity_be_t2025, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(geneva_conditional_reciprocity_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.3).
narrative_ontology:measurement(geneva_conditional_reciprocity_su_t1955, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1955, 0.36).
narrative_ontology:measurement(geneva_conditional_reciprocity_su_t1970, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement(geneva_conditional_reciprocity_su_t1990, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(geneva_conditional_reciprocity_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.66).
narrative_ontology:measurement(geneva_conditional_reciprocity_su_t2006, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2006, 0.7).
narrative_ontology:measurement(geneva_conditional_reciprocity_su_t2015, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(geneva_conditional_reciprocity_su_t2025, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Geneva Conventions' covers three structurally distinct claims about what binds belligerents, decomposed per the epsilon-invariance principle. This story (conditional_reciprocity_reading) authors epsilon for the conditional-restraint arrangement: protections owed in proportion to adversary compliance. The humanitarian_ceiling_reading authors epsilon for the absolute-floor arrangement (no conditionality; victim set shrinks to direct targets of attack). The security_maximization_reading authors epsilon for the defeasible arrangement (protections yield to operational necessity; victim set widens to most detainees). The upstream member is the ceiling reading — highest textual and customary support (Common Article 1, tribunal jurisprudence) — which the other two readings cite or resist; this reading sits mid-family, borrowing the text's authority while narrowing its application. All three files link one another through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
