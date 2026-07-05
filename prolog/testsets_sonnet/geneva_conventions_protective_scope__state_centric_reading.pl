% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Protective Scope — State-Centric (Article 4) Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict
 *
 * SUMMARY:
 *   This story instantiates the state-centric reading of the Geneva
 *   conventions' protective-scope kernel: Article 4 of the Third Geneva
 *   Convention is read as a strict, four-part checklist (responsible command,
 *   fixed distinctive sign, open carriage of arms, conduct per the laws of
 *   war) whose satisfaction is required before POW protections attach.
 *   Fighters who do not or cannot satisfy this checklist — most prominently
 *   the guerrilla, insurgent, and irregular militia fighter typical of
 *   asymmetric conflict — fall outside the treaty's protective floor as this
 *   reading construes it. The reading has a genuine coordination function: it
 *   gives conventional militaries a workable, bright-line test in the fog of
 *   war. It also has a structurally asymmetric extraction effect: the
 *   criteria were drafted around, and are best satisfied by, exactly the
 *   organizational form of a conventional state military, so the reading
 *   systematically produces protection for the well-resourced, uniformed side
 *   of a conflict and produces exposure for the side that fights
 *   asymmetrically because it must. This is one of three sibling readings of
 *   the same kernel (geneva_conventions_protective_scope); the
 *   hybrid_proportionality_reading and universal_rights_reading are separate
 *   constraint stories with their own ε values, victim sets, and beneficiary
 *   structures — see network.affects_constraints and
 *   commentary.kernel_context.
 *
 * KEY AGENTS:
 *   - conventional_state_militaries: Primary beneficiary (institutional/arbitrage) — satisfy the criteria by organizational design, receive reciprocal POW protection, gain targeting/detention latitude over adversaries who cannot satisfy the criteria
 *   - unprivileged_belligerents: Primary target (powerless/trapped) — excluded from POW status by criteria structurally aligned against their organizational form
 *   - state_military_lawyers: Agenda-setter (institutional/analytical) — operationalize and interpret the criteria in targeting and detention decisions
 *   - asymmetric_conflict_theorists: Analytical observer — traces the structural mismatch between the criteria's interstate-symmetric assumptions and contemporary asymmetric conflict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.42).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.55).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Protective Scope — State-Centric (Article 4) Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/legal_theory/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '96388c48-b078-4717-8a1f-f158341c1a07').
narrative_ontology:cs_kernel_codification('96388c48-b078-4717-8a1f-f158341c1a07', fixed_text).
narrative_ontology:cs_authority_grounding('96388c48-b078-4717-8a1f-f158341c1a07', lineage).
narrative_ontology:cs_interpretation_layer_present('96388c48-b078-4717-8a1f-f158341c1a07').
narrative_ontology:cs_reading_relation('96388c48-b078-4717-8a1f-f158341c1a07', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('96388c48-b078-4717-8a1f-f158341c1a07', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('96388c48-b078-4717-8a1f-f158341c1a07', foundational, combatant_privilege_requires_reciprocal_visibility).
narrative_ontology:cs_axiom_status(combatant_privilege_requires_reciprocal_visibility, holdable).
narrative_ontology:cs_axiom_grounding('96388c48-b078-4717-8a1f-f158341c1a07', combatant_privilege_requires_reciprocal_visibility, conventional).
narrative_ontology:cs_axiom('96388c48-b078-4717-8a1f-f158341c1a07', foundational, organizational_form_determines_protective_eligibility).
narrative_ontology:cs_axiom_status(organizational_form_determines_protective_eligibility, holdable).
narrative_ontology:cs_axiom_grounding('96388c48-b078-4717-8a1f-f158341c1a07', organizational_form_determines_protective_eligibility, instrumental).
narrative_ontology:cs_reference_frame('96388c48-b078-4717-8a1f-f158341c1a07', interstate_symmetric_warfare_1949).
narrative_ontology:cs_drift_state('96388c48-b078-4717-8a1f-f158341c1a07', post_9_11_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96388c48-b078-4717-8a1f-f158341c1a07', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, occupying_power_command_structures).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, state_military_lawyers).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, irregular_militia_fighters).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, captured_non_state_combatants).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, responsible_command_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, distinction_principle_via_uniform).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Field uniformed, hierarchically commanded forces that cleanly satisfy Article 4's four criteria (responsible command, fixed distinctive sign, carrying arms openly, conduct per laws of war). They receive full POW protections for their own captured personnel and, symmetrically, can treat captured irregulars from asymmetric adversaries as falling outside treaty-mandated POW status — permitting broader targeting and detention latitude. They helped draft and continue to interpret the criteria through military legal advisers and state delegations to treaty bodies.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter).

% Administer captured territory and detention operations; the state-centric reading gives them a bright-line test for who must be processed as a POW versus who may be handled under domestic or military-commission law with fewer procedural protections. This materially lowers the administrative and political cost of prosecuting or detaining irregular fighters.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, occupying_power_command_structures, beneficiary,
    institutional, generational, arbitrage, national).

% Fight without a fixed distinctive sign or a command structure recognizable to the capturing state — often because guerrilla tactics or clandestine organization are the only viable resistance a materially weaker force can mount. Once captured, they fall outside Article 4 POW status: no guaranteed combatant immunity for lawful acts of war, exposure to domestic criminal prosecution for the mere act of fighting, and no guaranteed baseline of treatment beyond what Common Article 3 or customary law is read to require. They have no seat in treaty interpretation and no capacity to alter their own qualifying status after capture.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, biographical, trapped, national).

% Organized community or factional defense forces that may satisfy some but not all Article 4 sub-criteria (e.g., visible command but no fixed insignia, or vice versa). Their status is adjudicated after the fact by the capturing power's own military tribunals, applying the very criteria that power benefits from interpreting narrowly. Exit is not available once captured; their bargaining position is zero.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, irregular_militia_fighters, payer,
    powerless, biographical, trapped, regional).

% The immediate, individual-level instantiation of the victim class: a person in custody whose treatment (interrogation limits, judicial process, release conditions) turns on a classification decision made unilaterally by their captor under a legal standard the captor's own state helped author and now applies to itself as adjudicator.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, captured_non_state_combatants, payer,
    powerless, immediate, trapped, local).

% Draft rules of engagement, targeting directives, and detention classification memos that operationalize the Article 4 criteria. They administer the boundary between privileged and unprivileged status in practice, shaping how strictly the criteria are read case by case, and their institutional incentive runs toward readings that preserve maximal operational latitude for their own state's forces.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_military_lawyers, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, state_military_lawyers, beneficiary).

% Advocate for the broadest defensible protective scope and document treatment of detainees regardless of formal status, but have no binding authority to override a capturing state's Article 4 determination. Their monitoring reports register objection but do not reclassify anyone; they operate at the mercy of access agreements the detaining power can revoke.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, icrc_and_humanitarian_monitors, excluded,
    organized, generational, constrained, global).

% Study how the Article 4 framework, designed around symmetric interstate warfare between comparably organized militaries, performs when one party is structurally incapable of meeting the criteria by the nature of the asymmetry itself — producing a legal regime that systematically favors the better-resourced, more conventionally organized combatant.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, asymmetric_conflict_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, verifiable test (responsible command, fixed distinctive sign, open carriage of arms, conduct per laws of war) that lets capturing states and their forces distinguish, in the chaos of active conflict, whom they must treat as a POW under a stable, predictable, internationally recognized standard — solving a genuine coordination problem for symmetric interstate war between comparably organized militaries.
% TRANSFER_FUNCTION: Moves the burden of legal ambiguity and its consequences (prosecution exposure, reduced procedural protection, detention without POW status) from conventional state militaries onto non-state and irregular combatants whose organizational form cannot satisfy criteria drafted around state military structures — while moving interpretive discretion and administrative flexibility to the capturing state.
% ABSENT_VOICES: Non-state armed groups, insurgent and resistance movements, and captured individuals themselves have no seat at the diplomatic conferences or treaty-interpretation bodies that fix or re-fix the Article 4 criteria; the criteria are negotiated exclusively among states, including states with direct interest in denying POW status to their internal or asymmetric adversaries.
% DISAPPEARANCE_RATIONALE: State militaries and their legal establishments would say the world rearranges catastrophically without a bright-line criterion — no stable basis for distinguishing combatants from civilians, undermining the entire distinction principle. Advocates for unprivileged belligerents would say the disappearance of the state-centric reading specifically (as opposed to Geneva protection generally) would not unravel humanitarian law; it would simply shift adjudication to Common Article 3 or a proportionality-based reading, which is precisely what the sibling readings propose. The disagreement is genealogical: is the state-centric threshold necessary to the coordination function, or is it a specific, contestable policy choice layered onto a broader protective floor that would persist without it?
% FOUNDING_PROBLEM: The 1949 drafters sought to prevent the disorder of the World Wars, where combatants who did not wear uniforms or operate under clear command made it difficult for opposing militaries to apply humane treatment reliably; the Article 4 criteria were meant to give conventional armies workable rules for extending POW status without exposing themselves to enemy fighters posing as civilians.
% FOUNDING_PROBLEM_CORROBORATION: State military legal establishments attest the founding problem remains live — distinguishing combatants from civilians is unresolved and arguably harder in urban and networked conflict. Independent IHL scholars and ICRC commentary (a source outside the beneficiary class) attest that the criteria, as currently applied, function less to solve the distinction problem than to withhold protections from precisely the asymmetric adversaries most states now actually fight, and that state practice has drifted from the drafters' original interstate-symmetric assumption without formal revision of the treaty text.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, contested).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).
:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42, rising slowly from 0.22 at the treaty's founding) because the state-centric reading's cost falls narrowly but severely on a specific, identifiable class (unprivileged belligerents) rather than diffusely; the rise across the interval tracks the growing prevalence of asymmetric and non-international conflict relative to the interstate wars the criteria were built for, without any change to the treaty text itself — extraction accumulates as the world the rule governs drifts away from the world it was drafted for. Suppression (0.55) reflects that the reading is maintained by the capturing state's own unilateral, largely unreviewable classification authority — there is no independent tribunal a captured irregular fighter can appeal to that is not itself an organ of the capturing power. Theater ratio is kept modest (0.28) because the coordination function (distinguishing combatants from civilians) is genuinely served for the conventional-military case; the theatrical element is the residual invocation of Article 4 formalism in cases where its underlying organizational assumptions plainly do not obtain. Accessibility collapse is moderate (0.40) — the criteria are legally fixed but their application is contested in every asymmetric conflict, so alternatives (hybrid or universal readings) remain live in adjudication and diplomacy, unlike a true mountain. Resistance is authored high (0.60) because this reading is actively and continuously contested by ICRC commentary, IHL scholarship, and state practice pressure toward Common Article 3 and customary-law floors.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries and the command structures they build sit at the beneficiary end of directionality: the criteria were negotiated by states, mirror the organizational form of state militaries, and give those militaries both a shield (reciprocal POW protection) and a sword (targeting/detention latitude against adversaries who cannot satisfy the criteria). Unprivileged belligerents, irregular militia fighters, and captured non-state combatants sit at the target end: they bear the classification cost, cannot alter their status after capture, and have no seat in the interpretive process. State military lawyers are agenda-setters who administer the boundary and thus experience the constraint as an operational tool rather than an imposition. ICRC and humanitarian monitors are excluded rather than positioned as beneficiaries or payers — they advocate but do not adjudicate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (distinguishing lawful combatants from civilians in interstate war) was real and remains partially live for conventional conflicts. But the state-centric reading's persistence in an era dominated by asymmetric and non-international conflict — where the criteria are least satisfiable by design — risks classifying a rule whose founding function has partly atrophied as though it still performs its original coordination work uniformly. This is precisely the tangled_rope signature: genuine coordination function for the symmetric case, coupled with asymmetric extraction that requires active enforcement (unilateral classification by the capturing power) to persist against sustained resistance from humanitarian monitors and IHL scholarship advocating the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    criteria_are_neutral_test_or_designed_exclusion,
    'Are the Article 4 criteria a neutral, generally applicable test for combatant status that happens to be harder for irregular forces to meet, or were they substantively shaped by state drafters to exclude the organizational forms that asymmetric and anti-colonial adversaries would necessarily adopt?',
    'Historical analysis of the 1949 diplomatic conference travaux préparatoires, including which state delegations proposed the four-part test and what conflicts (interstate vs. colonial/partisan resistance) were explicitly discussed as the target cases for inclusion or exclusion.',
    'If the criteria were substantively designed with exclusionary intent toward irregular/partisan forces, the tangled_rope classification is strongly supported and the beneficiary structure is not incidental. If the criteria emerged from a genuinely neutral attempt at operational clarity that happened to track pre-existing military organizational norms, the extractive element is better read as an unintended structural byproduct rather than designed asymmetry — though the effect on victims is unchanged either way.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_are_neutral_test_or_designed_exclusion, empirical, 'Whether Article 4''s exclusionary effect on irregular combatants was drafted intent or incidental structural byproduct.').

omega_variable(
    sibling_reading_foreclosure_or_coexistence,
    'Does adopting the state-centric reading as the operative legal standard for a given conflict logically foreclose simultaneous application of the universal_rights_reading''s floor, or can a state apply state-centric Article 4 status determination for POW purposes while still being bound by the universal reading''s Common-Article-3-plus-human-rights floor for baseline humane treatment?',
    'Comparative analysis of state practice and tribunal rulings (e.g., ICTY, ICJ) on whether Common Article 3 functions as a genuinely universal floor that operates independently of Article 4 POW-status determination, or whether states treat Article 4 exclusion as also excluding Common Article 3 coverage in practice.',
    'If Common Article 3 operates as an independent floor regardless of POW status, the state-centric and universal_rights readings coexist (state-centric governs a narrower privileged tier, universal governs the floor beneath it). If states treat POW-status exclusion as effectively excluding Common Article 3 protection too, the readings are in tension approaching foreclosure in practice, even if not in formal treaty text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_or_coexistence, conceptual, 'Whether state-centric POW exclusion forecloses or coexists with the universal-reading''s humane-treatment floor in practice.').

omega_variable(
    asymmetric_conflict_frequency_drift,
    'Is the rising extractiveness trajectory authored in this story genuinely tracking a documented shift in the proportion of armed conflicts that are asymmetric/non-international relative to interstate, or is it an assumption about conflict-type drift that has not been independently verified against conflict databases (e.g., UCDP)?',
    'Cross-reference the measurement trajectory against Uppsala Conflict Data Program or similar longitudinal conflict-type datasets for 1949–2025.',
    'If independently corroborated, the T17 mountain-extraction-accumulation-style rising trajectory is well-grounded as tracking real-world drift away from the rule''s founding assumptions. If not corroborated, the rising ε trajectory in this story is itself a contestable authored judgment rather than an established empirical trend.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_conflict_frequency_drift, empirical, 'Whether the authored rising-extractiveness trajectory reflects verified conflict-type drift or an unverified authorial assumption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2001, 0.24).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(gene_tr_t2025, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1949, 0.22).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1980, 0.33).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2001, 0.38).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(gene_be_t2025, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1949, 0.35).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1980, 0.44).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(gene_su_t2025, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__state_centric_reading, 0.1).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, hybrid_proportionality_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% Three sibling stories decompose the natural-language concept 'Geneva protective scope' per the ε-invariance principle: state_centric_reading (this file, ε≈0.42, tangled_rope, narrow protected class centered on conventional militaries), hybrid_proportionality_reading (conflict-type-scaled protection, intermediate ε expected), and universal_rights_reading (broadest protected class, lowest ε on the protective floor itself but highest constraint on state military operational latitude). Each is authored as an independent constraint with its own beneficiary/victim structure; they are linked here rather than merged because measuring 'Geneva protective scope' under the state-centric observable yields a structurally different ε, victim set, and classification than measuring it under the universal-rights observable — per DP-001, that divergence means these are different constraints, not one constraint under two measurement conventions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
