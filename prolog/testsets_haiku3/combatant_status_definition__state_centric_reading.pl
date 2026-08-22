% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status Gating (IHL Article 4)
 *   domain: legal/geopolitical
 *
 * SUMMARY:
 *   The Geneva Conventions establish combatant status as the gateway to
 *   lawful warfare immunity and prisoner-of-war protections. This constraint
 *   instantiates the state-centric reading: combatant status is reserved for
 *   formal state military organizations meeting Article 4 criteria
 *   (responsible command, fixed distinctive sign, arms carried openly,
 *   compliance with laws of war). Non-state armed groups are categorically
 *   excluded. The reading is contested by the National Liberation reading
 *   (which extends status to organized liberation movements under certain
 *   conditions per AP I Article 1(4)) and the Functional Protection reading
 *   (which grants all detainees Common Article 3 minimums regardless of
 *   combatant status). The claim/metric gap is deliberate: this reading is
 *   CLAIMED as tangled rope (coordination benefit: clear combatant/civilian
 *   distinction; extraction: state monopoly on legal warfare). The authored
 *   metrics describe substantially extractive operation (0.81 extractiveness,
 *   0.76 suppression), reflecting the tension between genuine coordination
 *   function and asymmetric protection benefiting state actors. The
 *   measurement series shows gradual intensification of extractiveness over
 *   the 75-year interval, indicating accumulating pressure on the
 *   state-origin test as non-state militarization increases.
 *
 * KEY AGENTS:
 *   - State military organizations: Primary beneficiary; automatic combatant status under Article 4; capture triggers Geneva III POW protections
 *   - Non-state armed groups: Primary payer; categorically excluded from combatant recognition; captured members face prosecution as war criminals or common criminals
 *   - Detained non-state fighters: Powerless payers; identity-locked (their status depends entirely on state recognition of their organization, not their individual conduct); subject to criminal prosecution for acts of war
 *   - State detaining authorities: Agenda setter; controls classification of combatant status and determines prosecutorial pathway for detainees
 *   - International humanitarian law bodies (ICRC, UN, courts): Observer seat; monitors compliance; documents alleged violations; can issue findings pressuring status reclassification
 *   - Sibling-reading advocates (National Liberation, Functional Protection): Excluded from state-centric enforcement; advocate alternative frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.81).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.76).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status Gating (IHL Article 4)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "legal/geopolitical").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '8f5578b3-57e2-4430-b2a3-cdcecc2441c7').
narrative_ontology:cs_kernel_codification('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', fixed_text).
narrative_ontology:cs_authority_grounding('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', lineage).
narrative_ontology:cs_interpretation_layer_present('8f5578b3-57e2-4430-b2a3-cdcecc2441c7').
narrative_ontology:cs_reading_relation('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', foundational, state_origin_constitutive_combatant_status).
narrative_ontology:cs_axiom_status(state_origin_constitutive_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', state_origin_constitutive_combatant_status, conventional).
narrative_ontology:cs_axiom('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', foundational, status_determines_protection_level).
narrative_ontology:cs_axiom_status(status_determines_protection_level, holdable).
narrative_ontology:cs_axiom_grounding('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', status_determines_protection_level, deontological).
narrative_ontology:cs_reference_frame('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', geneva_convention_article_4_state_combatant_definition).
narrative_ontology:cs_drift_state('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', contemporary_non_state_militarization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8f5578b3-57e2-4430-b2a3-cdcecc2441c7', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_military_organizations).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, detained_non_state_fighters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State militaries organized and uniformed under state sovereign authority automatically qualify for combatant status under Article 4. Their personnel captured in armed conflict receive prisoner-of-war status, triggering Geneva III protections: humane treatment, protection from prosecution for lawful acts of war, repatriation upon cessation. The constraint reserves this immunity for state actors exclusively.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_military_organizations, beneficiary,
    institutional, generational, arbitrage, global).

% Armed organizations lacking state status are categorically excluded from combatant status recognition under the state-centric reading, regardless of command structure, discipline, or humanitarian compliance. Their members captured are treated as unlawful combatants or civilians, prosecutable under domestic criminal law for acts of war; no immunity for military necessity, no POW protections, no repatriation guarantees.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_groups, payer,
    moderate, biographical, trapped, global).

% Individual fighters with non-state groups face detention without POW status, subjection to domestic criminal prosecution for armed activity, and absence of immunity protections. Their status depends entirely on whether their organizing authority is recognized as a state; their individual discipline, compliance with laws of war, or humanitarian conduct is irrelevant to the categorization gate.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, detained_non_state_fighters, payer,
    powerless, biographical, identity_locked, global).

% State governments control the enforcement of the combatant status threshold: they determine who qualifies as a lawful combatant under their reading of Article 4, decide whether detained persons receive POW status or criminal prosecution, and adjudicate humanitarian compliance claims. The reading centralizes classification authority in state hands.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_detaining_authorities, agenda_setter,
    institutional, generational, analytical, national).

% The International Committee of the Red Cross, UN treaty bodies, and humanitarian courts monitor compliance with the combatant status rule and investigate alleged violations of the protections it creates or withholds. They collect testimony from detained persons, state authorities, and documenting organizations, and can issue findings that pressure states to reclassify or extend protections.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_humanitarian_law_bodies, observer,
    institutional, generational, analytical, global).

% Advocacy networks promoting the national-liberation and functional-protection readings are structurally excluded from the state-centric reading's enforcement machinery. They challenge its core premise and argue for alternative classification schemes, but have no seat in the determination of which combatants qualify for status under this reading's framework.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, sibling_readings_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, state_military_organizations).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, legible combatant-noncombatant distinction for purposes of granting immunity and regulating lawful warfare: state militaries are authorized combatants, entitled to engage in hostilities and immune from prosecution for military necessity; the constraint protects civilians from targeting and protects captured combatants from dual jeopardy.
% TRANSFER_FUNCTION: Moves legal immunity, humanitarian protections, and war-conduct recognition from state-organized military actors to themselves (positive transfer); withholds these protections from non-state armed groups and channels their detained members toward criminal prosecution under domestic law (negative transfer). The state apparatus retains sole authority to classify which entities qualify.
% ABSENT_VOICES: Non-state fighters and armed groups fighting occupation, colonialism, or oppression have no seat in the determination of their own status under this reading. National liberation movements, resistance organizations, and non-state humanitarian advocates argue the reading is arbitrary and deny recognition to organized, disciplined combatants; this reading excludes them from the classification conversation by definition.
% DISAPPEARANCE_RATIONALE: If the state-centric combatant status gate vanished, the Geneva system would collapse into either universal human-minimum protections (functional reading adoption) or inclusion of qualified non-state combatants (national liberation reading). The immediate consequence: detaining authorities would lose the legal basis for dual criminal-military prosecution of non-state fighters; non-state armed groups would gain recognition pathways; the distinction between lawful and unlawful combatants would revert to functional criteria rather than organizational origin.
% FOUNDING_PROBLEM: Early laws of war (Hague, initial Geneva) operated in a world of state-organized armies. The combatant status rule was designed to distinguish lawful from unlawful warfare by tying legal combatancy to state authority structures. The founding problem: how to protect soldiers and civilians when organized violence can be distinguished into state-authorized and private/brigand violence.
% FOUNDING_PROBLEM_CORROBORATION: State militaries and Western governments affirm the founding problem is live: distinguishing state combatants from non-state actors, insurgents, and terrorists remains essential. Humanitarian organizations, international courts, and national liberation advocates dispute this: they attest the founding problem's distinction has blurred (state proxies, non-state organizational maturity, functional indistinguishability); the rule persists to protect state monopoly over warfare, not to serve the original protective function.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the constraint creates an asymmetric legal regime: state actors gain immunity for military necessity while non-state actors face criminal prosecution for identical acts. This is not incidental—it is the reading's structural effect. The measurement trajectory (0.68→0.81 over 75 years) reflects the growing tension between the state-origin criterion and the functional militarization of non-state groups: as non-state armed organizations become more professional, disciplined, and organized, the arbitrary exclusion intensifies—the constraint's extractive force increases because the functional basis for the distinction weakens. Suppression is high (0.76) because maintaining the state-origin gate requires active enforcement: states must resist recognition of functionally qualified non-state groups, international bodies must defer to state determinations of status, and alternative readings (National Liberation, Functional) must be containable. Theater ratio (0.42, moderate-low) reflects that while the security coordination function is genuine (distinguishing lawful combatants serves humanitarian purposes), a growing share of enforcement energy defends the state monopoly rather than the original protective logic. The reading claims Tangled Rope because: (1) COORDINATION FUNCTION—genuine: establishing a clear combatant/civilian distinction prevents targeting of civilians and provides captured combatants protections; (2) ASYMMETRIC EXTRACTION—genuine: state militaries benefit from automatic recognition and full protections; non-state fighters bear the cost of categorical exclusion and criminal prosecution; (3) ACTIVE ENFORCEMENT—genuine: states actively resist non-state status claims, international bodies apply the rule selectively, and the constraint persists only through institutional maintenance. Without enforcement, the distinction would collapse as non-state groups achieved functional parity with state militaries.
 *
 * PERSPECTIVAL GAP:
 *   State military seats and state authorities compute this as Rope (pure coordination—a clear rule benefiting all who can qualify). Non-state fighter seats compute this as Snare (pure extraction—categorical exclusion with no functional basis). This divergence is exactly what the per-seat classification apparatus detects. The reading's claim is Tangled Rope because both functions coexist structurally; the divergent computed types are the evidence the claim is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   State military organizations are the structural beneficiaries: they hold institutional power, have arbitrage-level exit options (can redefine themselves within state apparatus), and are explicitly named as beneficiaries (automatic status, full protections). Their directionality is near 0.0 (full beneficiary—the constraint subsidizes their legal position). Non-state armed groups are structural targets: moderate power (organized but unrecognized), trapped or constrained exit (they can cease fighting or dissolve, but cannot gain status within the constraint's framework), and are explicitly named as victims (categorical exclusion, no immunity). Their directionality is near 1.0 (full target—the constraint extracts maximum cost). Detained fighters are the most extreme: powerless, identity-locked (their status depends entirely on whether an external authority reclassifies their group), biographical time horizon (their detention is measured in years), and global scope (the rule applies everywhere). Their d approaches 1.0 (the constraint extracts asymmetric legal jeopardy). The asymmetry drives effective extraction χ: high extraction for powerless, trapped agents at global scope; near-zero extraction for institutional agents with arbitrage options. The reading satisfies the Tangled Rope criterion: both coordination (combatant/civilian distinction) and extraction (state monopoly on legal warfare) are present, asymmetric, and enforced.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing lawful state-authorized warfare from brigandage and piracy) was live and functional when combatant status rules were written. The status is NOW CONTESTED: state militaries affirm the founding problem is live and the rule solves it. National Liberation advocates argue the problem is dead (non-state groups are now organized enough to qualify) and the rule persists as state rent collection. Humanitarian bodies argue the founding problem was solved decades ago and the constraint now primarily defends state monopoly. The measurement trajectory shows extractiveness rising while the founding problem's status falls—a classic mandatrophy signature: the constraint's original protective function is increasingly decoupled from its enforcement logic. The constraint persists despite the founding problem's obsolescence in non-state warfare because states benefit from maintaining the monopoly. This is the exact pattern mandatrophy detection identifies: coordination function atrophied; extraction mechanism hardened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_definition_fluidity,
    'What qualifies as a ''state'' for combatant status purposes? Is the test purely sovereign recognition, de facto territorial control, UN membership, or some hybrid? How does the constraint respond to state collapse, proxy warfare, or contested sovereignty?',
    'Close analysis of case law from international courts, state practice in proxy conflicts, and UN/ICRC guidance on recognition of combatant entities in grey-zone scenarios (failed states, occupation, civil war with foreign support).',
    'If the state definition is tight and formal (UN membership, traditional sovereignty), the extraction is stable but may not track functional militarization (state proxies evade the gate). If loose (de facto control), the constraint becomes contested and depends on recognizer authority. The reading''s coherence rides on this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_definition_fluidity, conceptual, 'Ambiguity in what counts as state military organization for Article 4 purposes').

omega_variable(
    organized_non_state_group_recognition,
    'Are non-state armed groups with clear command structure, uniforms, discipline, and humanitarian compliance genuinely indistinguishable from state militaries in functional terms? Does the state-centric reading deny status to these groups despite meeting functional combatant criteria?',
    'Comparative analysis of state practice and humanitarian doctrine: do state detaining authorities ever grant combatant-equivalent protections to organized non-state groups? Does the National Liberation reading''s Article 1(4) expansion demonstrate that functional qualification is achievable by non-state actors?',
    'If non-state groups demonstrably meet all functional criteria for lawful combatancy and are still excluded by this reading, the constraint shifts from coordination (distinguishing lawful from unlawful) to pure extraction (protecting state monopoly on legal warfare). This is the kernel dispute — the reading''s core claim hinges on whether the state-origin test is necessary for coordination or is ideological cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organized_non_state_group_recognition, empirical, 'Whether functional combatant criteria can be met by non-state groups').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the exclusion of non-state combatants from status maintained by structural barriers (law, treaty, state control of recognition) or by internalized acceptance (non-state groups abandon claims to status, humanitarian advocates accept the rule as legitimate)?',
    'Post-recognition trajectory analysis: in cases where non-state groups have gained de facto combatant status (AP I Article 1(4) signatories, some liberation movements), did suppression of alternative claims persist, or did advocates shift framing when the structural barrier weakened?',
    'If suppression is purely structural, removal of the state-origin gate would immediately shift status landscape and protections. If partially internalized (non-state groups accept delegitimacy), persistence would require active re-education and norm reinforcement after structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression of non-state combatant recognition').

omega_variable(
    reading_kernel_foundational_premise_contest,
    'Does this reading genuinely instantiate a coherent reading of the Geneva combatant-status kernel, or does it collapse under the functional indistinguishability of state and organized non-state militaries?',
    'Examine statements from ICRC, UN special rapporteurs, and international court opinions that affirm or reject the state-origin rule as constitutive versus evidentiary.',
    'If the state-origin test is shown to be evidentiary (functional criteria can substitute), the reading forecloses to the National Liberation reading. If it is constitutive (state origin is necessary, not just sufficient), the reading coexists with both siblings — it is one live option in a contested landscape. The classification depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_foundational_premise_contest, conceptual, 'Whether the state-centric reading''s core premise forecloses or coexists with sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__state_centric_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(comb_tr_t10, combatant_status_definition__state_centric_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(comb_tr_t25, combatant_status_definition__state_centric_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(comb_tr_t40, combatant_status_definition__state_centric_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(comb_tr_t55, combatant_status_definition__state_centric_reading, theater_ratio, 55, 0.41).
narrative_ontology:measurement(comb_tr_t75, combatant_status_definition__state_centric_reading, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__state_centric_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(comb_be_t10, combatant_status_definition__state_centric_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(comb_be_t25, combatant_status_definition__state_centric_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(comb_be_t40, combatant_status_definition__state_centric_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(comb_be_t55, combatant_status_definition__state_centric_reading, base_extractiveness, 55, 0.8).
narrative_ontology:measurement(comb_be_t75, combatant_status_definition__state_centric_reading, base_extractiveness, 75, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__state_centric_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(comb_su_t10, combatant_status_definition__state_centric_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(comb_su_t25, combatant_status_definition__state_centric_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(comb_su_t40, combatant_status_definition__state_centric_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(comb_su_t55, combatant_status_definition__state_centric_reading, suppression_requirement, 55, 0.75).
narrative_ontology:measurement(comb_su_t75, combatant_status_definition__state_centric_reading, suppression_requirement, 75, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest on combatant status under the Geneva Conventions. The state-centric reading (this file) is distinguished from the national-liberation reading by whether non-state armed groups can achieve combatant status through functional qualification; from the functional-protection reading by whether combatant status determines the level of humanitarian protections. All three readings share the referent (the standing Geneva system for combatant status) but author different epsilon values: state-centric reading sees high extraction (0.81) because state monopoly; NL reading sees lower extraction if organized non-state groups qualify; FP reading approaches zero extraction if protections are status-independent. The three stories form a constraint family linked via network.affects_constraints; each reading's ε is independent of the others' (per ε-invariance principle). Empirical test: state practice on recognition of non-state combatants; international court determinations on protection thresholds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__state_centric_reading, powerless, 0.95).
constraint_indexing:directionality_override(combatant_status_definition__state_centric_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
