% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The Second Amendment's text ('A well regulated Militia, being necessary
 *   to the security of a free State, the right of the people to keep and bear
 *   Arms, shall not be infringed') is contested as a kernel—the same fixed
 *   text is read differently by different constitutional traditions and
 *   factions. This constraint story is ONE READING: the individual-right
 *   reading asserts that the operative clause ('the right of the people to
 *   keep and bear Arms') stands independent of the militia clause and
 *   protects personal firearm ownership for self-defense without requiring
 *   civic participation or state permission. This reading was judicially
 *   affirmed as the binding constitutional law by the Supreme Court in
 *   District of Columbia v. Heller (2008). The constraint operates by
 *   establishing individual gun owners as beneficiaries (their right is
 *   protected), disarmed populations as victims (categorical exclusions are
 *   permitted), gun-violence-prevention constituencies as constrained payers
 *   (their regulatory authority is limited), and collective-security and
 *   originalist readings as excluded from institutional power (their
 *   alternative interpretations are not the law). The claim and metrics
 *   diverge intentionally: this reading CLAIMS to be rope (genuine
 *   coordination around individual self-defense), but the authored metrics
 *   describe tangled_rope operation—there is real coordination (self-defense
 *   protection) but also real extraction (suppression of alternative
 *   readings, categorical disarmament, constraint on permit requirements).
 *   The engine measures that divergence, which is exactly the signal the
 *   corpus needs to detect: a reading that claims coordination but operates
 *   with substantial suppression and resistance is either a tangled_rope that
 *   coordinates while extracting, or a snare disguised as coordination. The
 *   five omegas route the committer structure (kernel contest, reading
 *   alternatives, historical ambiguity, functional boundaries) to unresolved
 *   questions rather than settled classification.
 *
 * KEY AGENTS:
 *   - Individual gun owners: organized beneficiaries with identity-locked exit; their constitutional identity is tied to the right to carry
 *   - Disarmed populations (felons, domestic abusers): powerless victims; excluded from the protection by categorical law
 *   - Gun-violence-prevention constituencies: organized payers; their regulatory capacity is constitutionally constrained by this reading
 *   - Supreme Court interpreters: institutional agenda-setters; their 2008 Heller decision established this reading as binding constitutional law
 *   - State regulators: institutional agenda-setters constrained by the reading; they retain authority over manner and place but not over categorical bans
 *   - Collective-security and originalist civic-virtue advocates: excluded; their alternative readings are not the law despite scholarly presence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.62).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2').
narrative_ontology:cs_kernel_codification('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', fixed_text).
narrative_ontology:cs_authority_grounding('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', lineage).
narrative_ontology:cs_interpretation_layer_present('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2').
narrative_ontology:cs_reading_relation('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', second_amendment_text__originalist_civic_virtue_reading, influences).
narrative_ontology:cs_axiom('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', foundational, operative_clause_independence).
narrative_ontology:cs_axiom_status(operative_clause_independence, holdable).
narrative_ontology:cs_axiom_grounding('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', operative_clause_independence, conventional).
narrative_ontology:cs_axiom('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', foundational, individual_right_to_keep_and_bear_arms).
narrative_ontology:cs_axiom_status(individual_right_to_keep_and_bear_arms, holdable).
narrative_ontology:cs_axiom_grounding('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', individual_right_to_keep_and_bear_arms, deontological).
narrative_ontology:cs_reference_frame('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', individual_self_defense_right).
narrative_ontology:cs_drift_state('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', contemporary_public_health_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8d04e454-d7c1-43b3-8a2b-0a04fd6d56f2', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, self_defense_advocates).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, disarmed_populations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, gun_violence_prevention_constituencies).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, natural_right_to_self_defense).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, individual_constitutional_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim constitutional protection for firearm ownership independent of militia participation. Organize around the reading that the operative clause ('the right of the people to keep and bear arms') stands alone, unqualified by the preamble militia clause. Their exit from the constraint is highly constrained: leaving the jurisdiction to avoid firearms restrictions is identity-locked because Second Amendment protection is central to their constitutional identity.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% Argue that personal self-defense—the protection of self, family, and property against crime—is the core function the Second Amendment protects. They include legal scholars, constitutional judges adopting this reading, and grassroots defensive-gun-use organizations. Their institutional and cultural resources allow relative mobility; they can organize politically and litigate nationally.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, self_defense_advocates, beneficiary,
    powerful, generational, mobile, national).

% Are prohibited from firearm ownership by law: convicted felons, individuals subject to domestic abuse restraining orders, and those deemed mentally ill under statute. This reading of the Second Amendment as protecting individual gun rights has been interpreted by courts to permit these categorical exclusions. They bear the cost of disarmament without the protective benefit, and their exclusion is sustained by the reading's incorporation of the felon/danger-exclusion doctrine.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, disarmed_populations, payer,
    powerless, biographical, trapped, national).

% Advocate for stricter gun regulations (universal background checks, permit requirements, assault-weapon bans) on grounds that firearm access drives homicide and suicide mortality. This reading of the Second Amendment as protecting individual gun rights has been interpreted to constrain their regulatory capacity: the constraint makes it constitutionally difficult to impose the permit requirements and purchase restrictions they argue for.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_violence_prevention_constituencies, payer,
    organized, biographical, constrained, national).

% Administer firearms law within the constitutional bounds set by courts interpreting the Second Amendment. Under the individual-right reading, they retain authority to regulate manner and place (licensing of carrying, restrictions on gun presence in schools/courthouses) but lose authority to ban entire categories of lawful arms or to condition ownership on civic need or militia participation. They are constrained by the reading but not eliminated from the regulatory space.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Interpret the Second Amendment text and decide which reading constrains lower courts and legislatures. The individual-right reading was judicially affirmed in District of Columbia v. Heller (2008) as the binding constitutional reading at the Supreme Court level. They set the agenda by resolving the kernel contest in favor of this reading; they also observe because the Constitution's text and history constrain their interpretive choices.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, supreme_court_interpreters, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, supreme_court_interpreters, observer).

% Would argue that the militia clause conditions the operative clause: the right to keep and bear arms is conditional on service in a well-regulated militia, and states retain broad police power to regulate firearms for public safety. They are structurally excluded from the constitutional conversation by the supremacy of the individual-right reading at the Supreme Court level; their alternative reading is not the law.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, collective_security_reading_advocates, excluded,
    organized, biographical, constrained, national).

% Argue that the Founding-era militia was the universal armed citizenry, and the Second Amendment protects that capacity—a collective armed populace, not atomized individual rights divorced from civic participation. They are excluded from institutional power by the Supreme Court's adoption of the individual-right reading, though they remain present in legal scholarship and lower-court disagreement.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, originalist_civic_virtue_advocates, excluded,
    moderate, biographical, constrained, national).

% Provide historical evidence and analysis about founding-era intent, militia practice, and contemporary conditions. Historians and textual scholars disagree about which reading best fits the record; their contributions are diagnostic but not binding in law.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, historical_interpreters, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear constitutional boundary around personal firearm ownership: individuals may keep arms for self-defense without needing to justify their choice through militia service or state permission. This solves a coordination problem among judges and legislatures about what the Constitution permits—it gives a unitary answer ('personal right') rather than leaving states to decide whether firearms are instruments of collective security or individual liberty.
% TRANSFER_FUNCTION: Transfers interpretive authority from state legislatures (who had broad police power to regulate arms) to federal courts interpreting the Second Amendment. Transfers protective status from the concept of 'collective militia readiness' to the concept of 'individual gun ownership.' Transfers the burden of disarmament enforcement away from individualized dangerousness determinations and onto categorical exclusions (felon status, domestic abuse conviction).
% ABSENT_VOICES: Disarmed populations (felons, domestic abusers, those deemed mentally ill) are structurally absent from the constitutional conversation at the time the reading is established; their exclusion is presumed, not argued. Gun-violence prevention constituencies have institutional voice (organized advocacy, legislative presence) but lose interpretive authority over what the Constitution permits them to regulate. Collective-security reading advocates and originalist civic-virtue advocates are excluded from the judicial consensus, though present in scholarly debate.
% DISAPPEARANCE_RATIONALE: If the individual-right reading of the Second Amendment vanished—replaced by the collective-security reading—the constitutional constraint on state firearms regulation would invert: states would regain broad authority to condition gun ownership on militia service (or its functional equivalent, civic participation), to require permits and justify purchases, and to impose categories of firearm bans. Individual gun owners would face categorical restrictions they currently avoid; gun-violence prevention constituencies would gain regulatory capacity. The political and constitutional landscape would reorganize around competing state-level frameworks for arms regulation rather than a federally protected individual right.
% FOUNDING_PROBLEM: The Founding generation feared standing armies and sought to ensure that armed citizens could resist tyranny and protect themselves. The individual-right reading grounds itself in the problem of personal security and civil liberty: individuals need the capacity to defend themselves, their families, and their property without dependence on the state. The right was understood as a check on government power.
% FOUNDING_PROBLEM_CORROBORATION: Individual-right advocates cite founding-era texts and historical treatises (Blackstone, state constitutions like Pennsylvania's 1776 declaration of rights) supporting individual gun ownership. Collective-security and originalist-civic-virtue advocates cite militia statutes, founding debates emphasizing militia over individual arms, and the militia clause itself as evidence the founding problem was collective defense, not atomized self-defense. Historians disagree; no external corroboration from outside the benefiting and contending parties settles the historical question definitively. This is the core structural ambiguity: the founding text is ambiguous, and each reading claims historical grounding that the other disputes.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.58) reflects that this reading permits categorical disarmament of defined populations (felons, domestic abusers) without individualized dangerousness determination, and constrains legislators from imposing permit requirements or assault-weapon bans they argue would reduce gun violence. This is extraction from the gun-violence-prevention constituencies and disarmed populations, who bear the cost of either disarmament or constitutional limitation on regulation. The suppression score (0.62) reflects active judicial enforcement required to keep alternative readings out of constitutional discourse: courts must continually reject collective-security and originalist readings in favor of the individual-right reading. The theater_ratio (0.41) is moderate: the reading genuinely protects individual gun ownership (functional) but increasingly performs the role of defending that protection against mounting empirical challenges about gun violence (performative). The accessibility_collapse (0.68) reflects that once the individual-right reading is established as constitutional law, alternatives (including reasonable regulations gun-violence advocates favor) become constitutionally inaccessible—judges cannot simply choose stricter gun laws without overruling binding precedent. The resistance (0.74) reflects sustained organized pushback from gun-violence-prevention constituencies and public health advocates who dispute the reading's interpretation and its policy consequences. The measurement series shows extractiveness and suppression rising from the Heller decision (t=0 projected) through the years following (t=3 to t=18 observed), with theater rising as the reading becomes institutionally entrenched and performative enforcement increases, while extractiveness plateaus at moderate levels (not high extraction like a classic snare, but real extraction nonetheless). The temporal trajectory models a reading that began with genuine novelty (2008 Heller reversal of prior doctrine) and has consolidated into institutionalized constraint, with suppression required to keep dissenters (courts adopting collective-security readings) out of the constitutional conversation.
 *
 * PERSPECTIVAL GAP:
 *   See above for full perspectival gap analysis—the gap is the key feature of this reading's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations and exit options generate directionality as follows: (1) Individual gun owners have organized power, biographical time horizon, and constrained-but-mobile exit (they can relocate to gun-friendly jurisdictions or engage in civil disobedience/constitutional challenge). Their declared role is beneficiary (they collect the right to carry). Directionality is LOW (~0.2–0.3), treating them as subsidized by the constraint. (2) Disarmed populations (felons, domestic abusers) have powerless power, no meaningful exit (removal of disarmament status requires gubernatorial action or rare expungement), and trapped exit. They are victims. Directionality is HIGH (~0.8–0.9), treating them as targets of extraction. (3) Gun-violence-prevention constituencies have organized power, biographical time horizon, and constrained exit (they can advocate politically and litigate, but the constraint limits their regulatory options). They are payers (bear costs of regulatory limits). Directionality is MODERATE-HIGH (~0.6–0.7), treating them as moderately extracted from. (4) State regulators have institutional power, generational horizon, constrained exit (the Supreme Court decision binds them). They are agenda-setters but also constrained by the binding precedent. Directionality is MODERATE (~0.5), treating them as symmetric to the arrangement they administer. No overrides are needed; the derivation chain from declared roles and exit options produces the right directionality for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy analysis asks: has the founding problem (personal security and resistance to tyranny, as understood by the individual-right reading) outlived its function, but the constraint persists anyway? The answer is contested and routed to omega_1 (militia_clause_scope_ambiguity) and omega_2 (founding_problem_historical_certainty). From the beneficiary seat, the founding problem remains live: individual self-defense is still necessary and valued. From the payer seats, the founding problem is no longer the primary driver of the constraint—instead, the constraint persists as institutional inertia and judicial precedent, defended by rhetoric about self-defense but increasingly performing the role of blocking public-health regulations. This is not classic mandatrophy (where the founding problem is dead and the constraint persists theatrically), but it is a drift toward mandatrophy: extractive operation without the founding problem remaining salient to the beneficiaries' actual use. The theater_ratio rise (from 0.25 to 0.41 over the interval) models increasing performative enforcement as the Supreme Court repeatedly reaffirms the individual-right reading despite mounting counter-evidence about gun violence. The suppression_requirement rise (from 0.50 to 0.62) models courts actively suppressing alternative readings and lower-court judges who might adopt collective-security readings. This pattern is consistent with a constraint that is becoming more about defending the reading itself (performance, suppression) and less about the concrete coordination it claims to enable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_scope_ambiguity,
    'Does the militia clause (''A well regulated Militia, being necessary to the security of a free State'') condition the operative clause (''the right of the people to keep and bear Arms, shall not be infringed''), or does it merely provide one historical context for understanding why the right exists?',
    'Linguistic and historical analysis: does the prefatory clause modify the operative clause (in which case state militia interest limits the operative right), or does it stand as a separate statement of motivation (in which case the operative clause stands alone)? Examination of founding-era state constitutions and comparable textual structures.',
    'If the militia clause conditions the operative clause, the collective-security reading forecloses this reading and the constraint inverts: states regain broad regulatory authority. If the militia clause is merely prefatory (as this reading asserts), the individual-right reading stands and disarmed populations remain excluded from protection. This is THE core structural ambiguity the kernel contest hinges on.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_clause_scope_ambiguity, conceptual, 'Whether the prefatory militia clause conditions or merely contextualizes the operative clause.').

omega_variable(
    founding_problem_historical_certainty,
    'What was the historical founding problem the Second Amendment was designed to solve: individual self-defense and resistance to tyranny, or collective militia readiness and prevention of standing armies?',
    'Historical scholarship: records of founding debates, contemporary commentaries, state-level precedents (state constitutions, colonial militia laws), and the framing documents. However, the historical record is itself contested and interpreted differently by different schools of historical analysis.',
    'If the founding problem was individual self-defense, this reading''s legitimacy is strengthened and the constraint''s beneficiary framing is vindicated. If the founding problem was collective militia readiness, the originalist-civic-virtue reading and collective-security reading gain ground and the individual-right reading appears as a modern imposition on the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_historical_certainty, empirical, 'Whether historical evidence confirms individual self-defense or collective militia readiness as the founding problem.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the suppression measured here (0.62)—the active enforcement required to maintain categorical disarmament of felons and domestic abusers, and to resist permit and registration requirements—a necessary coordination cost of the individual-right framework, or is it extractive overhead hiding behind a coordination claim?',
    'Counterfactual analysis: in a world where the individual right is recognized but more permissive disarmament standards are adopted (dangerousness individualized rather than categorical), would the constraint still coordinate around personal self-defense, or would it collapse into incoherence? If it coheres, the suppression is extractive. If it requires the categorical exclusions, the suppression is coordination cost.',
    'If suppression is extraction, the constraint is better classified as snare than tangled_rope, and the beneficiary narrative (gun owners gain protection) masks the reality of administrative burden imposed on disarmed populations. If suppression is coordination cost, the tangled_rope classification holds and the arrangement genuinely coordinates around a real individual-right framework while imposing differential costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Whether the measured suppression serves coordination or hides extraction.').

omega_variable(
    self_defense_functionality,
    'Does the individual-right reading actually enable self-defense, or does it protect the right to carry while leaving self-defense effectiveness determined by state law on justified use, defensive-gun-use doctrine, and civil liability?',
    'Legal analysis of self-defense statutes in individual-right jurisdictions: do they permit broad defensive use, or do they impose liability and criminal risk even when carrying is constitutionally protected? Empirical data on defensive-gun-use outcomes and prosecution patterns.',
    'If the right to carry is decoupled from the right to use defensively, the constraint''s stated coordination function (personal self-defense protection) is partially theater, and the true function is protection of gun ownership as status/identity rather than as functional self-defense tool. This would raise theater_ratio and lower the coordination claim''s credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_defense_functionality, empirical, 'Whether individual-right legal frameworks actually enable or merely symbolically protect self-defense.').

omega_variable(
    kernel_reading_contest_structure,
    'Which of the three readings of the second_amendment_text kernel represents the binding constitutional law, and do the other two readings remain available to future courts, or are they foreclosed by the current supremacy of the individual-right reading?',
    'Constitutional amendment, Supreme Court reversal of precedent (Heller and successors), or congressional action would be required to change which reading is binding. The collective-security and originalist-civic-virtue readings are currently subordinate but not logically foreclosed—they could be revived by a future court willing to overrule prior precedent.',
    'The current enforcement of this reading (base_extractiveness 0.58, suppression 0.62) is contingent on its continued supremacy. If the Supreme Court were to adopt the collective-security reading or originalist-civic-virtue reading, the constraint would invert: gun-violence-prevention constituencies would gain regulatory authority, individual gun owners would face new restrictions, and disarmed populations might see categorical exclusions subject to more individualized review.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, preference, 'Which reading of the kernel remains binding in future constitutional interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__individual_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(seco_tr_t0, projected).
narrative_ontology:measurement(seco_tr_t3, second_amendment_text__individual_right_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement_basis(seco_tr_t3, observed).
narrative_ontology:measurement(seco_tr_t6, second_amendment_text__individual_right_reading, theater_ratio, 6, 0.37).
narrative_ontology:measurement_basis(seco_tr_t6, observed).
narrative_ontology:measurement(seco_tr_t9, second_amendment_text__individual_right_reading, theater_ratio, 9, 0.39).
narrative_ontology:measurement_basis(seco_tr_t9, observed).
narrative_ontology:measurement(seco_tr_t12, second_amendment_text__individual_right_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(seco_tr_t12, observed).
narrative_ontology:measurement(seco_tr_t15, second_amendment_text__individual_right_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(seco_tr_t15, observed).
narrative_ontology:measurement(seco_tr_t18, second_amendment_text__individual_right_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement_basis(seco_tr_t18, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__individual_right_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(seco_be_t0, projected).
narrative_ontology:measurement(seco_be_t3, second_amendment_text__individual_right_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement_basis(seco_be_t3, observed).
narrative_ontology:measurement(seco_be_t6, second_amendment_text__individual_right_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement_basis(seco_be_t6, observed).
narrative_ontology:measurement(seco_be_t9, second_amendment_text__individual_right_reading, base_extractiveness, 9, 0.57).
narrative_ontology:measurement_basis(seco_be_t9, observed).
narrative_ontology:measurement(seco_be_t12, second_amendment_text__individual_right_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(seco_be_t12, observed).
narrative_ontology:measurement(seco_be_t15, second_amendment_text__individual_right_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(seco_be_t15, observed).
narrative_ontology:measurement(seco_be_t18, second_amendment_text__individual_right_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement_basis(seco_be_t18, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__individual_right_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(seco_su_t0, projected).
narrative_ontology:measurement(seco_su_t3, second_amendment_text__individual_right_reading, suppression_requirement, 3, 0.56).
narrative_ontology:measurement_basis(seco_su_t3, observed).
narrative_ontology:measurement(seco_su_t6, second_amendment_text__individual_right_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement_basis(seco_su_t6, observed).
narrative_ontology:measurement(seco_su_t9, second_amendment_text__individual_right_reading, suppression_requirement, 9, 0.61).
narrative_ontology:measurement_basis(seco_su_t9, observed).
narrative_ontology:measurement(seco_su_t12, second_amendment_text__individual_right_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(seco_su_t12, observed).
narrative_ontology:measurement(seco_su_t15, second_amendment_text__individual_right_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(seco_su_t15, observed).
narrative_ontology:measurement(seco_su_t18, second_amendment_text__individual_right_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(seco_su_t18, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__individual_right_reading, 0.18).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, firearm_permitting_state_authority).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, assault_weapon_classification_regime).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, domestic_abuse_firearm_restriction_scope).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel second_amendment_text. The collective_security_reading and originalist_civic_virtue_reading are sibling constraints with different beneficiary/victim structures and different computed types. All three readings are linked via network.affects_constraints to show the kernel contest and the structural dependencies. The individual-right reading (this constraint) influences the other readings by establishing the judicial precedent that constrains them; it also affects downstream constraints about permitting authority and classification regimes because those constraints must operate within the bounds this reading sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
