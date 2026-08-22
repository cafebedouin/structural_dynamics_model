% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity as Protective Scaffold for Vigorous Law Enforcement
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This story instantiates the protective_scaffold reading of the qualified
 *   immunity kernel: the doctrine as a necessary shield allowing officers to
 *   act decisively in legally ambiguous, high-stakes situations without fear
 *   of personal financial ruin from good-faith errors, and as a mechanism
 *   protecting departments' ability to recruit and retain officers. This is
 *   one of three structurally distinct constraints sharing the qualified
 *   immunity kernel — the accountability_void_reading (extraction mechanism
 *   guaranteeing impunity) and constitutional_fidelity_reading (illegitimate
 *   judicial fabrication) are separate constraints with their own ε values,
 *   not alternative framings of this one. Per the ε-invariance principle,
 *   this story does not hedge across those readings; it authors a single,
 *   stable ε for the standing arrangement as the protective-scaffold reading
 *   sees it.
 *
 * KEY AGENTS:
 *   - law_enforcement_officers: Primary beneficiary (organized/constrained) — shielded from personal liability
 *   - municipal_governments: Institutional beneficiary (institutional/arbitrage) — avoids officer retention crisis, manages indemnification
 *   - police_unions: Organized beneficiary and agenda-setter (organized/mobile) — lobbies for preservation
 *   - courts: Agenda-setter (institutional/analytical) — applies the clearly-established-law test
 *   - constitutional_violation_survivors: Primary target (powerless/trapped) — bears uncompensated harm
 *   - civil_rights_plaintiffs: Target (powerless/trapped) — faces dismissal without merits review
 *   - civil_rights_attorneys: Excluded voice (moderate/constrained) — screens out unviable cases invisibly
 *   - legal_scholars: Analytical observer — assesses empirical deterrence claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.52).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.58).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity as Protective Scaffold for Vigorous Law Enforcement").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, '71ea62b2-4cb0-4d99-8da8-34e60db53537').
narrative_ontology:cs_kernel_codification('71ea62b2-4cb0-4d99-8da8-34e60db53537', distributed).
narrative_ontology:cs_authority_grounding('71ea62b2-4cb0-4d99-8da8-34e60db53537', practice).
narrative_ontology:cs_interpretation_layer_present('71ea62b2-4cb0-4d99-8da8-34e60db53537').
narrative_ontology:cs_reading_relation('71ea62b2-4cb0-4d99-8da8-34e60db53537', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('71ea62b2-4cb0-4d99-8da8-34e60db53537', qualified_immunity_doctrine__constitutional_fidelity_reading, influences).
narrative_ontology:cs_axiom('71ea62b2-4cb0-4d99-8da8-34e60db53537', foundational, officer_decisional_protection_necessary_for_effective_policing).
narrative_ontology:cs_axiom_status(officer_decisional_protection_necessary_for_effective_policing, holdable).
narrative_ontology:cs_axiom_grounding('71ea62b2-4cb0-4d99-8da8-34e60db53537', officer_decisional_protection_necessary_for_effective_policing, instrumental).
narrative_ontology:cs_axiom('71ea62b2-4cb0-4d99-8da8-34e60db53537', secondary, clearly_established_standard_appropriately_screens_ambiguous_conduct).
narrative_ontology:cs_axiom_status(clearly_established_standard_appropriately_screens_ambiguous_conduct, holdable).
narrative_ontology:cs_axiom_grounding('71ea62b2-4cb0-4d99-8da8-34e60db53537', clearly_established_standard_appropriately_screens_ambiguous_conduct, empirically_contingent).
narrative_ontology:cs_reference_frame('71ea62b2-4cb0-4d99-8da8-34e60db53537', harlow_good_faith_immunity_standard).
narrative_ontology:cs_drift_state('71ea62b2-4cb0-4d99-8da8-34e60db53537', post_2010_circuit_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('71ea62b2-4cb0-4d99-8da8-34e60db53537', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, police_unions).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_plaintiffs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform split-second judgment calls in ambiguous, high-stakes encounters and are shielded from personal civil liability unless their conduct violated a 'clearly established' right that a reasonable officer would have known. This reading holds the shield is what allows them to act decisively rather than hesitating out of fear of personal bankruptcy from good-faith mistakes made under pressure.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers, beneficiary,
    organized, biographical, constrained, national).

% Fund police departments, negotiate with unions, and benefit from a doctrine that keeps recruitment and retention viable by capping the personal financial exposure of officers. Lobbies to preserve the doctrine and shapes indemnification practices that interact with it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments, agenda_setter).

% Advocate for the doctrine's preservation and expansion in legislatures and courts, framing it as essential to officer morale and willingness to make aggressive but lawful interventions. Actively lobbies against reform efforts.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, police_unions, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, police_unions, agenda_setter).

% Apply the two-step 'clearly established law' framework, determining case by case whether prior precedent gave sufficiently specific notice. Retain broad discretion over how narrowly or broadly to define the right at stake, which determines outcomes in individual suits.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Have suffered an actual constitutional violation (excessive force, unlawful search, wrongful arrest) but find their civil suit dismissed before trial because no prior case matched their facts closely enough to have 'clearly established' the right. Bear the full cost of the harm with no monetary remedy and no judicial finding on the merits in many dismissed cases.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, biographical, trapped, local).

% Attempt to bring Section 1983 suits against officers and face a doctrine that frequently resolves cases on immunity grounds without reaching whether a violation occurred at all, which also prevents the precedent from clearly establishing the right for future plaintiffs — a self-reinforcing loop from this reading's own acknowledgment, even as it defends the doctrine's necessity.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Screen out plausible but doctrinally difficult constitutional claims because qualified immunity makes them financially unviable to litigate on contingency. Their case-selection pressure is invisible to the courts applying the doctrine and does not appear in the population of cases actually litigated.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_attorneys, excluded,
    moderate, biographical, constrained, national).

% Study the doctrine's origin, evolution, and empirical effects on officer behavior and plaintiff outcomes. Divided on whether the doctrine achieves its stated deterrence-of-frivolous-litigation goal or merely forecloses meritorious claims.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, legal_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__protective_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shields officers from personal financial ruin over split-second judgment calls made in good faith under ambiguous legal standards, on the theory that without this shield, officers would either decline to act in ambiguous situations (under-enforcement) or departments would struggle to recruit and retain officers willing to accept the personal liability risk inherent in the job.
% TRANSFER_FUNCTION: Moves the cost of uncertain-law constitutional violations from the individual officer (and, functionally, from the municipality that would otherwise settle or lose at trial) onto the person whose rights were violated, who is left to absorb the harm without compensation when no sufficiently similar precedent existed at the time of the incident.
% ABSENT_VOICES: Civil rights attorneys who declined cases as unviable never appear in the docket; their silent case-selection is invisible to courts assessing whether the doctrine's costs are acceptable. Legislators who could replace the judge-made 'clearly established' standard with a statutory standard rarely act because the doctrine imposes no direct cost on them.
% DISAPPEARANCE_RATIONALE: This reading holds that if qualified immunity vanished overnight, officers would face a surge of litigation (including bad-faith and frivolous suits), personal liability exposure would deter proactive policing in ambiguous encounters, and departments would face recruitment and retention crises absent expanded indemnification schemes — the world would rearrange around a more litigation-averse enforcement posture. Sibling readings dispute this causal claim; the empirical evidence on deterrence effects is itself contested.
% FOUNDING_PROBLEM: Officers making rapid decisions in ambiguous legal terrain (use of force, searches, seizures) needed protection from personal liability for conduct that was reasonable given the law as it existed at the time, so the law could develop through litigation without punishing officers for failing to predict future rulings.
% FOUNDING_PROBLEM_CORROBORATION: Police unions and municipal risk-management officials attest the recruitment/retention problem remains live. Independent empirical researchers (e.g., studies on officer indemnification patterns showing municipalities, not officers, typically pay judgments) and several sitting federal judges in published opinions have questioned whether the originally-stated problem — protecting officers' personal assets — still requires the current doctrine's scope, given that officers are near-universally indemnified in practice. This reading treats that critique as unresolved rather than dispositive.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, contested).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.52 (not high) because this reading holds the doctrine's core function — protecting good-faith judgment calls in genuinely ambiguous law — is real and operative in a substantial share of cases, even while acknowledging (per the founding_problem_corroboration) that the officer-personal-liability rationale is weakened by near-universal indemnification. Suppression is authored at 0.58, reflecting that the doctrine's persistence depends on active judicial maintenance of the 'clearly established' framework and organized lobbying resistance to reform, not on being self-evidently just. Theater ratio is modest (0.28) because from this reading's own lights, the doctrine performs a real function most of the time; it is not claimed to be a hollow shell. The rising extraction and suppression trajectories reflect the doctrine's post-2000s expansion via increasingly narrow definitions of 'clearly established,' a drift this reading acknowledges even while defending the underlying rationale.
 *
 * PERSPECTIVAL GAP:
 *   From the officer/beneficiary seat, this reading holds the doctrine is functionally necessary coordination: without it, officers would rationally under-police ambiguous situations or depart the profession. From the survivor/payer seat, the identical structure delivers dismissal without a merits ruling and no compensation, regardless of whether a violation occurred. The engine computes these as different seat-level classifications from the same structural data — this reading does not claim the payer seat is wrong to experience it as extraction, only that a genuine coordination function coexists with that extraction, which is precisely the tangled_rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers, municipal governments, and police unions are structural beneficiaries: the doctrine caps their financial exposure and stabilizes recruitment, so directionality sits near the beneficiary end. Constitutional violation survivors and civil rights plaintiffs are structural targets — trapped exit options (a violation, once suffered, cannot be undone or routed around) and powerless standing push directionality toward the full-target end. Courts occupy an agenda-setting seat with analytical exit — they administer the standard but do not personally bear its costs or collect its benefits, which is why courts are not listed as beneficiaries despite setting the terms.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists collapsing into either 'pure extraction' (accountability_void_reading) or 'illegitimate from inception' (constitutional_fidelity_reading) by insisting the coordination function was real at founding and remains partially live — officers do face genuine split-second ambiguity, and some claims against them are meritless. Tangled Rope classification captures this: coordination and extraction coexist through the same structure, and the classification does not require choosing one narrative over the other. Whether the founding problem's corroborating premise (personal financial exposure) still holds given near-universal indemnification is left as an omega rather than resolved in this reading's favor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indemnification_undermines_founding_rationale,
    'If officers are near-universally indemnified by their departments in practice, does the doctrine''s stated rationale (protecting officers'' personal assets) still hold, or has the doctrine''s actual function shifted to protecting municipal budgets and department reputations rather than individual officers?',
    'Empirical audit of indemnification rates across a representative sample of jurisdictions, comparing officer out-of-pocket exposure with and without qualified immunity in matched cases.',
    'If indemnification is near-universal, this reading''s coordination story weakens substantially — the doctrine would be protecting institutional finances under an individual-protection narrative, which shifts the classification''s beneficiary structure toward municipal governments and away from the officer-centered rationale, and strengthens the case for the accountability_void_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indemnification_undermines_founding_rationale, empirical, 'Whether the officer-personal-liability founding rationale survives near-universal indemnification practice.').

omega_variable(
    clearly_established_standard_scope,
    'Is the ''clearly established law'' standard, as currently applied by courts, calibrated to filter genuinely ambiguous cases, or has judicial practice drifted toward requiring near-identical precedent that forecloses meritorious claims regardless of ambiguity?',
    'Longitudinal analysis of circuit court qualified immunity rulings tracking the specificity threshold required to defeat immunity, compared against the standard''s original articulation in Harlow v. Fitzgerald and Anderson v. Creighton.',
    'If the standard has drifted toward requiring near-identical precedent, this reading''s claim that the doctrine screens for genuine ambiguity (rather than screening out valid claims wholesale) is weakened, supporting reclassification toward higher extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_standard_scope, empirical, 'Whether judicial application of the clearly-established standard has drifted from its original calibration.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice to author this constraint under the protective_scaffold reading rather than the accountability_void_reading or constitutional_fidelity_reading itself a contestable framing decision, and what would change if a different reading were treated as authoritative?',
    'This is inherent to the kernel structure and is not resolvable by additional data — it is a genuine multiplicity of legitimate framings held by different institutional actors (law enforcement/municipal seats favor this reading; civil rights litigators and legal historians favor the sibling readings).',
    'Adopting the constitutional_fidelity_reading would treat the doctrine as illegitimate from inception regardless of its policy outcomes, making the coordination-function question moot; adopting the accountability_void_reading would treat the coordination story as pure cover, collapsing the tangled_rope classification toward snare. This story deliberately holds only the protective_scaffold reading and links to the siblings via network edges rather than resolving between them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading is authoritative is itself contested and unresolved by this story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qual_tr_t8, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(qual_tr_t16, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(qual_tr_t24, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(qual_tr_t32, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(qual_tr_t40, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(qual_be_t8, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(qual_be_t16, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(qual_be_t24, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(qual_be_t32, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(qual_be_t40, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(qual_su_t8, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(qual_su_t16, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(qual_su_t24, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(qual_su_t32, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(qual_su_t40, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__protective_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the qualified_immunity_doctrine kernel. protective_scaffold_reading (this file) authors ε=0.52 under the premise that the coordination function (protecting good-faith judgment under legal ambiguity) remains substantially operative. accountability_void_reading authors a materially higher ε under the premise that the coordination story is cover for systematic impunity. constitutional_fidelity_reading treats the doctrine's legitimacy as foreclosed by its lack of textual authorization, independent of any extraction measurement. All three share the same underlying doctrinal kernel but instantiate structurally distinct constraints with different beneficiary/victim framings and different claimed types; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
