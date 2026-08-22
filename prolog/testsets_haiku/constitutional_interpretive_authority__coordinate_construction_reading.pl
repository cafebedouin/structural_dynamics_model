% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__coordinate_construction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Constitutional Interpretive Authority — Coordinate Construction Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The coordinate construction reading holds that no single branch of
 *   government possesses final interpretive authority over the constitution.
 *   Instead, constitutional meaning emerges through dialogue, contestation,
 *   and negotiation among the legislative, executive, and judicial branches.
 *   Each branch interprets the constitution within its sphere, but no branch
 *   can unilaterally declare what the constitution means. This reading
 *   describes how constitutional systems actually function in practice:
 *   through appointment cycles, electoral shifts, amendment campaigns, and
 *   inter-branch political struggle rather than through the pronouncements of
 *   courts or legislatures acting alone. The constraint is CLAIMED as rope
 *   (genuine coordination of a genuine problem) and the authored metrics
 *   describe a coordination arrangement that is beginning to show signs of
 *   political extraction — the theater ratio rises as branches increasingly
 *   use constitutional interpretation for partisan advantage rather than
 *   good-faith dialogue.
 *
 * KEY AGENTS:
 *   - Legislative branch: One coordinate voice; can propose amendments and control budgets but cannot monopolize interpretation.
 *   - Judicial branch: Articulates constitutional meaning through adjudication but cannot enforce without legislative and executive cooperation.
 *   - Executive branch: Interprets and enforces constitutional law in administration and foreign policy; subject to legislative override and judicial review.
 *   - Political parties: Benefit from the flexibility of inter-branch contestation; can shift constitutional meaning through electoral victories.
 *   - Citizens and civil society: Depend on political mobilization to move constitutional meaning in their favor; bear the cost of instability and deadlock.
 *   - Judicial supremacy advocates: Excluded; their preferred reading (courts as final authority) is institutionally marginalized.
 *   - Parliamentary supremacy advocates: Excluded; their preferred reading (legislatures as final authority) is institutionally marginalized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.58).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.41).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Constitutional Interpretive Authority — Coordinate Construction Reading").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "constitutional_law/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, '63efafdb-c16c-47a8-b6c2-7bde12e95df3').
narrative_ontology:cs_kernel_codification('63efafdb-c16c-47a8-b6c2-7bde12e95df3', fixed_text).
narrative_ontology:cs_authority_grounding('63efafdb-c16c-47a8-b6c2-7bde12e95df3', distributed).
narrative_ontology:cs_reading_relation('63efafdb-c16c-47a8-b6c2-7bde12e95df3', constitutional_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('63efafdb-c16c-47a8-b6c2-7bde12e95df3', constitutional_interpretive_authority__parliamentary_supremacy_reading, coexists_with).
narrative_ontology:cs_axiom('63efafdb-c16c-47a8-b6c2-7bde12e95df3', foundational, no_single_authoritative_interpreter).
narrative_ontology:cs_axiom_status(no_single_authoritative_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('63efafdb-c16c-47a8-b6c2-7bde12e95df3', no_single_authoritative_interpreter, conventional).
narrative_ontology:cs_axiom('63efafdb-c16c-47a8-b6c2-7bde12e95df3', foundational, constitutional_meaning_through_political_contestation).
narrative_ontology:cs_axiom_status(constitutional_meaning_through_political_contestation, holdable).
narrative_ontology:cs_axiom_grounding('63efafdb-c16c-47a8-b6c2-7bde12e95df3', constitutional_meaning_through_political_contestation, instrumental).
narrative_ontology:cs_reference_frame('63efafdb-c16c-47a8-b6c2-7bde12e95df3', coordinate_branch_authority_framework).
narrative_ontology:cs_drift_state('63efafdb-c16c-47a8-b6c2-7bde12e95df3', contemporary_high_polarization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('63efafdb-c16c-47a8-b6c2-7bde12e95df3', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_democracy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, political_parties).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, citizens_and_civil_society).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, citizens_and_civil_society).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, checks_and_balances_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, separation_of_powers_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates as one voice in constitutional interpretation; can propose amendments, control budgets of coordinate branches, appoint lower-court judges and executive officials. Does not possess final authority — its constitutional readings can be challenged by judicial interpretation or reshaped by executive action. Bears the cost of accepting interpretive challenges and negotiating constitutional meaning.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, legislative_branch, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, legislative_branch, payer).

% Articulates constitutional meaning through case adjudication; lacks power to initiate cases or enforce its interpretations without legislative and executive cooperation. Cannot amend the constitution or appropriate funds. Bears the cost of accepting legislative override (amendment) and executive non-compliance (defiance).
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, judicial_branch, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, judicial_branch, payer).

% Enforces and interprets constitutional law in administrative and military contexts; subject to legislative appropriation and override, judicial review, and electoral accountability. Its constitutional readings are tested against judicial review and legislative constraint. Bears the cost of accepting judicial invalidation and legislative defiance of its policies.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch, payer).

% Benefit from a constitutional framework that can be reinterpreted through electoral victories and branch control without formal amendment. Coalition-building across branches is the mechanism for constitutional change. Their power depends on mobilizing majorities, not on institutional position. Can shift constitutional meaning by appointing judges, controlling legislatures, and winning presidencies.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, political_parties, beneficiary,
    organized, biographical, mobile, national).

% Benefit from interpretive flexibility: constitutional meaning can respond to social change without formal amendment process. Subject to the outcomes of inter-branch contestation — their rights depend on which branch prevails in each interpretive dispute. Must mobilize political power to shift constitutional meaning in their favor.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, citizens_and_civil_society, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, citizens_and_civil_society, payer).

% Argue for concentrated judicial authority and final interpretive power. Are marginalized by the coordinate construction reading's dispersal of authority. Would advocate for formal doctrine granting courts override authority but are excluded from the institutional framework this reading describes.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, judicial_supremacy_advocates, excluded,
    organized, generational, constrained, national).

% Argue for concentrated legislative authority and removal of judicial review. Are excluded by the coordinate construction reading's recognition of coordinate branches. Their position receives no institutional home in this framework and depends on extra-constitutional mobilization (revolution, amendment) to prevail.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, parliamentary_supremacy_advocates, excluded,
    organized, generational, constrained, national).

% Analyze and articulate the constitutional framework; their interpretations influence how branches understand their own authority. Do not possess formal authority but shape the discourse within which authority disputes occur.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__coordinate_construction_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates inter-branch conflict over constitutional meaning by establishing that no single authoritative interpreter exists; constitutional meaning emerges through political negotiation, appointment cycles, electoral shifts, and amendment processes rather than singular judicial decree or legislative command. Solves the problem of how to amend or reinterpret a fixed text without formal amendment when consensus shifts.
% TRANSFER_FUNCTION: Moves interpretive authority from centralized adjudicators (as in judicial supremacy) or legislatures (as in parliamentary supremacy) to a distributed mechanism wherein political power translates into constitutional meaning: winning majorities appoint judges, control budgets, pass legislation. The constraint transfers the capacity to reshape constitutional meaning to electoral politics rather than judicial opinions or legislative will alone.
% ABSENT_VOICES: Advocates for strong judicial supremacy (who would argue courts should be the ultimate interpreters) and advocates for strong parliamentary supremacy (who would argue legislatures should be unchecked) are both structurally excluded from institutional voice — neither position fits the coordinate construction reading's framework. Their objections come from outside the system (academic critique, constitutional amendment proposals, revolutionary rhetoric).
% DISAPPEARANCE_RATIONALE: If no branch accepted the coordinate construction reading — if, instead, one branch claimed final authority — the constitutional system would reorganize: either as a judicially-dominated system (courts veto legislative acts) or a legislatively-dominated system (parliament cannot be judicially reviewed). The current stability of inter-branch dialogue depends on mutual acceptance of this reading's dispersed authority structure. Abandoning it would trigger constitutional crisis or regime change.
% FOUNDING_PROBLEM: How to govern through a written constitution without either (a) enslaving present generations to past text interpreted only one way, or (b) allowing present majorities to govern without constitutional constraint. The coordinate construction reading solves this by allowing constitutional meaning to evolve through inter-branch contestation while maintaining fidelity to the written text.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists from Madison onward attest the founding problem is live: each generation must interpret the constitution in light of new circumstances. The Framers' deliberate vagueness in constitutional text is documented in historical records and scholarly analysis (Rakove, Amar, Sunstein on constitutional interpretation). Judicial and legislative actors across the spectrum acknowledge some form of the coordinate construction reading in practice — they dispute single-branch supremacy even as they advocate for their own branch's weight in the dialogue. The only parties that deny the founding problem's current salience are those advocating wholesale constitutional replacement or explicit supremacy doctrines (rare institutional positions).
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__coordinate_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the coordinate construction reading produces genuine coordination benefit — it allows constitutions to evolve without formal amendment — but also enables political extraction: branches use constitutional interpretation to consolidate power, parties use constitutional disputes to mobilize supporters, and powerless actors bear the cost of interpretive instability. Suppression is moderate-low (0.41) because no single enforcer can impose a reading against unified opposition; resistance is high (0.72) because each branch and every party faction contests others' interpretive authority. Theater ratio rises from 0.25 to 0.47 (peaking at t=40 then moderating at t=50) because as the system matures, inter-branch dialogue increasingly becomes a vehicle for partisan contestation rather than good-faith constitutional reasoning. The trajectory shows increasing cost to participating actors of the interpretive instability the reading tolerates. At t=50, extractiveness moderates slightly as growing awareness of theater-ratio rise triggers some institutional self-correction and return to substantive engagement.
 *
 * PERSPECTIVAL GAP:
 *   Branches experience this constraint differently: the judiciary frames its role as guardian of constitutional fidelity and rule of law (beneficiary narrative); the legislature frames its role as representative and accountable (beneficiary narrative); the executive frames its role as keeper of national security and administrative efficiency (beneficiary narrative). From the perspective of powerless citizens, the same structure appears as an arena of political struggle in which their interests are collateral — they must mobilize politically to move constitutional meaning, and when political forces deadlock, their rights hang in balance. The engine computes this divergence from the structural data: powerful institutional seats compute differently from powerless seats, not because they experience different rules, but because their position relative to the constraint differs (agenda-setter vs. payer vs. beneficiary).
 *
 * DIRECTIONALITY LOGIC:
 *   The three coordinate branches are all agenda-setters; they participate symmetrically in the interpretive dialogue. Their directionality should compute near 0.5 — they benefit from the coordination function (stable constitutional framework) and bear the cost of accepting challenges (having their interpretations overridden). Political parties compute as beneficiaries (d near 0.0) because they gain the ability to shift constitutional meaning without formal amendment and bear minimal direct cost. Citizens compute as bearing costs (d higher, toward 0.6–0.7) because they depend on political mobilization and face instability. The excluded parties (judicial and parliamentary supremacy advocates) are structurally outside the framework and do not compute as seats; they represent alternative readings not instantiated here.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the mandatrophy trap because the founding problem it addresses is genuinely live: each generation must interpret a fixed text in light of new circumstances, and the coordinate construction reading provides a mechanism (inter-branch dialogue) for doing so without formal amendment. The founding problem has not become obsolete. However, the measurement series shows rising theater ratio (extractive use of the interpretation mechanism) and then moderate retreat, suggesting that participants are becoming aware of the mechanism's vulnerability to partisan capture and are self-correcting. This is NOT mandatrophy (the founding problem is not dead), but it IS a warning about the constraint's sustainability under high politicization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_dialogue_vs_power_struggle,
    'Is the inter-branch dialogue described by the coordinate construction reading genuine constitutional reasoning, or does it systematically serve partisan power consolidation?',
    'Empirical analysis of constitutional interpretation over electoral cycles: do branches shift constitutional readings following electoral realignment (suggesting partisan capture), or do they maintain interpretive positions across partisan control (suggesting reasoning independent of power)?',
    'If predominantly partisan, the constraint''s claimed coordination function is largely theater and the true classification edges toward tangled_rope (coordination as cover for extraction). If genuine reasoning dominates, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_dialogue_vs_power_struggle, empirical, 'Whether inter-branch constitutional dialogue serves constitutional coherence or partisan power.').

omega_variable(
    stability_cost_of_dispersed_authority,
    'Does the dispersion of interpretive authority produce constitutional instability that powerless actors cannot afford?',
    'Historical analysis of constitutional crises and their relationship to authority dispersion; comparison of rights security and legal predictability under this reading versus under single-authority alternatives.',
    'High instability costs would shift directionality for powerless actors toward higher target positions (d=0.65–0.75) and potentially trigger reclassification to snare or tangled_rope. Low costs would support the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_cost_of_dispersed_authority, empirical, 'Whether dispersed authority produces costs to powerless actors that outweigh coordination benefits.').

omega_variable(
    whether_coordinate_reading_forecloses_supremacy_readings,
    'Does the coordinate construction reading logically foreclose the judicial and parliamentary supremacy readings within the same constitutional framework, or do all three readings coexist as live positions?',
    'Formal logical analysis: can a single constitutional text ground all three readings, or does accepting one reading''s core premise necessitate rejecting another''s?',
    'If foreclosure exists, the reading_relations should be ''forecloses'' rather than ''coexists_with''. If all three readings are logically compatible (only politically exclusive), the ''coexists_with'' relation stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(whether_coordinate_reading_forecloses_supremacy_readings, conceptual, 'The logical relationship between the coordinate construction reading and supremacy readings.').

omega_variable(
    theater_ratio_trajectory_interpretation,
    'Does the rising theater ratio from t=0 to t=40 indicate the coordinate construction reading itself is eroding, or does it indicate the reading is functioning but under increasing strain?',
    'Qualitative analysis of branch behavior at each time point: does theater ratio growth reflect increasing partisan use of constitutional interpretation, or increasing performative compliance with the reading''s norms?',
    'If the reading is eroding, the constraint may be transitioning toward piton (maintained by inertia and theater, not genuine coordination). If the reading is strained but holding, it remains a rope under pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_trajectory_interpretation, empirical, 'The significance of the measured theater-ratio rise and subsequent moderation.').

omega_variable(
    excluded_voices_institutional_viability,
    'Could judicial supremacy or parliamentary supremacy advocates achieve institutional dominance through amendment or regime change, or are they structurally constrained to remain excluded voices?',
    'Historical analysis of amendment efforts and constitutional moments; comparison of amendment adoption rates for supremacy-doctrine amendments versus other constitutional changes.',
    'High viability for excluded readings would suggest the coordinate construction reading is unstable and carries higher foreclosure risk from its siblings. Low viability would support the ''coexists_with'' relation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_voices_institutional_viability, empirical, 'Whether excluded readings have plausible paths to institutional dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 40, 0.51).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 50, 0.47).
narrative_ontology:measurement_basis(cons_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(cons_be_t50, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(constitutional_interpretive_authority__coordinate_construction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__coordinate_construction_reading, 0.18).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).

% DUAL FORMULATION NOTE:
% The constitutional_interpretive_authority kernel decomposes into three structurally distinct constraints, each instantiating a different reading: coordinate_construction_reading (this file), judicial_supremacy_reading, and parliamentary_supremacy_reading. Each reading produces a different constraint_id, different beneficiary/victim structure, and different ε value, because the readings answer the kernel question (who has final authority?) differently. They are not the same constraint viewed from different observer perspectives; they are three different structural arrangements that the same kernel text could ground. The three stories are linked by this network block to enable contamination analysis: if one reading's institutional support weakens, the system computes how that affects the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__coordinate_construction_reading, powerless, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
