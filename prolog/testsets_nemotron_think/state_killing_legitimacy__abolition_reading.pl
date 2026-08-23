% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: State Killing Arrangement (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint story analyzes the standing arrangement of state killing
 *   (capital punishment) from the abolition reading's structural perspective.
 *   The abolition reading holds that state killing categorically violates
 *   human dignity regardless of desert or utility. The story evaluates the
 *   actual operation of the death penalty system — not the abolitionist
 *   alternative — and finds it operates as a snare: pure extraction of life
 *   from condemned persons, sustained by active enforcement (suppression of
 *   alternatives, procedural barriers to relief), with a coordination cover
 *   story (deterrence/retribution) that the abolition reading regards as
 *   empirically and normatively unsustained. The claimed_type 'snare'
 *   reflects the abolition reading's structural assessment; the metrics
 *   describe the arrangement's observed operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.88).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.85).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "State Killing Arrangement (Abolition Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '1a35d664-9500-4b92-849a-add4b2473e90').
narrative_ontology:cs_kernel_codification('1a35d664-9500-4b92-849a-add4b2473e90', formalized).
narrative_ontology:cs_authority_grounding('1a35d664-9500-4b92-849a-add4b2473e90', extraction).
narrative_ontology:cs_interpretation_layer_present('1a35d664-9500-4b92-849a-add4b2473e90').
narrative_ontology:cs_reading_relation('1a35d664-9500-4b92-849a-add4b2473e90', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('1a35d664-9500-4b92-849a-add4b2473e90', state_killing_legitimacy__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('1a35d664-9500-4b92-849a-add4b2473e90', foundational, human_dignity_inviolable_by_state).
narrative_ontology:cs_axiom_status(human_dignity_inviolable_by_state, holdable).
narrative_ontology:cs_axiom_grounding('1a35d664-9500-4b92-849a-add4b2473e90', human_dignity_inviolable_by_state, deontological).
narrative_ontology:cs_axiom('1a35d664-9500-4b92-849a-add4b2473e90', foundational, state_killing_never_justified_by_utility).
narrative_ontology:cs_axiom_status(state_killing_never_justified_by_utility, holdable).
narrative_ontology:cs_axiom_grounding('1a35d664-9500-4b92-849a-add4b2473e90', state_killing_never_justified_by_utility, deontological).
narrative_ontology:cs_reference_frame('1a35d664-9500-4b92-849a-add4b2473e90', absolute_human_dignity_framework).
narrative_ontology:cs_drift_state('1a35d664-9500-4b92-849a-add4b2473e90', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1a35d664-9500-4b92-849a-add4b2473e90', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, prosecutors).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, retributive_advocates).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, state_monopoly_on_violence).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, deterrence_theory).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, retributive_justice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face execution by the state with no meaningful exit; legal appeals are structurally constrained by procedural bars, clemency is politically discretionary, and the sentence itself removes them from the polity. Their life is the extraction target of the arrangement.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, payer,
    powerless, immediate, trapped, national).

% Administers the death penalty system through legislatures, courts, and corrections departments. Claims the arrangement coordinates deterrence and retributive justice. Collects legitimacy and social control from the demonstration of ultimate sovereign power over life. Can modify or abolish the arrangement but faces political incentives to maintain it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain conviction leverage and career advancement from death-eligible charges; the threat of execution extracts plea bargains and trial concessions. Their professional incentives align with maintaining the arrangement's credibility. Can exit to private practice or non-capital dockets but benefit from the arrangement's existence.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, prosecutors, beneficiary,
    powerful, biographical, mobile, national).

% Victims' families and advocacy groups who experience the arrangement as delivering moral closure and proportional justice. Their participation is voluntary but structurally supported by victim-impact statements and political rhetoric. Exit means rejecting the cultural script of retribution, which carries social cost.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retributive_advocates, beneficiary,
    moderate, biographical, constrained, local).

% Litigate, legislate, and organize against the arrangement. They do not collect from it nor pay into it directly; they analyze its structural operation and mobilize counter-pressure. Their exit is analytical — they can shift focus to other human rights campaigns but remain positioned against this arrangement.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_advocates, observer,
    organized, generational, analytical, global).

% UN treaty bodies, regional courts, and NGOs that condemn the arrangement as a human rights violation. They are structurally excluded from domestic decision-making on executions; their objections are received as external pressure rather than participatory input. Their exit is analytical — they monitor and report but cannot veto.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, international_human_rights_bodies, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement claims to coordinate two functions: (1) general deterrence — the threat of execution prevents future murders by rational calculation; (2) retributive proportionality — the murderer forfeits their right to life through desert, satisfying a moral balance. The abolition reading holds that neither function is empirically or normatively sustained by the arrangement's actual operation.
% TRANSFER_FUNCTION: Moves the condemned person's life from them to the state as a performative demonstration of sovereign power. The state collects legitimacy, deterrence signaling, and retributive satisfaction; the condemned person pays the ultimate cost. No reciprocal transfer returns value to the condemned.
% ABSENT_VOICES: The condemned persons themselves are silenced by the arrangement's terminal act — they cannot testify to its operation post-execution. International human rights bodies are excluded from domestic constitutional adjudication on the death penalty. Future generations who inherit the precedent of state killing are not represented in current deliberations.
% DISAPPEARANCE_RATIONALE: If the death penalty vanished overnight, the criminal justice system would reorganize around life without parole as the maximum sanction. Prosecutors would lose plea leverage from death eligibility. Retributive advocates would lose the symbolic apex of punishment. The state would lose its ultimate coercive spectacle. Abolitionist jurisdictions demonstrate this rearrangement is stable and functional.
% FOUNDING_PROBLEM: The arrangement was built to solve the problem of ultimate sanction: how to definitively incapacitate the most dangerous offenders, satisfy societal demand for proportional response to murder, and demonstrate sovereign authority over life and death in a manner that deters future violence.
% FOUNDING_PROBLEM_CORROBORATION: Criminological consensus (National Research Council 2012, updated 2023) finds no reliable evidence that execution deters more effectively than long imprisonment. International human rights law (ICCPR, Protocol 6/13 ECHR, American Convention) treats abolition as the normative endpoint, with 112 states parties to abolition protocols. The state's own corrections administrators increasingly report that life without parole achieves incapacitation without the arrangement's error, cost, and legitimacy deficits.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-maximal (0.88) because the arrangement takes the condemned person's entire future life-course with no reciprocal transfer. Suppression is high (0.85) because the arrangement's persistence depends on actively suppressing alternatives: procedural bars (AEDPA, Teague, procedural default), clemency politicization, secrecy protocols for execution drugs, and judicial deference that insulates the system from meaningful review. Theater ratio (0.38) reflects the gap between the arrangement's claimed coordination functions and its actual operation: lengthy delays (mean 20+ years sentence-to-execution), arbitrary application (geographic/racial disparities), and botched executions reveal the coordination story as increasingly performative. Accessibility collapse (0.92) is near-total for condemned persons — once sentenced, alternatives (commutation, judicial relief, actual innocence claims) collapse structurally. Resistance (0.72) is substantial: abolitionist litigation, legislative repeal campaigns, pharmaceutical boycotts, international pressure, and declining public support all contest the arrangement, but have not displaced it in retentionist jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the state/prosecutor seats, the arrangement presents as coordination (deterrence, retribution, finality). From the condemned person's seat, it is pure extraction with no coordination benefit — they are dead either way, but the arrangement chooses the mode and timing. From abolitionist/international seats, the arrangement is a legitimacy crisis for the state. The engine computes these divergent seat classifications from the single structural description; the abolition reading's claim (snare) is its assessment from the condemned person's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are full targets (d ≈ 1.0): trapped, powerless, identity-locked by the sentence itself, bearing the full extraction. State apparatus is the agenda-setter with arbitrage-grade exit (can abolish legislatively but chooses not to). Prosecutors are beneficiaries with mobile exit (collect career benefits, can leave capital practice). Retributive advocates are beneficiaries with constrained exit (social cost to rejecting retribution script). Abolitionist advocates and international bodies are observers/excluded — they experience the arrangement analytically but lack structural power to alter it domestically. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ultimate sanction for worst crimes) is contested as live: retentionists argue life without parole is insufficiently proportional or deterrent; abolitionists and criminological evidence argue it achieves the same ends without the arrangement's extraction. The arrangement persists not because the founding problem is unsolved, but because the state apparatus and prosecutors benefit from the arrangement's extractive operation (leverage, spectacle, sovereign demonstration). This is mandatrophy: the arrangement's mandate has outlived its coordination function, but the extraction function maintains it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_foreclosure,
    'Does the abolition reading''s categorical premise logically foreclose the retributive and deterrence readings within a single legal framework, or do they coexist as competing but structurally compatible positions?',
    'Constitutional theory analysis: if a constitution adopts the abolition reading as supreme law (e.g., via interpretation of ''cruel and unusual'' or ''right to life''), the sibling readings are legally foreclosed. If the constitution is silent or permissive, they coexist as legislative options. The kernel''s authority_grounding determines which.',
    'If foreclosure holds, the kernel is a binary switch — abolition or retention, no stable middle. If coexistence holds, the kernel admits a spectrum of hybrid regimes (e.g., death penalty only for terrorism, only with heightened procedural safeguards). This changes the drift_state magnitude assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_foreclosure, conceptual, 'Whether the kernel''s readings are mutually exclusive in a single framework').

omega_variable(
    deterrence_empirical_status,
    'Is the deterrence claim empirically falsified, or does genuine uncertainty persist such that the deterrence reading''s coordination function remains plausibly live?',
    'Updated meta-analysis of panel studies with state-level controls, addressing the Donohue-Wolfers critique and subsequent replies. If deterrence effect is statistically indistinguishable from zero across robust specifications, the coordination cover story collapses for the deterrence reading.',
    'If deterrence is falsified, the deterrence reading''s coordination function is a cover story — the arrangement is a snare from all seats. If deterrence remains contested, the deterrence reading retains a genuine (though disputed) coordination claim, making the arrangement a tangled_rope from the deterrence seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_status, empirical, 'Empirical status of the deterrence coordination claim').

omega_variable(
    retributive_desert_coherence,
    'Is the retributive claim (forfeiture of life-right through desert) a coherent normative premise, or does it presuppose a metaphysical status (moral agency, free will) that the abolition reading''s dignity framework denies?',
    'Philosophical analysis of whether ''forfeiture'' is a coherent concept within a dignity-based framework that treats human rights as inalienable. If rights are inalienable, forfeiture is a category error; if rights are alienable by conduct, forfeiture is coherent.',
    'If forfeiture is incoherent within dignity framework, the retributive reading is not a genuine alternative but a category mistake — strengthening the abolition reading''s foreclosure claim. If coherent, the readings represent a genuine normative disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retributive_desert_coherence, conceptual, 'Coherence of the retributive forfeiture premise within a dignity framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 1972, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1972, state_killing_legitimacy__abolition_reading, theater_ratio, 1972, 0.25).
narrative_ontology:measurement(stat_tr_t1980, state_killing_legitimacy__abolition_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(stat_tr_t1990, state_killing_legitimacy__abolition_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(stat_tr_t2000, state_killing_legitimacy__abolition_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(stat_tr_t2010, state_killing_legitimacy__abolition_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(stat_tr_t2024, state_killing_legitimacy__abolition_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(stat_be_t1972, state_killing_legitimacy__abolition_reading, base_extractiveness, 1972, 0.78).
narrative_ontology:measurement(stat_be_t1980, state_killing_legitimacy__abolition_reading, base_extractiveness, 1980, 0.82).
narrative_ontology:measurement(stat_be_t1990, state_killing_legitimacy__abolition_reading, base_extractiveness, 1990, 0.85).
narrative_ontology:measurement(stat_be_t2000, state_killing_legitimacy__abolition_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(stat_be_t2010, state_killing_legitimacy__abolition_reading, base_extractiveness, 2010, 0.88).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__abolition_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1972, state_killing_legitimacy__abolition_reading, suppression_requirement, 1972, 0.7).
narrative_ontology:measurement(stat_su_t1980, state_killing_legitimacy__abolition_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(stat_su_t1990, state_killing_legitimacy__abolition_reading, suppression_requirement, 1990, 0.82).
narrative_ontology:measurement(stat_su_t2000, state_killing_legitimacy__abolition_reading, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(stat_su_t2010, state_killing_legitimacy__abolition_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__abolition_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% This constraint family (state_killing_legitimacy) decomposes the natural-language concept 'death penalty legitimacy' into three structurally distinct claims with different ε values and beneficiary/victim structures. The abolition reading assesses the standing arrangement as a snare (ε=0.88). The retributive reading assesses it as a rope or tangled_rope (coordination via desert). The deterrence reading assesses it as a rope (coordination via signaling). Their ε values differ because they identify different coordination functions and different extraction targets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_legitimacy__abolition_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
