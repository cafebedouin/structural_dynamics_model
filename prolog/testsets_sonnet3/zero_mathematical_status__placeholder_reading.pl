% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__placeholder_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__placeholder_reading, []).

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
 *   constraint_id: zero_mathematical_status__placeholder_reading
 *   human_readable: Zero as Positional Placeholder (Non-Arithmetic Reading)
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This constraint captures the reading of zero's mathematical status held
 *   by positional-notation cultures (Babylonian sexagesimal scribes, early
 *   Hellenistic astronomical tables, and comparable traditions) that adopted
 *   a placeholder mark to resolve positional ambiguity but did not extend
 *   zero into the domain of arithmetic operands — no defined a+0, a-0=0,
 *   a×0=0 rules, no treatment of zero as a root or coefficient. This is
 *   distinct from the number_reading (zero as a fully operational number, per
 *   Brahmagupta) and from the parmenidean_rejection (zero as ontologically
 *   impossible even as a placeholder). The three readings are not the same
 *   constraint measured differently; they are structurally distinct claims
 *   with different ε, different beneficiaries, and different persistence
 *   conditions, linked here only through the shared kernel of 'what is zero's
 *   mathematical status.'
 *
 * KEY AGENTS:
 *   - babylonian_and_hellenistic_scribal_computists: primary beneficiary — notational efficiency without ontological commitment
 *   - practitioners_seeking_general_arithmetic_closure: primary payer — blocked from treating zero as operand
 *   - later_algebraists_needing_zero_as_operand: downstream payer — forced to import or develop number-zero externally
 *   - parmenidean_philosophical_tradition: excluded voice — objects to any zero-entity, not consulted
 *   - historians_of_mathematics: analytical observer — documents the transition and its costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__placeholder_reading, 0.38).
domain_priors:suppression_score(zero_mathematical_status__placeholder_reading, 0.42).
domain_priors:theater_ratio(zero_mathematical_status__placeholder_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__placeholder_reading, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__placeholder_reading, "Zero as Positional Placeholder (Non-Arithmetic Reading)").
narrative_ontology:topic_domain(zero_mathematical_status__placeholder_reading, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:requires_active_enforcement(zero_mathematical_status__placeholder_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__placeholder_reading, '40a33ef3-4574-4543-a1db-b327fe83ea93').
narrative_ontology:cs_kernel_codification('40a33ef3-4574-4543-a1db-b327fe83ea93', distributed).
narrative_ontology:cs_authority_grounding('40a33ef3-4574-4543-a1db-b327fe83ea93', practice).
narrative_ontology:cs_interpretation_layer_present('40a33ef3-4574-4543-a1db-b327fe83ea93').
narrative_ontology:cs_reading_relation('40a33ef3-4574-4543-a1db-b327fe83ea93', zero_mathematical_status__number_reading, influences).
narrative_ontology:cs_reading_relation('40a33ef3-4574-4543-a1db-b327fe83ea93', zero_mathematical_status__parmenidean_rejection, coexists_with).
narrative_ontology:cs_axiom('40a33ef3-4574-4543-a1db-b327fe83ea93', foundational, zero_is_notation_not_quantity).
narrative_ontology:cs_axiom_status(zero_is_notation_not_quantity, holdable).
narrative_ontology:cs_axiom_grounding('40a33ef3-4574-4543-a1db-b327fe83ea93', zero_is_notation_not_quantity, conventional).
narrative_ontology:cs_axiom('40a33ef3-4574-4543-a1db-b327fe83ea93', secondary, positional_disambiguation_suffices_without_operand_status).
narrative_ontology:cs_axiom_status(positional_disambiguation_suffices_without_operand_status, overridden).
narrative_ontology:cs_axiom_grounding('40a33ef3-4574-4543-a1db-b327fe83ea93', positional_disambiguation_suffices_without_operand_status, instrumental).
narrative_ontology:cs_reference_frame('40a33ef3-4574-4543-a1db-b327fe83ea93', sexagesimal_positional_scribal_practice).
narrative_ontology:cs_drift_state('40a33ef3-4574-4543-a1db-b327fe83ea93', post_brahmagupta_transmission_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('40a33ef3-4574-4543-a1db-b327fe83ea93', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__placeholder_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, babylonian_and_hellenistic_scribal_computists).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, positional_notation_users).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, practitioners_seeking_general_arithmetic_closure).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, later_algebraists_needing_zero_as_operand).
narrative_ontology:constraint_vindicates(zero_mathematical_status__placeholder_reading, positional_place_value_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use a placeholder mark (later a symbol) purely to disambiguate positional value in a sexagesimal or similar system — distinguishing 3 06 from 36, for instance. They gain enormous notational efficiency without needing to justify what 'nothing' means as a quantity, and they set the convention that the mark is a spacer, not an operand.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, babylonian_and_hellenistic_scribal_computists, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__placeholder_reading, babylonian_and_hellenistic_scribal_computists, agenda_setter).

% Merchants, astronomers, and administrators who read and write positional numerals benefit from compact, unambiguous notation for large numbers, without any need to resolve zero's ontological or arithmetic status. Their exit option is limited to reverting to non-positional counting systems, which is costly and rarely taken.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, positional_notation_users, beneficiary,
    organized, generational, constrained, regional).

% Mathematicians and calculators who need zero to behave as an operand — to appear in subtraction results, division problems, or algebraic equations — find the placeholder convention refuses to answer what happens when a quantity is subtracted to nothing, or what 'zero of something' means as a term in an equation. They must either invent workarounds, avoid the operations, or import a different tradition's number-zero informally, without the placeholder tradition's endorsement.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, practitioners_seeking_general_arithmetic_closure, payer,
    moderate, generational, constrained, regional).

% Once algebra requires solving equations that produce or use zero as a term (roots, coefficients, remainders), the placeholder-only convention becomes a structural bottleneck. They eventually route around it by importing or independently developing zero-as-number frameworks (e.g., via transmission from Indian mathematics), but during the period this reading holds, they pay in blocked problems and unformalized workarounds.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, later_algebraists_needing_zero_as_operand, payer,
    moderate, civilizational, mobile, continental).

% Holds that nothing cannot be, and so zero cannot be a number at all — not even derivatively. This tradition is not consulted by the scribal/notational practice, which proceeds pragmatically without engaging the ontological objection; the philosophical camp would object that even a placeholder use smuggles in an illegitimate 'something' standing for 'nothing,' but this objection has no purchase on working notation.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, parmenidean_philosophical_tradition, excluded,
    institutional, civilizational, analytical, continental).

% Reconstruct which ancient systems (Babylonian, early Greek positional experiments, pre-Gupta counting boards) used zero purely as a placeholder versus as a number, and trace the transition. They are not party to the extraction but document the structural cost the placeholder-only convention imposed on later arithmetic development.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of positional ambiguity — distinguishing magnitude by place value without a placeholder symbol produces unreadable or ambiguous numerals. The placeholder convention coordinates all users of the positional system around a shared disambiguation mark.
% TRANSFER_FUNCTION: Moves computational and notational efficiency to everyday practitioners (scribes, merchants, astronomers) at the cost of formal generality to those who later need zero to participate in arithmetic operations — subtraction to zero, zero as a coefficient, zero as a root. The efficiency gain is collected by notation users; the closure cost is paid by anyone whose problem requires an operational zero.
% ABSENT_VOICES: The Parmenidean philosophical tradition, which would object on ontological grounds to any admission of zero-as-entity even as a mark; and future algebraists who are not yet present to object that the convention will eventually block their work. Neither voice is in the room when the placeholder convention is set.
% DISAPPEARANCE_RATIONALE: If the placeholder convention were removed and positional notation reverted to ambiguous or spacer-free numerals, day-to-day computation, record-keeping, and astronomical calculation would become materially harder — the coordination function is real and its removal would be felt immediately by working scribes and calculators, independent of the separate question of zero's arithmetic status.
% FOUNDING_PROBLEM: Positional numeral systems needed an unambiguous way to mark an empty place-value slot (distinguishing, e.g., 203 from 23) without inventing a new philosophical category of number.
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics (an observer seat outside the beneficiary group) attest that the placeholder function was fully absorbed into number-based zero conventions once Indian arithmetic (Brahmagupta et al.) formalized zero's operations and this framework diffused via Arabic transmission to later traditions — the placeholder-only restriction no longer describes any live mathematical practice, though it is a documented historical stage.
narrative_ontology:disappearance_verdict(zero_mathematical_status__placeholder_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__placeholder_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__placeholder_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_mathematical_status__placeholder_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__placeholder_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__placeholder_reading_tests).
:- end_tests(zero_mathematical_status__placeholder_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the placeholder convention is not itself coercive extraction in the ordinary economic sense — it is a genuine notational efficiency gain — but it does impose a real, structurally asymmetric cost: anyone whose mathematical problem requires zero as an operand is simply unserved by this convention, and that unmet need does not register within the placeholder tradition's own terms. Suppression (0.42) reflects the fact that the convention's persistence required scribal and institutional practice to actively NOT generalize the placeholder into a number — a deliberate boundary maintained by pedagogical and administrative practice, not a passive absence. Accessibility collapse is moderate (0.5): the placeholder framing does not foreclose alternative treatments of zero elsewhere (other traditions developed number-zero independently), but within this tradition's own practice, the operational use of zero collapses to nothing but a spacer.
 *
 * PERSPECTIVAL GAP:
 *   From the scribal/notational seat, this is a clean rope: it solves a real coordination problem (positional ambiguity) with minimal overhead, no one is a designated victim, and the convention is stable and useful on its own terms. From the seat of a practitioner needing zero as an operand, the same convention operates as an active constraint that must be actively maintained (via pedagogical and institutional practice) to keep zero categorized as 'not a number' — that maintenance is what makes the arithmetic-closure seat experience it as tangled_rope rather than mountain or rope. The engine should register this divergence rather than resolve it in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Scribal computists and positional-notation users are structural beneficiaries: they get the coordination gain (unambiguous place-value notation) without needing to resolve or fund the harder problem of zero's arithmetic behavior. Practitioners who need general arithmetic closure, and later algebraists specifically, are structural targets: the convention's boundary is exactly what blocks their work, and they bear the cost of either working around it or importing a foreign framework. The directionality here is not coercive in the interpersonal sense but structural in the epistemic sense — the convention's stability depends on not being pushed past its placeholder function.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (disambiguating positional place value) is genuinely dead in the sense that it was solved and then superseded — once zero-as-number frameworks were formalized and diffused, the placeholder-only restriction had no further work to do; the coordination function was absorbed into a strictly more general framework rather than being preserved as an empty shell. This prevents mislabeling the placeholder convention's HISTORICAL stage as either purely coordinative (ignoring the real cost it imposed on operand-seeking practitioners while it held) or purely extractive (ignoring that it genuinely solved a real problem within its scope). It is correctly read as a tangled rope during its active period: real coordination function, real asymmetric cost, actively maintained boundary — not because anyone profited from restricting zero, but because the restriction itself required ongoing pedagogical and institutional reinforcement not to be generalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    placeholder_versus_protonumber_boundary,
    'Is the placeholder mark in Babylonian/Hellenistic practice a genuinely pre-arithmetic notational device, or does its consistent, rule-governed use already constitute an implicit proto-arithmetic treatment that the tradition simply never formalized?',
    'Close philological and mathematical-historical analysis of surviving tablets and papyri for evidence of implicit operational rules (e.g., consistent behavior of the placeholder under repeated or combined positional operations) versus purely typographic/spacing use.',
    'If implicit proto-arithmetic use is found, this reading''s extractiveness should be lower (the closure gap is more apparent than real) and it shades toward a rope; if purely typographic, the tangled_rope reading with a genuine operand-closure cost is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(placeholder_versus_protonumber_boundary, empirical, 'Whether the placeholder use already implies unrecognized arithmetic content.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three sibling readings (placeholder, number, Parmenidean-rejection) of the zero_mathematical_status kernel disagree — is it about zero''s ontological status, its notational function, or its arithmetic operational rules?',
    'Structural decomposition already applied: each reading is authored as its own constraint story. The disagreement is located precisely at whether zero participates in arithmetic operations (this reading says no; number_reading says yes) and whether zero is coherent as any kind of entity at all (Parmenidean rejection says no to even the placeholder use).',
    'This placeholder_reading is compatible in principle with either the number_reading eventually superseding it (historically, this is what happened) or with the Parmenidean rejection being correct about number-status while this reading remains correct about notational legitimacy — the readings are not mutually exclusive at the notational level, only at the arithmetic-operand level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Committer-frame note: locates the exact axis of disagreement among the three kernel readings.').

omega_variable(
    beneficiary_versus_natural_convention_ambiguity,
    'Are scribal computists and positional-notation users genuine ''beneficiaries'' in the extractive sense, or is the placeholder convention simply the natural, non-extractive solution to a coordination problem with no meaningful victim — making the tangled_rope classification an overreading of what is actually closer to a rope with an unmet external need rather than an internal victim?',
    'Examine whether operand-seeking practitioners were members of the SAME community whose notational practice imposed the restriction (making it internal asymmetric cost) versus entirely separate traditions who simply had different needs (making it two ropes, not one tangled rope).',
    'If operand-seekers were largely external to the placeholder tradition (different scholarly communities, different eras), the tangled_rope classification overstates internal extraction and this may be better read as a rope with a documented external limitation, not an extractive structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_versus_natural_convention_ambiguity, conceptual, 'Whether the victim group is internal to the beneficiary community or external, affecting rope vs tangled_rope classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__placeholder_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__placeholder_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(zero_tr_t20, zero_mathematical_status__placeholder_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(zero_tr_t40, zero_mathematical_status__placeholder_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(zero_tr_t60, zero_mathematical_status__placeholder_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement(zero_tr_t80, zero_mathematical_status__placeholder_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(zero_tr_t100, zero_mathematical_status__placeholder_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__placeholder_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(zero_be_t20, zero_mathematical_status__placeholder_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(zero_be_t40, zero_mathematical_status__placeholder_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(zero_be_t60, zero_mathematical_status__placeholder_reading, base_extractiveness, 60, 0.36).
narrative_ontology:measurement(zero_be_t80, zero_mathematical_status__placeholder_reading, base_extractiveness, 80, 0.37).
narrative_ontology:measurement(zero_be_t100, zero_mathematical_status__placeholder_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_mathematical_status__placeholder_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(zero_su_t20, zero_mathematical_status__placeholder_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(zero_su_t40, zero_mathematical_status__placeholder_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(zero_su_t60, zero_mathematical_status__placeholder_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(zero_su_t80, zero_mathematical_status__placeholder_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement(zero_su_t100, zero_mathematical_status__placeholder_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__placeholder_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_mathematical_status__placeholder_reading, 0.05).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__parmenidean_rejection).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'zero's mathematical status' per the ε-invariance principle. placeholder_reading (this story, tangled_rope, ε≈0.38) sits structurally between number_reading (expected lower ε, closer to rope/mountain — zero fully integrated into arithmetic) and parmenidean_rejection (expected distinct profile — a philosophical denial constraint with its own beneficiary/victim structure among philosophical schools). The upstream story is number_reading's historical predecessor in most transmission chains (placeholder use often precedes full number-status recognition), so this story's persistence and eventual obsolescence structurally influences whether number_reading's beneficiary set can expand to include former placeholder-tradition practitioners.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
