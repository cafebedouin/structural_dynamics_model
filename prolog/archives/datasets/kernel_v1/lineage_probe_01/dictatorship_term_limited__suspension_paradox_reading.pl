% ============================================================================
% CONSTRAINT STORY: dictatorship_term_limited__suspension_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dictatorship_term_limited__suspension_paradox_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: dictatorship_term_limited__suspension_paradox_reading
 *   human_readable: Dictatorship as Legal Suspension (Suspension Paradox Reading)
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   The Roman dictatorship and its modern constitutional descendants present
 *   a fundamental paradox: an office of the law whose institutional content
 *   is the law's suspension. This constraint instantiates the
 *   suspension_paradox_reading of the dictatorship_term_limited kernel. From
 *   this reading's perspective, the dictatorship is not merely an emergency
 *   power whose bounds can be constitutionally specified — it is the
 *   revelation that law contains the capacity to negate itself from within
 *   its own framework. The constraint teaches that the constitution does not
 *   stand above the exception but contains the exception as its own negation.
 *   This reading coexists with the constitutional_emergency_reading (which
 *   frames the dictatorship as a bounded, time-limited response to genuine
 *   threats) and influences the precedent_for_caesarism_reading (by
 *   establishing the logical precedent that legal suspension can be invoked
 *   legitimately, normalizing the form for later perpetual extension). The
 *   extractiveness trajectory (0.45 → 0.68) reflects that the suppression
 *   machinery accumulates over the interval: early invocations establish
 *   legal precedent for suspension; subsequent invocations extend the bounds;
 *   by the endpoint, the suppression requirement has reached 0.72 as the
 *   legal framework has absorbed the exception into normal governance.
 *   Theater ratio remains moderate (0.55) because the constraint operates as
 *   both doctrinal claim and enforcement mechanism — it is not purely
 *   performative (there are real suspensions with real consequences) but also
 *   not purely functional (the doctrine of suspension serves sovereignty
 *   theory more than emergency response).
 *
 * KEY AGENTS:
 *   - Sovereign Authority: Beneficiary (institutional/arbitrage) — gains legitimacy for emergency power and ultimate authority claims; can invoke or refrain from invoking suspension
 *   - Sovereignty Theory (Jurisprudential Tradition): Beneficiary (institutional/arbitrage) — the dictatorship provides empirical proof that sovereignty precedes law; the constraint sustains the theoretical framework
 *   - Subject Populations During Suspension: Victim (powerless/trapped) — experience complete loss of legal protection; no exit from jurisdiction; suppression is total
 *   - Rule-of-Law Self-Image: Victim (moderate/constrained) — the constitutional order's normative identity is contradicted by its own exception clause; cannot exit the paradox without ceasing to be rule-of-law
 *   - Constitutional Guardians (Courts, Opposition): Organized actors (organized/constrained) — must coordinate restoration of law while forced to legally authorize law's negation; caught in the paradox's logical trap
 *   - Analytical Observer: Sees the paradox as either a logical necessity (mountain) or a contingent doctrinal commitment with identifiable beneficiaries (false summit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dictatorship_term_limited__suspension_paradox_reading, 0.68).
domain_priors:suppression_score(dictatorship_term_limited__suspension_paradox_reading, 0.72).
domain_priors:theater_ratio(dictatorship_term_limited__suspension_paradox_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dictatorship_term_limited__suspension_paradox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dictatorship_term_limited__suspension_paradox_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dictatorship_term_limited__suspension_paradox_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dictatorship_term_limited__suspension_paradox_reading, tangled_rope).
narrative_ontology:human_readable(dictatorship_term_limited__suspension_paradox_reading, "Dictatorship as Legal Suspension (Suspension Paradox Reading)").
narrative_ontology:topic_domain(dictatorship_term_limited__suspension_paradox_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(dictatorship_term_limited__suspension_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dictatorship_term_limited__suspension_paradox_reading, '0cf973a7-9bba-4d4d-9a4a-c68a1e761642').
narrative_ontology:cs_kernel_codification('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', fixed_text).
narrative_ontology:cs_authority_grounding('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', extraction).
narrative_ontology:cs_interpretation_layer_present('0cf973a7-9bba-4d4d-9a4a-c68a1e761642').
narrative_ontology:cs_reading_relation('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', dictatorship_term_limited__constitutional_emergency_reading, coexists_with).
narrative_ontology:cs_reading_relation('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', dictatorship_term_limited__precedent_for_caesarism_reading, influences).
narrative_ontology:cs_axiom('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', foundational, law_contains_own_negation).
narrative_ontology:cs_axiom_status(law_contains_own_negation, holdable).
narrative_ontology:cs_axiom_grounding('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', law_contains_own_negation, deontological).
narrative_ontology:cs_axiom('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', foundational, sovereignty_precedes_law).
narrative_ontology:cs_axiom_status(sovereignty_precedes_law, holdable).
narrative_ontology:cs_axiom_grounding('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', sovereignty_precedes_law, deontological).
narrative_ontology:cs_reference_frame('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', bounded_emergency_authority).
narrative_ontology:cs_drift_state('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', contemporary_institutional_capture, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0cf973a7-9bba-4d4d-9a4a-c68a1e761642', '').
narrative_ontology:cs_kernel_id(dictatorship_term_limited__suspension_paradox_reading, dictatorship_term_limited).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dictatorship_term_limited__suspension_paradox_reading, sovereign_authority).
narrative_ontology:constraint_beneficiary(dictatorship_term_limited__suspension_paradox_reading, sovereignty_theory).
narrative_ontology:constraint_victim(dictatorship_term_limited__suspension_paradox_reading, rule_of_law_self_image).
narrative_ontology:constraint_victim(dictatorship_term_limited__suspension_paradox_reading, normative_constitutional_order).
narrative_ontology:constraint_victim(dictatorship_term_limited__suspension_paradox_reading, subject_populations_during_suspension).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Trapped within the jurisdiction; the law's suspension means loss of all legal protections and remedies. No exit available; suppression is total (the law itself is suspended). The office exists to extract obedience without legal constraint.
constraint_indexing:constraint_classification(dictatorship_term_limited__suspension_paradox_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RULE-OF-LAW SELF-IMAGE (TANGLED ROPE) — The constitutional order must coordinate legitimate sovereign power (genuine coordination function); it also extracts by containing its own negation (the dictatorship provision). The constraint simultaneously coordinates law-bound governance and enables law-unbounded exception. Constrained exit: the self-image cannot exit the constitution without ceasing to be a rule-of-law order.
constraint_indexing:constraint_classification(dictatorship_term_limited__suspension_paradox_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOVEREIGN AUTHORITY (ROPE) — Experiences the dictatorship provision as a coordination mechanism: it legalizes emergency response, enables rapid decision-making during crisis, and preserves sovereignty's ultimate authority. The beneficiary sees the constraint as solving a genuine problem (how to respond to existential threat without dissolving the law). Arbitrage exit: can invoke or refrain from invoking the suspension.
constraint_indexing:constraint_classification(dictatorship_term_limited__suspension_paradox_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SOVEREIGNTY THEORY (ROPE) — The dictatorship is the proof that law contains its own negation and that sovereignty precedes legal form. The constraint enables a sophisticated claim: the constitution is law-shaped coordination that rests on sovereign power as its ground. Generational-horizon: the tradition benefits from the dictatorship as evidence for the sovereignty-first thesis.
constraint_indexing:constraint_classification(dictatorship_term_limited__suspension_paradox_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: CONSTITUTIONAL GUARDIANS (TANGLED ROPE) — Must coordinate the restoration of law after suspension while also preventing dictatorship abuse. The constraint enables their coordinating function (defining suspension bounds) but also extracts through the paradox itself: they must legally authorize its own negation. Constrained because the mechanism forces a choice: either validate the paradox (and accept the negation) or reject it (and lose the coordination tool).
constraint_indexing:constraint_classification(dictatorship_term_limited__suspension_paradox_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW READING (MOUNTAIN) — From a civilizational horizon, the dictatorship paradox reveals an immutable feature of legal order itself: law must contain the capacity to suspend itself to preserve itself in extremis. This is treated as an inherent logical property of any rule-based system with a self-preservation clause. However, the structural data contradicts this classification — the beneficiaries and suppression mechanisms are institutional, not logical. The engine will flag this as a false summit: naturalization of a contingent doctrinal choice.
constraint_indexing:constraint_classification(dictatorship_term_limited__suspension_paradox_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dictatorship_term_limited__suspension_paradox_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dictatorship_term_limited__suspension_paradox_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dictatorship_term_limited__suspension_paradox_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dictatorship_term_limited__suspension_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dictatorship_term_limited__suspension_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from the rule-of-law self-image by forcing constitutional order to contain its own negation. The sovereign authority and sovereignty theory benefit throughout the suspension interval. The extraction is not maximal (not 0.85+) because the beneficiaries' arbitrage exit option and the constraint's limited temporal scope (it is invoked periodically, not continuously) create measurable mobility. However, ε=0.68 reflects that the structural asymmetry is substantial: one beneficiary class gains legitimacy for unrestricted power; the victim class (subject populations and the rule-of-law norm) loses all protections. Suppression (0.72): High. The constraint suppresses alternatives: constitutional amendments that would remove the suspension clause face sovereignty theory opposition (the clause proves their core thesis); legal challenges to suspension invoke the clause's own legitimacy against their claims; exit from the jurisdiction is the only genuine alternative. The trajectory shows suppression requirement rising as the constraint normalizes: early invocations treated with legal formality; later invocations invoke the mechanism with less ceremony as it becomes institutionalized. Theater ratio (0.55): Moderate. The constraint has genuine extractive content (real suspensions occur; real powers are exercised) but also significant performative content (the doctrine of suspension is invoked to justify power that would be exercised anyway; the legal form obscures the political reality). The moderate ratio reflects that this reading rejects the view that the constraint is purely doctrinal theater — it has real suppressive force — but also rejects the view that it is pure functional emergency mechanism; the sovereignty theory that benefits from the paradox is invested in maintaining its doctrinal mystification.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap centers on whether the paradox is structural feature or contingent abuse. The sovereign authority and sovereignty theory see coordination (Rope) — a mechanism for legitimate response to genuine threats that preserves ultimate sovereignty. The rule-of-law self-image sees mixed coordination and extraction (Tangled Rope) — the constitution must coordinate emergency response but also extracts through the paradox it contains. The constitutional guardians see tangled rope as well but with different extraction vector: they coordinate restoration but are forced to validate the negation. Subject populations see pure extraction (Snare) — suspension means total loss of legal protection; the beneficiary extracts without constraint. The analytical observer risks seeing a logical necessity (Mountain) but the structural data reveals beneficiaries and victims, triggering false summit detection. The reading's central claim is that the paradox is not resolvable into 'emergency power plus bounds' — the bounds themselves are rhetorical theater; the real mechanism is sovereignty's self-assertion through law's negation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary and victim declarations plus exit options. Sovereign authority and sovereignty theory are beneficiaries with arbitrage exit — they can invoke or refrain from invoking the suspension, creating low directionality (d ≈ 0.15-0.25) and negative effective extraction (χ < 0 from their perspective). Subject populations are victims with trapped exit — complete loss of alternatives during suspension, creating high directionality (d ≈ 0.95) and maximum experienced extraction. The rule-of-law self-image is a victim with constrained exit — it cannot exit the paradox without abandoning its constitutional identity, creating high directionality (d ≈ 0.80). Constitutional guardians are caught between their coordinating function and forced validation of the negation; they are both moderately beneficiary-adjacent (they maintain legal order) and victim-adjacent (they must legally authorize law's negation), creating d ≈ 0.55 and experienced extraction that varies by institutional context. This perspectival variation is the constraint's essential feature: from the beneficiary's view, it is coordination; from the victim's view, it is extraction; from the guardian's view, it is paradox.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy in the classical sense (high extraction masquerading as high coordination). Instead, it exhibits what might be called 'paradox mandatrophy': the constraint's extractiveness is legitimate in the sense that it correctly identifies real asymmetry (one party benefits from legal self-negation; the other party suffers from legal self-negation), but the legitimacy claim rests on reframing extraction as coordination. The sovereign authority genuinely benefits from a mechanism that enables rapid response to threats — this is a real coordination problem. But the mechanism also enables extraction by re-defining suspension as lawful and normal — this transforms legitimacy claim into extraction mechanism. The constraint resolves by acknowledging that the coordinate and extractive functions cannot be separated: the same office that solves the emergency problem IS the mechanism by which law negates itself. There is no version of 'dictatorship' that is pure coordination without the capacity for abuse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_bounds_determinacy,
    'Can the constitutional bounds on dictatorship (temporal limit, scope of powers, triggering conditions) be legally specified in a way that prevents expansion?',
    'Historical comparative analysis of constitutional dictatorships: tracking which temporal and material bounds survived intact; analysis of whether each bound''s breach was preceded by explicit legal reinterpretation or by extra-legal seizure',
    'If bounds are determinable and respected: the constraint functions as Scaffold (temporary, with sunset enforced by law). If bounds systematically expand: the constraint functions as Snare (the legal bound is theater; the real suppression is extra-legal and disguised as legal).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_bounds_determinacy, empirical, 'Whether constitutional suspension bounds remain binding or systematically expand').

omega_variable(
    paradox_foreclosure_status,
    'Does the suspension paradox logically foreclose the constitutional_emergency_reading within a single coherent legal framework, or can both coexist?',
    'Formal logical analysis of the axioms declared by each reading; identification of whether the core claims directly contradict (foreclose) or occupy compatible logical spaces (coexist)',
    'If foreclosure: this reading eliminates the emergency reading as a coherent position. If coexistence: both readings remain live but point toward different institutional outcomes (paradox-accepting vs paradox-denying jurisprudence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradox_foreclosure_status, conceptual, 'Logical relationship between suspension paradox and constitutional emergency readings').

omega_variable(
    sovereignty_precedence_empirical_content,
    'Does the dictatorship provision reveal sovereignty as logically and causally prior to law, or does the claim rest on a metaphysical assertion that has no empirical test?',
    'Examine whether sovereignty_theory beneficiary claims can be falsified by historical or structural data; identify whether the ''sovereignty precedes law'' assertion would be abandoned if specific historical conditions changed',
    'If empirically tractable: the assertion is a contingent institutional fact. If metaphysical: the sovereignty_theory benefit from the dictatorship is a doctrinal commitment independent of structural outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_precedence_empirical_content, conceptual, 'Whether sovereignty-first claims rest on empirical or metaphysical foundations').

omega_variable(
    legitimate_emergency_vs_expansion,
    'Is extractiveness high because the mechanism itself is extractive, or because the mechanism is legitimate but systematically misused?',
    'Comparative institutional analysis: cases where suspension was invoked for genuine existential threat (narrow scope, brief duration, restored law afterward) vs cases where suspension was gateway to permanent power shift. Quantitative: proportion of historical invocations that resulted in restoration vs perpetuation.',
    'If legitimate but misused: the constraint''s ε should be lower (0.35-0.45); the high ε reflects abusive instantiation, not inherent structure. If inherently extractive: the high ε is accurate; the ''bounded emergency'' claim is the theater, not the reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimate_emergency_vs_expansion, empirical, 'Whether high extractiveness is structural or contingent on abuse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dictatorship_term_limited__suspension_paradox_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dict_tr_t0, dictatorship_term_limited__suspension_paradox_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dict_tr_t2, dictatorship_term_limited__suspension_paradox_reading, theater_ratio, 2, 0.52).
narrative_ontology:measurement(dict_tr_t4, dictatorship_term_limited__suspension_paradox_reading, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(dict_be_t0, dictatorship_term_limited__suspension_paradox_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dict_be_t2, dictatorship_term_limited__suspension_paradox_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(dict_be_t4, dictatorship_term_limited__suspension_paradox_reading, base_extractiveness, 4, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dict_su_t0, dictatorship_term_limited__suspension_paradox_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dict_su_t2, dictatorship_term_limited__suspension_paradox_reading, suppression_requirement, 2, 0.65).
narrative_ontology:measurement(dict_su_t4, dictatorship_term_limited__suspension_paradox_reading, suppression_requirement, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dictatorship_term_limited__suspension_paradox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dictatorship_term_limited__suspension_paradox_reading, constitutional_emergency_reading).
narrative_ontology:affects_constraint(dictatorship_term_limited__suspension_paradox_reading, precedent_for_caesarism_reading).

% DUAL FORMULATION NOTE:
% The dictatorship_term_limited kernel decomposes into three distinct constraint stories, each representing a different reading of the same historical and doctrinal material. The suspension_paradox_reading asserts that the dictatorship reveals law's self-negation (ε=0.68, Tangled Rope with false summit potential at analytical horizon). The constitutional_emergency_reading treats the dictatorship as a bounded emergency tool (different ε profile, different beneficiary/victim structure). The precedent_for_caesarism_reading interprets the dictatorship as rehearsal for permanent seizure of power (different causal mechanism, different extractiveness trajectory). Each reading's ε value reflects its structural claim: the suspension_paradox reading has high extractiveness because the core claim is that extraction-through-legal-negation is constitutive; the emergency reading would have lower extractiveness if written (the bounds are treated as binding); the caesarism reading would show high extractiveness through different mechanism (normalization of exception). All three stories are linked via network.affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dictatorship_term_limited__suspension_paradox_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
