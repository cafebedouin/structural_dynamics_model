% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__symbolic_myth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__symbolic_myth_reading, []).

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
 *   constraint_id: magna_carta_1215__symbolic_myth_reading
 *   human_readable: Magna Carta as Symbolic Myth: Authority Through Invocation Rather Than Text
 *   domain: legal/doctrinal
 *
 * SUMMARY:
 *   Magna Carta's actual institutional power derives not from the clauses
 *   themselves — most repealed, the remainder interpreted away — but from the
 *   myth that invoking the document constrains authority. This reading models
 *   the constraint as one of symbolic power: the document disciplines through
 *   its invocation, not through its content. Authority to wield the symbol
 *   becomes the primary beneficiary. Those expecting substantive protection
 *   from the clauses bear extraction via disappointed expectations. The
 *   constraint's extractiveness rises over time (0.35 → 0.58) as the gap
 *   between mythic invocation and textual reality widens: early iterations of
 *   Magna Carta (1215, 1217, 1225) involved genuine feudal negotiation over
 *   specific reliefs and wardships. By the 1600s, the myth had become
 *   decoupled from the text entirely — Sir Edward Coke invented the
 *   common-law constitution by reading medieval feudal clauses as if they
 *   established due process. By the contemporary period, invoking Magna Carta
 *   is purely performative, disciplining subjects through identity and
 *   legitimacy claims while remaining operationally inert. This reading
 *   instantiates the kernel through the symbolic-myth axis: Magna Carta's
 *   authority IS the invocation. The text is the artifact; the myth is the
 *   constraint.
 *
 * KEY AGENTS:
 *   - Power-Wielder Invoking the Myth (institutional/arbitrage): primary beneficiary — gains legitimacy and subject discipline through invoking Magna Carta without being bound by its clauses
 *   - Subject Expecting Textual Protection (powerless/trapped or identity_locked): primary victim — disciplined by the legend, protected by nothing substantive
 *   - Legal Establishment (institutional/arbitrage): secondary beneficiary — maintains doctrinal continuity by citing the myth without reading the text
 *   - Reform Coalition (organized/constrained): secondary victim/partial beneficiary — can invoke the myth for pressure but cannot make it substantive without delegitimizing their own framework
 *   - Textual Fidelity (abstract, powerless): victim — the actual clauses are invisible; historical accuracy is sacrificed to legitimacy
 *   - Historical Accuracy (abstract, powerless): victim — generations of misreading accumulate; the myth occludes what actually occurred in 1215
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__symbolic_myth_reading, 0.58).
domain_priors:suppression_score(magna_carta_1215__symbolic_myth_reading, 0.62).
domain_priors:theater_ratio(magna_carta_1215__symbolic_myth_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__symbolic_myth_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_1215__symbolic_myth_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magna_carta_1215__symbolic_myth_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__symbolic_myth_reading, snare).
narrative_ontology:human_readable(magna_carta_1215__symbolic_myth_reading, "Magna Carta as Symbolic Myth: Authority Through Invocation Rather Than Text").
narrative_ontology:topic_domain(magna_carta_1215__symbolic_myth_reading, "legal/doctrinal").

domain_priors:requires_active_enforcement(magna_carta_1215__symbolic_myth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__symbolic_myth_reading, 'f0a564ec-1895-468f-822a-14a78af43ff9').
narrative_ontology:cs_kernel_codification('f0a564ec-1895-468f-822a-14a78af43ff9', fixed_text).
narrative_ontology:cs_authority_grounding('f0a564ec-1895-468f-822a-14a78af43ff9', extraction).
narrative_ontology:cs_interpretation_layer_present('f0a564ec-1895-468f-822a-14a78af43ff9').
narrative_ontology:cs_reading_relation('f0a564ec-1895-468f-822a-14a78af43ff9', magna_carta_1215__common_law_foundation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0a564ec-1895-468f-822a-14a78af43ff9', magna_carta_1215__feudal_bargain_reading, influences).
narrative_ontology:cs_axiom('f0a564ec-1895-468f-822a-14a78af43ff9', foundational, authority_constituted_through_invocation).
narrative_ontology:cs_axiom_status(authority_constituted_through_invocation, holdable).
narrative_ontology:cs_axiom_grounding('f0a564ec-1895-468f-822a-14a78af43ff9', authority_constituted_through_invocation, conventional).
narrative_ontology:cs_axiom('f0a564ec-1895-468f-822a-14a78af43ff9', foundational, textual_fidelity_not_operative).
narrative_ontology:cs_axiom_status(textual_fidelity_not_operative, holdable).
narrative_ontology:cs_axiom_grounding('f0a564ec-1895-468f-822a-14a78af43ff9', textual_fidelity_not_operative, empirically_contingent).
narrative_ontology:cs_reference_frame('f0a564ec-1895-468f-822a-14a78af43ff9', mythic_invocation_authority).
narrative_ontology:cs_drift_state('f0a564ec-1895-468f-822a-14a78af43ff9', contemporary_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0a564ec-1895-468f-822a-14a78af43ff9', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__symbolic_myth_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__symbolic_myth_reading, power_wielder_invoking_myth).
narrative_ontology:constraint_beneficiary(magna_carta_1215__symbolic_myth_reading, legal_establishment_using_legend).
narrative_ontology:constraint_victim(magna_carta_1215__symbolic_myth_reading, textual_fidelity).
narrative_ontology:constraint_victim(magna_carta_1215__symbolic_myth_reading, historical_accuracy).
narrative_ontology:constraint_victim(magna_carta_1215__symbolic_myth_reading, agents_expecting_substantive_protection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED SUBJECT (SNARE) — A subject or petitioner invokes Magna Carta expecting substantive protection based on what the text claims (due process, limits on arbitrary power). The myth has conditioned them to believe the document provides real constraint. But the actual clauses — most repealed, never enforced as written — offer no protection. The text is dead; the myth is alive. Maximum extraction: the subject is disciplined by a legend that bears no relation to actual law.
constraint_indexing:constraint_classification(magna_carta_1215__symbolic_myth_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL BENEFICIARY (ROPE) — A king, parliament, or legal establishment benefits from invoking Magna Carta as constraint while being constrained only by the myth, not the text. The document serves as coordination mechanism: 'We govern under Magna Carta' coordinates legitimacy claims without limiting actual power. The beneficiary experiences this as pure coordination — the myth disciplines subjects' expectations while leaving power operational flexibility. No extraction perceived because no subject is demanding the text be honored.
constraint_indexing:constraint_classification(magna_carta_1215__symbolic_myth_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ORGANIZED REFORMERS (TANGLED ROPE) — A coalition of legal scholars, reform advocates, or parliamentarians sees Magna Carta as both coordinating principle (subjects have expectations; legitimacy requires invoking it) and extractive mechanism (those expectations are betrayed; the text is not honored). Reformers can organize and pressure for actual implementation, but face constraints: invoking the legend risks legitimizing the myth without achieving substance. They simultaneously benefit (the myth creates pressure for reform) and bear costs (the symbolic capture prevents substantive victory). Constrained exit: they can leave the system but cannot change it from within without legitimizing the myth.
constraint_indexing:constraint_classification(magna_carta_1215__symbolic_myth_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears immutable: all complex legal traditions rely on mythic documents whose power exceeds their textual specificity. Founding documents derive authority from invocation, not compliance. This looks like a natural law of how legality functions — the myth IS the constraint because authority is always constituted through symbolic invocation. However, this perspective risks naturalizing what is structurally a power asymmetry: the myth works because the text is invisible, and making the text visible (printing it, reading it, comparing it to current law) immediately threatens the myth's function. This is a false summit: naturalization of a contingent institutional artifact.
constraint_indexing:constraint_classification(magna_carta_1215__symbolic_myth_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: DOCTRINAL PITON (PITON) — Within legal scholarship and jurisprudence, Magna Carta persists as a foundational reference despite most of it being repealed or superseded. The doctrine serves a theatrical function: citing Magna Carta establishes legitimacy and continuity without requiring the document to actually govern. The performative citation of 'Magna Carta tradition' carries more weight than reading what Magna Carta actually says. Theater ratio is very high (0.81) — the invocation is almost entirely symbolic, the substance minimal. The constraint persists through inertia: to abandon the mythic reference would require admitting the founding document is largely irrelevant, which would undermine the entire legitimacy structure. So the doctrine is maintained performatively.
constraint_indexing:constraint_classification(magna_carta_1215__symbolic_myth_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: IDENTITY-LOCKED SUBJECT (SNARE) — Across generations, subjects internalize the mythic authority of Magna Carta as part of their national identity and legal expectations. They cannot exit the constraint because their sense of constitutional legitimacy is fused with the myth. A British subject invoking 'Magna Carta rights' has made the legend constitutive of their identity as a rights-bearing subject. Even if the text is dead, the identity is alive. The constraint extracts by defining what counts as 'legitimate expectation,' and the subject cannot imagine legitimate power without invoking the myth. This is snare with identity-lock: the text disciplines through the subject's own self-concept, not through external enforcement.
constraint_indexing:constraint_classification(magna_carta_1215__symbolic_myth_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__symbolic_myth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_1215__symbolic_myth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_1215__symbolic_myth_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__symbolic_myth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_1215__symbolic_myth_reading, TR),
    TR >= 0.70.

:- end_tests(magna_carta_1215__symbolic_myth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through the gap between symbolic promise (Magna Carta protects rights) and textual reality (most clauses repealed or operative only through reinterpretation). The extraction is not maximal (snare χ threshold is 0.66) because subjects still gain some legitimacy benefits and because some clauses (39–40) do shape doctrine, even if distorted. The myth creates a real coordinating norm that subjects value (sense of constitutional legitimacy) even though it offers no protection. So the extraction is real but not total — beneficiaries do provide something of value (symbolic authority), victims do receive something (identity within a named tradition), but the asymmetry is severe. Theater ratio (0.81): Very high. Almost all invocations of Magna Carta are symbolic — the document is cited for legitimacy, not consulted for specific guidance. Contemporary judges and lawyers cite Magna Carta's name without quoting its clauses. The actual substantive work is done by later law (common law, statute, constitution) that the myth has displaced from visibility. Suppression (0.62): High. The mechanism maintaining the constraint is preventing subjects from reading the actual text and comparing it to modern law. If a citizen invoked Magna Carta clause 39 in a contemporary court, the judges would have to explain why a clause about feudal reliefs is cited as constitutional foundation. The suppression is not violent — it is doctrinal: the teaching materials emphasize myth, not text. The myth suppresses through invisibility: the text is not forbidden, just not seen.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (institutional power-wielder) experiences the constraint as pure coordination: Magna Carta provides legitimacy without limiting function — this is Rope from the beneficiary's perspective. The trapped subject experiences pure extraction: the myth promises protection the text cannot deliver — this is Snare. The organized reformers see both: the myth is a tool they can leverage (coordination benefit) but cannot fulfill without delegitimizing their entire framework (extraction cost) — this is Tangled Rope. The historian or analyst risks naturalizing the myth as an immutable feature of how legal authority works (Mountain) when it is actually a contingent power asymmetry. The doctrinal system maintains the myth performatively as a degraded tradition (Piton). The generational subject has fused their legal identity with the myth, making exit cognitive rather than material (identity_locked Snare). No single perspective is 'correct' — they are all reports from within different structural positions relative to the myth.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is computed from beneficiary/victim status and exit options. The institutional beneficiary has arbitrage options (can shift legitimacy claims to other documents if needed) but is incentivized to maintain the Magna Carta myth because it works efficiently. Their d is low (~0.15), producing negative χ — they experience the constraint as beneficial coordination. The trapped subject has no exit and bears full cost — their d is high (~0.95), producing maximum χ. The identity-locked subject has structural mobility (could leave the country, adopt a different legal system) but cannot exercise it because their constitutional identity is fused with Magna Carta mythology — their d is derived from victim status + identity_locked exit, producing high f(d). The organized reformers face constraints (the myth is harder to change than to accept) but have agency (can pressure for reform, can appeal to the myth itself as source of legitimacy pressure) — their d is moderate. The analytical observer's d is derived from canonical analytical position (0.73), producing the mountain classification risk (false summit).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    myth_versus_text_boundary,
    'At what point does repeated misreading of a text become a different constraint than the text itself? When does the myth become analytically prior to the artifact?',
    'Empirical: document which invocations of Magna Carta cite the actual text vs. cite the legend. Compare the two sets for contradictions. If zero correlation exists between what the text says and how it is invoked, the myth and text are structurally independent constraints.',
    'If myth is prior: this constraint (snare via invocation) is analytically primary; the common-law-foundation reading is studying a dead text. If text and myth are entangled: decompose into separate constraints (one per reading) linked by network edges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(myth_versus_text_boundary, empirical, 'Whether the myth and text are structurally independent constraints or one constraint with two interpretations').

omega_variable(
    benefit_concentration_shifting,
    'Who benefits from the Magna Carta myth in THIS century? Has the beneficiary set shifted across generations?',
    'Historical analysis of invocations: identify who cited Magna Carta and what they gained. Map the beneficiary set across periods: 1215, 1297, 1600s, 1700s American independence, 1800s, 1900s, contemporary. Look for shifts in who wields the symbol and what power flows to them.',
    'If beneficiary is stable (always the institutional power-wielder): the constraint is a permanent asymmetry. If beneficiary shifts (sometimes reformers use the myth against incumbents): the constraint alternates between snare and tangled rope depending on who controls the invocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_concentration_shifting, empirical, 'Whether the beneficiary of the Magna Carta myth is fixed or shifts across history').

omega_variable(
    reading_contention_mechanism,
    'Does this reading (myth as power via invocation) foreclose the common-law-foundation reading? Can both readings coexist within a single framework?',
    'Logical: if the common-law reading asserts that Magna Carta''s clauses founded due process and shaped constitutional law, and this reading asserts that the actual clauses are mostly dead and the myth is what disciplines, can both be true? Check whether a single legal scholar or institution can hold both positions without contradiction.',
    'If foreclosed: only one reading can be historically accurate; the other is a misunderstanding. If coexistent: they describe different mechanisms at different levels of analysis (the myth creates conditions for common-law development, but the myth is not the same as the text). If influencing: one reading makes the other harder to maintain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_mechanism, conceptual, 'Logical relationship between symbolic-myth reading and common-law-foundation reading').

omega_variable(
    invocation_threshold_for_extraction,
    'How many times must a text be invoked without being read/enforced before the invocation becomes the only operative constraint?',
    'Measure: ratio of citations to Magna Carta in legal opinions vs. citations to its actual clauses. If the ratio of mythic invocation to substantive textual use exceeds ~10:1 consistently across a period, the myth has achieved independence from the text.',
    'If threshold is crossed: this reading (myth-as-constraint) is correct; the text is vestigial. If threshold is not crossed: the common-law reading remains partially accurate; the text still shapes doctrine even through selective invocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invocation_threshold_for_extraction, empirical, 'Invocation-to-substantive-use ratio threshold for declaring myth independence').

omega_variable(
    first_principles_constraint_identity,
    'Is this constraint about Magna Carta itself, or about a general pattern where foundational myths discipline through invocation rather than text? If the latter, should it be decomposed into a separate constraint about symbolic legal authority?',
    'Check: would this story be materially different if applied to the U.S. Constitution, the French Declaration of Rights, or any founding document? If yes, the constraint is Magna Carta-specific. If no, the constraint is a pattern that should be modeled separately (symbolic_legal_myth_authority) and linked to Magna Carta via network edges.',
    'If Magna Carta-specific: this story captures the actual historical extraction of this specific document''s myth. If pattern-level: decompose and create a second constraint for the general mechanism, linking both back to the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_principles_constraint_identity, conceptual, 'Whether constraint is Magna Carta-specific or instance of a general symbolic-myth pattern').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__symbolic_myth_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_theater_t0_1215, magna_carta_1215__symbolic_myth_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(magna_carta_theater_t1_1600s, magna_carta_1215__symbolic_myth_reading, theater_ratio, 400, 0.76).
narrative_ontology:measurement(magna_carta_theater_t2_1800s, magna_carta_1215__symbolic_myth_reading, theater_ratio, 600, 0.82).
narrative_ontology:measurement(magna_carta_theater_t3_contemporary, magna_carta_1215__symbolic_myth_reading, theater_ratio, 800, 0.81).

% Extraction over time
narrative_ontology:measurement(magna_carta_extract_t0_1215, magna_carta_1215__symbolic_myth_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(magna_carta_extract_t1_1600s, magna_carta_1215__symbolic_myth_reading, base_extractiveness, 400, 0.48).
narrative_ontology:measurement(magna_carta_extract_t2_1800s, magna_carta_1215__symbolic_myth_reading, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(magna_carta_extract_t3_contemporary, magna_carta_1215__symbolic_myth_reading, base_extractiveness, 800, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_supp_t0_1215, magna_carta_1215__symbolic_myth_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(magna_carta_supp_t1_1600s, magna_carta_1215__symbolic_myth_reading, suppression_requirement, 400, 0.58).
narrative_ontology:measurement(magna_carta_supp_t2_1800s, magna_carta_1215__symbolic_myth_reading, suppression_requirement, 600, 0.65).
narrative_ontology:measurement(magna_carta_supp_t3_contemporary, magna_carta_1215__symbolic_myth_reading, suppression_requirement, 800, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__symbolic_myth_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__symbolic_myth_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__symbolic_myth_reading, magna_carta_1215__common_law_foundation_reading).
narrative_ontology:affects_constraint(magna_carta_1215__symbolic_myth_reading, magna_carta_1215__feudal_bargain_reading).
narrative_ontology:affects_constraint(magna_carta_1215__symbolic_myth_reading, symbolic_legal_authority_general).

% DUAL FORMULATION NOTE:
% The Magna Carta kernel generates three distinct constraint stories corresponding to three readings. This story (symbolic_myth) models the constraint as one where the myth has achieved independence from the text (ε=0.58, Snare). The common_law_foundation reading models the constraint as one where the clauses shape doctrine despite being repealed (ε lower, classification Rope or Tangled Rope depending on reading specificity). The feudal_bargain reading models the constraint as historical artifact with no operative modern mechanism (ε very low, classification Mountain or Piton depending on whether any doctrinal residue exists). All three link to a general-pattern constraint about symbolic_legal_authority (how any foundational myth disciplines through invocation). The three readings are not observational alternatives to the same constraint — they are decompositions of the kernel into three structurally distinct constraints with different ε values and different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
