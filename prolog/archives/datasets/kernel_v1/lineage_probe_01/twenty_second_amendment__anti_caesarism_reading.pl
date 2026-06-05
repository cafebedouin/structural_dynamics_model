% ============================================================================
% CONSTRAINT STORY: twenty_second_amendment__anti_caesarism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_twenty_second_amendment__anti_caesarism_reading, []).

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
 *   constraint_id: twenty_second_amendment__anti_caesarism_reading
 *   human_readable: The Twenty-Second Amendment as Anti-Caesarism: Rotation Entrenched Against the Popular Strongman
 *   domain: constitutional_law/executive_power
 *
 * SUMMARY:
 *   The Twenty-Second Amendment (ratified 1951) codified into constitutional
 *   law the informal Washington norm of two-term presidential rotation,
 *   broken by FDR's election to four terms (1932-1945). This constraint story
 *   instantiates the ANTI-CAESARISM READING: the two-term limit as a
 *   structural safeguard against indefinite executive power accumulation —
 *   the popular strongman prevented by text from perpetual incumbency. The
 *   reading interprets the amendment's core function as preventing the
 *   transformation of the presidency into a personal office held indefinitely
 *   by a single leader, regardless of electoral preference. Suppression is
 *   high (0.65) because the mechanism is legal prohibition: no amendment, no
 *   process short of constitutional revolution can restore the third-term
 *   option. But extractiveness is moderate (0.38) because the constraint
 *   coordinates genuine succession benefits while extracting from successful
 *   presidents. The constraint exhibits perspectival divergence across all
 *   six types, revealing that the 'true' classification depends on
 *   observational position: beneficiaries of rotation see rope; trapped
 *   supporters of a strongman see snare; the franchise sees tangled rope
 *   (coordination + choice override); the anti-Caesarism covenant itself,
 *   viewed as ritual, appears piton (performative reaffirmation of commitment
 *   to rotation). Time interval spans from the amendment's ratification (t=0)
 *   to the present (t=72 years, approximately), showing that suppression and
 *   extractiveness have remained stable after initial codification, while
 *   theater has risen slightly as the ritual reaffirmation of anti-Caesarism
 *   becomes more symbolic.
 *
 * KEY AGENTS:
 *   - Rotation Principle & Democratic Succession: Beneficiary (institutional/arbitrage) — the structural commitment to periodic power transfer
 *   - Electoral Challengers & Opposition: Beneficiary (institutional/arbitrage) — guaranteed periodic opportunity to contest power
 *   - The Sitting President (especially high-approval second-term): Victim (moderate/constrained) — capped at eight years regardless of electoral demand
 *   - Popular Strongman Movement & Supporters: Victim (powerless/trapped) — indefinitely barred from re-electing preferred leader by constitutional prohibition
 *   - The Franchise / Voters: Constrained beneficiary (organized/constrained) — gains succession coordination but loses unrestricted choice
 *   - The Anti-Caesarism Covenant: Institutional actor (institutional/analytical) — the constitutional text as reaffirmation of commitment to prevent personal rule
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(twenty_second_amendment__anti_caesarism_reading, 0.38).
domain_priors:suppression_score(twenty_second_amendment__anti_caesarism_reading, 0.65).
domain_priors:theater_ratio(twenty_second_amendment__anti_caesarism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(twenty_second_amendment__anti_caesarism_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(twenty_second_amendment__anti_caesarism_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(twenty_second_amendment__anti_caesarism_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(twenty_second_amendment__anti_caesarism_reading, tangled_rope).
narrative_ontology:human_readable(twenty_second_amendment__anti_caesarism_reading, "The Twenty-Second Amendment as Anti-Caesarism: Rotation Entrenched Against the Popular Strongman").
narrative_ontology:topic_domain(twenty_second_amendment__anti_caesarism_reading, "constitutional_law/executive_power").

domain_priors:requires_active_enforcement(twenty_second_amendment__anti_caesarism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(twenty_second_amendment__anti_caesarism_reading, 'dced3627-ce68-47a1-9468-945a855aa44d').
narrative_ontology:cs_kernel_codification('dced3627-ce68-47a1-9468-945a855aa44d', formalized).
narrative_ontology:cs_authority_grounding('dced3627-ce68-47a1-9468-945a855aa44d', lineage).
narrative_ontology:cs_interpretation_layer_present('dced3627-ce68-47a1-9468-945a855aa44d').
narrative_ontology:cs_reading_relation('dced3627-ce68-47a1-9468-945a855aa44d', twenty_second_amendment__democratic_choice_objection_reading, coexists_with).
narrative_ontology:cs_reading_relation('dced3627-ce68-47a1-9468-945a855aa44d', twenty_second_amendment__lame_duck_cost_reading, coexists_with).
narrative_ontology:cs_axiom('dced3627-ce68-47a1-9468-945a855aa44d', foundational, indefinite_executive_accumulation_is_dangerous).
narrative_ontology:cs_axiom_status(indefinite_executive_accumulation_is_dangerous, holdable).
narrative_ontology:cs_axiom_grounding('dced3627-ce68-47a1-9468-945a855aa44d', indefinite_executive_accumulation_is_dangerous, deontological).
narrative_ontology:cs_axiom('dced3627-ce68-47a1-9468-945a855aa44d', foundational, rotation_principle_transcends_individual_preference).
narrative_ontology:cs_axiom_status(rotation_principle_transcends_individual_preference, holdable).
narrative_ontology:cs_axiom_grounding('dced3627-ce68-47a1-9468-945a855aa44d', rotation_principle_transcends_individual_preference, deontological).
narrative_ontology:cs_reference_frame('dced3627-ce68-47a1-9468-945a855aa44d', washington_rotation_norm_as_constitutional_commitment).
narrative_ontology:cs_drift_state('dced3627-ce68-47a1-9468-945a855aa44d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dced3627-ce68-47a1-9468-945a855aa44d', '').
narrative_ontology:cs_kernel_id(twenty_second_amendment__anti_caesarism_reading, twenty_second_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(twenty_second_amendment__anti_caesarism_reading, rotation_principle).
narrative_ontology:constraint_beneficiary(twenty_second_amendment__anti_caesarism_reading, electoral_challengers).
narrative_ontology:constraint_beneficiary(twenty_second_amendment__anti_caesarism_reading, institutional_checks_on_executive).
narrative_ontology:constraint_victim(twenty_second_amendment__anti_caesarism_reading, successful_presidents_and_movements).
narrative_ontology:constraint_victim(twenty_second_amendment__anti_caesarism_reading, presidential_mandate_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPULAR STRONGMAN SUPPORTER (SNARE) — A president with demonstrated electoral dominance and policy agenda continuation cannot by constitutional mandate seek a third term, regardless of voter preference. The supporter is trapped by text: even if the electorate wishes to re-elect, the amendment blocks the choice unconditionally. Maximum suppression of exit — the mechanism is legal prohibition, not persuasion. The extraction is the forced termination of a preferred leader.
constraint_indexing:constraint_classification(twenty_second_amendment__anti_caesarism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SITTING PRESIDENT (TANGLED ROPE) — The two-term limit provides genuine coordination benefits: it clarifies succession planning, enables party renewal, and creates predictable transitions. But it also extracts from the president by capping accumulated executive power and eliminating the possibility of indefinite incumbency. The cost is constrained but surmountable — the president can remain politically active, build legacy, shape party direction. Mixed coordination (succession clarity) and extraction (power cap).
constraint_indexing:constraint_classification(twenty_second_amendment__anti_caesarism_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPPOSITION PARTY & ELECTORAL CHALLENGERS (ROPE) — The two-term limit is pure coordination. It guarantees periodic opportunity for power transfer, prevents indefinite incumbent advantage, and enables predictable campaign timing. The beneficiary experiences this as coordination mechanism solving the collective action problem: 'How do we ensure periodic contestation?' The answer is rotation. Arbitrage exit because the opposition can always exit the constraint by winning and taking advantage of the limitation that now applies to their opponent.
constraint_indexing:constraint_classification(twenty_second_amendment__anti_caesarism_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE DEMOCRATIC SYSTEM / ROTATION PRINCIPLE (ROPE) — From the perspective of the constitutional system qua rotation mechanism, the two-term limit is pure coordination without extraction. It solves the structural problem: 'How do we prevent the office from becoming hereditary or perpetual?' The limit enforces periodic renewal. No hidden costs or coercion from the system's own point of view — the mechanism works as designed. The beneficiary IS the system's functioning.
constraint_indexing:constraint_classification(twenty_second_amendment__anti_caesarism_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE FRANCHISE / POPULAR SOVEREIGNTY (TANGLED ROPE) — The two-term limit imposes a preemptive category restriction on voter choice: regardless of how strongly the electorate wishes to re-elect a president, that choice is forbidden in advance. The limit coordinates succession and rotation (genuine function). But it also extracts by overriding majority preference — the voters' sovereign choice is constrained by text. The franchise has constrained exit: it cannot amend the limit without supermajority consensus (27 states + both chambers + president signing = extremely difficult). Mixed coordination (succession clarity) and extraction (choice suppression).
constraint_indexing:constraint_classification(twenty_second_amendment__anti_caesarism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE ANTI-CAESARISM COVENANT AS RITUAL (PITON) — Viewed from a civilizational timescale, the two-term limit functions as ritualized anti-Caesarism: a formal, repeated, ceremonial reaffirmation that 'we do not have and will not have a permanent strongman.' The mechanism is partly substantive (structural prevention) and partly performative (restatement of the commitment to rotation). Theater ratio 0.35 reflects that the limit's core function — preventing indefinite incumbency — is real, but much of its force comes from the ritual reaffirmation that rotation is sacred, not from the constitutional text alone. The piton reading views this as potentially degraded if the underlying commitment to rotation weakens (e.g., if a successful president's party moves to amend the amendment, the piton's theatrical force exposes its fragility).
constraint_indexing:constraint_classification(twenty_second_amendment__anti_caesarism_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(twenty_second_amendment__anti_caesarism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(twenty_second_amendment__anti_caesarism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(twenty_second_amendment__anti_caesarism_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(twenty_second_amendment__anti_caesarism_reading, TR),
    TR >= 0.70.

:- end_tests(twenty_second_amendment__anti_caesarism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The amendment produces real extraction: it prevents a successful president and their movement from indefinite incumbency, imposing an eight-year ceiling on concentrated executive power. But extractiveness is not high because the extraction is limited to the temporal dimension — eight years is a substantial period, and the constraint does coordinate genuine succession benefits (preventing power vacuum, enabling party renewal, clarifying succession timing). The increase from t=0 (0.15) to t=16 (0.38) reflects that the amendment's impact became empirically clear after multiple successful applications: Eisenhower, Kennedy, Johnson, Reagan, Clinton demonstrated that the two-term limit was not a constitutional ornament but a working constraint. Suppression (0.65): High. The mechanism is legal prohibition, not persuasion or incentive. Once the amendment is ratified, there is no exit mechanism short of constitutional amendment (requiring 27 states + both chambers + president, effectively impossible in peacetime). A president cannot negotiate, lobby, or demonstrate their way out of the two-term limit. A movement cannot argue for an exception based on popularity or circumstance. The suppression value captures this structural immobility. Theater (0.35): Moderate-low. The constraint's function is primarily substantive — it actually prevents indefinite incumbency. But some theater is present: the reaffirmation of anti-Caesarism through adherence to the limit is partly ceremonial, especially when sitting presidents who might theoretically amend the amendment choose to respect it instead. The slight rise from t=0 (0.25) to t=16 (0.35) reflects increasing ritualization as the limit became normalized in practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates full perspectival divergence. The rotation principle sees rope (pure coordination — the constraint solves the succession problem). Electoral challengers see rope (guaranteed periodic contestation). The sitting president sees tangled rope (genuine succession benefits offset by power cap). Popular strongman supporters see snare (trapped indefinitely by legal prohibition). The franchise sees tangled rope (succession coordination offset by choice override). The anti-Caesarism covenant, viewed as ritual, appears piton (performative reaffirmation of commitment to rotation). The deepest gap: between the beneficiary perspectives (rope, institutional actors with arbitrage exit) and the victim perspectives (snare/tangled_rope, constrained or powerless actors). This gap reveals the amendment's core tension — it solves a coordination problem for the system while imposing a structural extraction on presidents and their movements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and power atoms. The rotation principle and challengers are beneficiaries with institutional power and arbitrage-level exit (they can amend the constitution, though difficultly); they experience low effective extraction because the constraint enables their activity. Successful presidents are victims with institutional power but constrained exit (they face legal prohibition); they experience high effective extraction because the constraint forcibly ends their tenure. Popular strongman supporters are victims with powerless status and trapped exit (legal prohibition bars their preferred outcome unconditionally); they experience maximum effective extraction (high d, high f(d)). The franchise is a complex actor: it is technically sovereign but face constrained exit from the amendment (supermajority requirement to amend). The amendment constrains the franchise's choices while providing succession coordination; this mixed structure places the franchise in the tangled_rope perspective with constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the classification depends on observational position. From the institutional beneficiary position (rotation principle, challengers), the constraint is rope (pure coordination). From the victim position (successful presidents, strongman supporters), the constraint is snare or tangled rope (extraction + some coordination). The analytical observer must choose which tension to emphasize: the coordination benefits (rotation, succession clarity) or the extraction costs (power cap, choice override). The anti-Caesarism reading emphasizes the coordination function — it interprets the amendment as solving the structural problem of indefinite executive power accumulation. But the reading acknowledges through omega variables that alternative readings (democratic choice, lame duck) are equally valid interpretations of the same text, each generating different classifications from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caesarism_definition_boundary,
    'What constitutes ''Caesarism'' that the amendment intends to prevent — indefinite incumbency, or indefinite power concentration by any single political movement?',
    'Historical analysis of founding intent (debates in 1947-1951); comparison to instances where term-limited presidents remained politically dominant through constitutional succession or party control',
    'If indefinite incumbency only: the constraint succeeds if a president serves two terms and exits. If indefinite power concentration: the constraint may fail if a president''s movement remains dominant through successors or if ex-presidents retain informal influence. Impacts beneficiary/victim classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caesarism_definition_boundary, conceptual, 'Scope of anti-Caesarism: personal incumbency vs. movement continuity').

omega_variable(
    democratic_choice_vs_rotation_precedence,
    'When voter preference for a third term conflicts with the rotation principle, which takes precedence — the immediate democratic choice or the long-term constitutional commitment to periodic renewal?',
    'Textual analysis of the amendment''s preamble and ratification debates; comparative constitutional frameworks (e.g., countries with no term limits vs. strict limits); empirical analysis of public opinion on term limits during high-approval presidencies',
    'If immediate choice precedence: the amendment extracts by overriding sovereign will and belongs in snare/high-extraction territory. If rotation precedence: the amendment coordinates by protecting the system''s structural renewal, and belongs in rope/low-extraction. This is the core theoretical divide between this reading and the democratic_choice_objection_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_choice_vs_rotation_precedence, preference, 'Precedence of voter preference vs. constitutional rotation principle').

omega_variable(
    washington_norm_codification_necessity,
    'Was constitutional codification (22nd Amendment) necessary to enforce Washington''s rotation norm, or did the amendment formalize a norm that was already stable and risked brittleness by juridification?',
    'Counterfactual analysis: what would have happened to presidential succession norms if FDR had been followed by other strong presidents without amendment? Comparison to other norms (e.g., Senate filibuster) that persisted without formal law. Historical analysis of post-FDR norm strength before vs. after 1951 ratification.',
    'If codification was necessary: the amendment solved a genuine structural problem (norms break, as Roosevelt proved). If codification risked degrading norm stability: the amendment may have converted a resilient informal constraint into a fragile legal one. Affects whether the beneficiary (rotation) is strengthened or weakened by formalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(washington_norm_codification_necessity, empirical, 'Whether formalization of Washington norm strengthened or risked degrading the rotation principle').

omega_variable(
    reading_kernel_contest,
    'Is the Twenty-Second Amendment fundamentally a constraint on Caesarism (this reading), or is it fundamentally a constraint on democratic choice (democratic_choice_objection_reading), or is it fundamentally a tradeoff that lames second-term presidents (lame_duck_cost_reading)?',
    'This omega documents the committer-axis contest. The three readings are live positions held by different constitutional scholars, courts, and political movements. No single reading forecloses another within a single consistent constitutional framework — the contest is real and ongoing. The resolution mechanism is interpretive tradition: which reading''s foundational axiom becomes more or less central to constitutional practice over generational timescales.',
    'This reading (anti-Caesarism) dominates when the threat model is indefinite executive power accumulation. Democratic-choice reading dominates when the threat model is majoritarianism overridden by supermajority entrenchment. Lame-duck reading dominates when the empirical focus is presidential effectiveness in second terms. All three are active readings in contemporary constitutional discourse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'The Twenty-Second Amendment is one contested kernel with three live readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(twenty_second_amendment__anti_caesarism_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_ac_theater_t0, twenty_second_amendment__anti_caesarism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tsl_ac_theater_t16, twenty_second_amendment__anti_caesarism_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(tsl_ac_theater_t72, twenty_second_amendment__anti_caesarism_reading, theater_ratio, 72, 0.35).

% Extraction over time
narrative_ontology:measurement(tsl_ac_extract_t0, twenty_second_amendment__anti_caesarism_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tsl_ac_extract_t16, twenty_second_amendment__anti_caesarism_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(tsl_ac_extract_t72, twenty_second_amendment__anti_caesarism_reading, base_extractiveness, 72, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(tsl_ac_supp_t0, twenty_second_amendment__anti_caesarism_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tsl_ac_supp_t16, twenty_second_amendment__anti_caesarism_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(tsl_ac_supp_t72, twenty_second_amendment__anti_caesarism_reading, suppression_requirement, 72, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(twenty_second_amendment__anti_caesarism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(twenty_second_amendment__anti_caesarism_reading, twenty_second_amendment__democratic_choice_objection_reading).
narrative_ontology:affects_constraint(twenty_second_amendment__anti_caesarism_reading, twenty_second_amendment__lame_duck_cost_reading).

% DUAL FORMULATION NOTE:
% The Twenty-Second Amendment kernel has three structurally distinct readings, each with different ε values and beneficiary/victim structures. The anti-Caesarism reading treats the amendment as preventing indefinite power accumulation (moderate extraction, high suppression, moderate theater). The democratic-choice reading would treat the amendment as overriding voter preference (higher extraction, higher suppression of choice). The lame-duck reading would treat the amendment as imposing second-term paralysis (different ε, different suppression profile). These three are separate constraint stories linked by the same kernel; they are not different observables of the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
