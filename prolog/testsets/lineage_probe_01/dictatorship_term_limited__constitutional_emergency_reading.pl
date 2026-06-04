% ============================================================================
% CONSTRAINT STORY: dictatorship_term_limited__constitutional_emergency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dictatorship_term_limited__constitutional_emergency_reading, []).

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
 *   constraint_id: dictatorship_term_limited__constitutional_emergency_reading
 *   human_readable: Constitutionalized Emergency Dictatorship (Term-Limited Reading)
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   The constitutional dictatorship instantiates an attempt to domesticate
 *   emergency power through naming, temporal bounding, and the promise of
 *   restoration: the danger is specified (the emergency condition), the time
 *   is bounded (the term of the dictatorship), and absolute power is granted
 *   with the explicit condition that it returns its commission to the legal
 *   order. This reading generates a tangled-rope structure where the legal
 *   order gains the unified command capacity necessary to survive the crisis
 *   AND imposes suppressive constraints on what kinds of emergency claims can
 *   be made (must be named, must be bounded, must include restoration
 *   mechanism). The constraint exhibits high suppression (0.65-0.68) because
 *   alternative emergency doctrines are foreclosed — the constitution
 *   stipulates THIS mechanism, not others. Extractiveness rises slightly over
 *   the interval (0.42 → 0.56) as the dictator's actual power accumulates
 *   during the emergency term, and as the gap between the restoration promise
 *   and the restoration deadline becomes apparent. Theater ratio rises (0.48
 *   → 0.62) as the ritual of emergency declaration and dictatorship
 *   appointment becomes formalized, increasingly decoupled from the actual
 *   functional crisis response. The constraint is one reading of a contested
 *   kernel (dictatorship_term_limited), whose sibling readings are the
 *   Caesarist precedent reading (normalizing indefinite extension through
 *   historical rehearsal) and the suspension paradox reading (the paradox of
 *   an office inside the law whose content is the law's absence). This
 *   reading forecloses neither sibling reading conclusively but coexists with
 *   both: the legal order can hold this reading while the caesarist history
 *   and the paradox remain structurally present.
 *
 * KEY AGENTS:
 *   - The Republican Authority Structure: Primary beneficiary (institutional/arbitrage) — retains the capacity to name emergencies, set time bounds, and demand restoration. Gains crisis-solving capacity without permanent power transfer.
 *   - The Legal Order (as abstract entity): Secondary beneficiary (institutional/arbitrage) — survives its own suspension through the constitutional mechanism that stipulates the suspension will end.
 *   - The Dictator: Tertiary beneficiary (institutional/arbitrage, or moderate/constrained if reluctant) — gains absolute power for a bounded term, promised restoration of normal channels afterward. Net position depends on whether dictator intends restoration (Cincinnatus model) or extension (Sulla/Caesar model).
 *   - Citizens Under Emergency: Primary victim (powerless/trapped) — rights suspended, due process eliminated, resistance to emergency measures constitutionally barred. No exit option during the term; forced to assume the restoration will occur.
 *   - Unrestricted Emergency Claims: Victim set (analytical/analytical) — alternative emergency doctrines (pure state-of-exception, parliamentary emergency, distributed authority) are suppressed by the constitutional codification of THIS mechanism exclusively.
 *   - Separation of Powers During Exception: Victim set (institutional/constrained) — other branches subordinated to the dictator during the emergency; must trust that subordination will end when the term expires.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dictatorship_term_limited__constitutional_emergency_reading, 0.52).
domain_priors:suppression_score(dictatorship_term_limited__constitutional_emergency_reading, 0.65).
domain_priors:theater_ratio(dictatorship_term_limited__constitutional_emergency_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dictatorship_term_limited__constitutional_emergency_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(dictatorship_term_limited__constitutional_emergency_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dictatorship_term_limited__constitutional_emergency_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dictatorship_term_limited__constitutional_emergency_reading, tangled_rope).
narrative_ontology:human_readable(dictatorship_term_limited__constitutional_emergency_reading, "Constitutionalized Emergency Dictatorship (Term-Limited Reading)").
narrative_ontology:topic_domain(dictatorship_term_limited__constitutional_emergency_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(dictatorship_term_limited__constitutional_emergency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dictatorship_term_limited__constitutional_emergency_reading, 'ea772fb5-3fd3-4b12-b443-4c48da8c1d80').
narrative_ontology:cs_kernel_codification('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', formalized).
narrative_ontology:cs_authority_grounding('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', lineage).
narrative_ontology:cs_interpretation_layer_present('ea772fb5-3fd3-4b12-b443-4c48da8c1d80').
narrative_ontology:cs_reading_relation('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', dictatorship_term_limited__dictatorship_precedent_for_caesarism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', dictatorship_term_limited__dictatorship_suspension_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', foundational, emergency_power_constitutionalizable).
narrative_ontology:cs_axiom_status(emergency_power_constitutionalizable, holdable).
narrative_ontology:cs_axiom_grounding('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', emergency_power_constitutionalizable, conventional).
narrative_ontology:cs_axiom('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', foundational, voluntary_restoration_mechanism_reliable).
narrative_ontology:cs_axiom_status(voluntary_restoration_mechanism_reliable, holdable).
narrative_ontology:cs_axiom_grounding('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', voluntary_restoration_mechanism_reliable, empirically_contingent).
narrative_ontology:cs_reference_frame('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', classical_roman_dictatorship_with_restoration).
narrative_ontology:cs_drift_state('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', modern_emergency_law_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ea772fb5-3fd3-4b12-b443-4c48da8c1d80', '').
narrative_ontology:cs_kernel_id(dictatorship_term_limited__constitutional_emergency_reading, dictatorship_term_limited).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dictatorship_term_limited__constitutional_emergency_reading, legal_order_survival).
narrative_ontology:constraint_beneficiary(dictatorship_term_limited__constitutional_emergency_reading, republican_authority_structure).
narrative_ontology:constraint_victim(dictatorship_term_limited__constitutional_emergency_reading, unrestricted_emergency_claims).
narrative_ontology:constraint_victim(dictatorship_term_limited__constitutional_emergency_reading, separation_of_powers_during_exception).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZEN UNDER EMERGENCY (SNARE) — Individual rights and due process are suspended by design. Exit is constitutionally barred during the emergency term. The citizen experiences maximum suppression with no recourse except the dictator's promised restoration. The threat of indefinite extension (if the dictator refuses to lay down power) makes this snare rather than tangled rope — extraction and suppression are high, coordination benefit is zero.
constraint_indexing:constraint_classification(dictatorship_term_limited__constitutional_emergency_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REPUBLICAN INSTITUTIONS (TANGLED ROPE) — The Senate delegates absolute power to the dictator for a named emergency and bounded time, receiving in return the guarantee that the exception will end and constitutional order will resume. This is genuine coordination (solving the crisis requires unified command) AND asymmetric extraction (the dictator wields absolute power during the term, constrained institutions absorb the cost of powerlessness). The coordination function is real; so is the extraction. The constraint's suppression of rival emergency doctrines (like open-ended state-of-exception language) is enforced through the constitutional amendment that codifies the dictatorship. Extraction is moderated by the constitutional time bound and the explicit sunset — but the suppression is high because the dictator's power during the term is absolute.
constraint_indexing:constraint_classification(dictatorship_term_limited__constitutional_emergency_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE DICTATOR / CINCINNATUS MODEL (ROPE) — Experiences the constraint as pure coordination: the republican authority structure grants absolute power in exchange for the dictator's promise to restore it. The dictator who accepts this bargain sees the term limit not as extraction but as a coordination mechanism that enables the crisis solution without threatening republican collapse. Net beneficiary in immediate term (absolute power), but the constraint structures the benefit as temporary and conditioned on restoration. Low experienced extraction because the dictator accepts the constitutionalized role. (Note: This classification assumes the dictator intends to restore power; Sulla and Caesar perspectives would differ.)
constraint_indexing:constraint_classification(dictatorship_term_limited__constitutional_emergency_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE LEGAL ORDER / SURVIVAL (SCAFFOLD) — From the perspective of constitutional continuity, the dictatorship is a temporary coordination mechanism with an explicit sunset. The legal order benefits from the crisis resolution (unified command averts collapse) and from the structural guarantee that the exception will end (restoration of separated powers). This is pure scaffold logic: temporary support structure, low effective extraction (because the dictatorship is designed to dissolve), coordinated exit via constitutional restoration. Theater ratio is moderate because the restoration ritual matters — public affirmation that the exception has ended and ordinary law resumes.
constraint_indexing:constraint_classification(dictatorship_term_limited__constitutional_emergency_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE HISTORICAL DICTATORSHIP INSTITUTION (PITON) — From a civilizational view spanning Rome's dictatorship to modern emergency law, the institution appears degraded: actual dictators rarely voluntarily restore power (Sulla, Caesar, Napoleon, modern emergency-clause abuse). The theater of term-limitedness persists in constitutional text despite the empirical record of violation. The institution is maintained through ritualistic invocation (emergency declared, dictator appointed, restored, emergency lifted) even though the functional mechanism (voluntary restoration) has atrophied. The form remains; the substance is mostly aspirational. Piton classification reflects high theater ratio (the ritual matters) and low functional power of the time-bound constraint (history shows it fails to prevent indefinite extension).
constraint_indexing:constraint_classification(dictatorship_term_limited__constitutional_emergency_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective grounded in Hobbesian sovereign theory, the emergency dictatorship instantiates a natural law: sovereign power cannot be constitutionally bound because constitutionality depends on the sovereign's will. The time limit is a performative gesture — it expresses the wish that emergency power will be temporary but cannot logically guarantee it, because the sovereign who imposed the limit can also suspend or revise it. This perspective sees the constraint as immutable: the paradox of legal suspension cannot be resolved through constitutional drafting. However, structural data contradicts this — beneficiaries (the legal order) and victims (citizens during exception) are identifiable, and the enforceability (suppression of alternatives) is contingent on the republican authority structure, not immutable law.
constraint_indexing:constraint_classification(dictatorship_term_limited__constitutional_emergency_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dictatorship_term_limited__constitutional_emergency_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dictatorship_term_limited__constitutional_emergency_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dictatorship_term_limited__constitutional_emergency_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dictatorship_term_limited__constitutional_emergency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dictatorship_term_limited__constitutional_emergency_reading, TR),
    TR >= 0.70.

:- end_tests(dictatorship_term_limited__constitutional_emergency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The dictatorship is granted absolute power during the emergency term, and this is structurally extractive — citizens' rights are suspended, alternatives are suppressed, and the dictator can act without institutional check. However, extractiveness is moderated (not maximal) by the constitutional sunset: the time bound is a real structural feature (not merely rhetorical) in the reading's own logic, and the promise of restoration is the compensating coordination function. The modest rise in extractiveness over the interval (0.42 → 0.56) reflects the accumulated exercise of power and the increasing awareness that the restoration deadline is approaching. Suppression (0.65): High but not maximum. The constitution suppresses alternative emergency doctrines (requiring that emergency power take the dictatorship form, not others) and suppresses ordinary rights claims during the term. But suppression is not total because the time bound and restoration mechanism provide a constitutionally guaranteed exit — citizens know the emergency will end. Compare this to pure state-of-exception doctrine (Schmitt), where suppression would approach 0.85+. Theater ratio (0.58): Moderate. The ritual of emergency declaration, dictatorship appointment, and restoration is functionally important (the public ceremony that marks the transition in and out of emergency) but increasingly theatrical as the emergency persists (the ceremony is repeated, formalized, potentially decoupled from actual crisis conditions). The rise in theater over the interval reflects this drift toward ritual.
 *
 * PERSPECTIVAL GAP:
 *   Each perspective reveals a different structural reading of the same constitutional mechanism. The Cincinnatus dictator sees pure coordination (rope) — absolute power to solve the crisis, promised restoration creating mutual benefit. The republican institutions see tangled rope — they gain crisis-solving capacity but lose all power during the term. The citizen sees snare — suspended rights with no recourse except the dictator's promise. The legal order (as abstract entity) sees scaffold — temporary support structure with guaranteed sunset. The historical dictatorship institution sees piton — the form (term-limited dictatorship) persists despite the empirical failure of the mechanism (dictators often refuse to restore). The analytical observer risks seeing a natural law (mountain) — the paradox of legal suspension is unresolvable — but the structural data contradicts this: the constraint is enforced by identifiable beneficiaries and victims, and its suppression is contingent on the constitutional amendment that codifies it. The perspectival gap reveals that this reading's confidence in the constitutional domestication of emergency power rests on an assumption (voluntary restoration) that is empirically fragile.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary-victim structure determines directionality. The republican authority structure (beneficiary) receives crisis-solving capacity; citizens (victims) receive suspended rights. The dictator's position is ambiguous — beneficiary if intending restoration (arbitrage exit), potential predator if refusing to restore (mobile or trapped power). The constraint's suppression of alternative emergency doctrines means that agents attempting to invoke other mechanisms face constitutionally foreclosed options. The derivation chain treats the republican institutions as institutional beneficiaries with arbitrage (they can restore ordinary governance if the crisis resolves), producing low d and negative effective extraction from their perspective. Citizens under emergency are powerless victims with trapped exit, producing high d and high experienced extraction. The time bound moderates the extraction relative to extraconstitutional emergency power because the mathematical formula (χ = ε × f(d) × σ(S)) applies the biographical time horizon (the citizen expects the emergency to resolve within their lifetime) rather than civilizational time (indefinite exception). This difference in time horizon between the beneficiary (institutional/immediate) and victim (powerless/biographical) is a key structural feature of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through the indexical tuple. At the republican institutional level (beneficiary perspective), the constraint is rope (pure coordination). At the citizen level (victim perspective), it is snare (pure extraction). At the legal order level (survival perspective), it is scaffold (temporary support). At the historical level, it is piton (degraded ritual). The constraint is not one type; it is a presheaf of types determined by the observer's structural position. The constraint's extractiveness of 0.52 is the average experienced across the institutional beneficiary (low extraction) and the trapped citizen (high extraction). The analytical observer's mountain classification is rejected as a false summit — the constraint's suppression of alternatives and its extraction from suspended citizens are contingent structural features, not immutable laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_restoration_assumption,
    'Does the constitutional dictatorship genuinely constrain absolute power, or does it merely express a hope that the dictator will voluntarily restore it?',
    'Historical analysis of constitutional dictatorships (Roman, modern emergency law): rate of voluntary restoration vs. indefinite extension; examination of enforcement mechanisms for the time limit (who has power to remove the dictator if they refuse to step down?).',
    'If voluntary restoration holds: constraint is Tangled Rope (real coordination, moderated extraction). If voluntary restoration fails: constraint is Snare (extraction mechanism masked as temporary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_restoration_assumption, empirical, 'Whether constitutionalized time limits are actually enforced or merely expressed as aspiration').

omega_variable(
    alternative_emergency_doctrines,
    'What alternative doctrines of emergency power (Schmittian state-of-exception, parliamentary emergency, dispersed emergency authority) does the constitutionalized dictatorship suppress, and why?',
    'Comparative constitutional law: analysis of emergency powers clauses across legal systems; identification of doctrinal competitors (pure Schmitt, distributed emergency authority, sunset language without personified dictatorship); assessment of which alternatives were deliberately foreclosed by the constitutional dictatorship framework.',
    'If suppression is structural (the dictatorship is the only logically coherent emergency mechanism): constraint is a mountain. If suppression is contingent (alternatives exist but were chosen against): constraint is a tangled rope with higher victims visibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_emergency_doctrines, conceptual, 'What emergency doctrines are foreclosed or suppressed by the dictatorship reading').

omega_variable(
    this_reading_vs_caesarism,
    'Is the dictatorship reading of constitutionalized emergency genuinely distinct from the precedent-for-caesarism reading, or does the constitutional term limit merely formalize the rehearsal that every Cincinnatus normalized?',
    'Doctrinal analysis: does the term-limit reading deny that historical dictatorships established precedent for indefinite extension, or does it bracket that precedent as a failure mode to be prevented? Does the reading logically foreclose the caesarism reading, or merely express confidence that its own mechanisms will work when the caesarism reading''s history suggests they won''t?',
    'If this reading forecloses caesarism: the two readings cannot coexist in a single legal framework, and the constitution must choose. If this reading coexists with caesarism: the constitution embodies both readings simultaneously, and actual dictatorship will reveal which reading prevails.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(this_reading_vs_caesarism, conceptual, 'Whether term-limited constitutionalization forecloses or merely hopes to prevent caesarist precedent').

omega_variable(
    suspension_paradox_coherence,
    'Does this reading''s claim that emergency power is constitutionalized (brought inside the law through naming and time-bounding) answer the suspension paradox, or does the paradox persist: an office inside the law whose content is the law''s absence?',
    'Philosophical analysis: does naming the exception and bounding its time resolve Agamben''s paradox, or do these moves merely obscure the paradox''s persistence? Is the ''return of commission'' a logical resolution or a performative gesture that restates the paradox as ritual?',
    'If the paradox is resolved: this reading genuinely distinguishes the constitutionalized dictatorship from the pure paradox reading. If the paradox persists: all three readings describe the same structural impossibility from different framings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_paradox_coherence, conceptual, 'Whether constitutionalizing emergency actually resolves the suspension paradox').

omega_variable(
    extraction_domestication_claim,
    'What evidence would confirm that the constitutionalized dictatorship ''domesticates'' the extractiveness of exception, as claimed in the expected structural delta?',
    'Measurement of extractiveness during actual constitutional dictatorships vs. extraconstitutional emergency power: compare rights violations, arbitrary detention, economic extraction, duration of emergency, degree of institutional subordination. If extractiveness is lower under constitutional dictatorship than under unregulated emergency power, domestication claim holds.',
    'If domestication is real: extractiveness should be significantly lower (~0.35-0.40) than in raw emergency power scenarios. If domestication is illusory: extractiveness does not decrease materially, and the constitution provides rhetorical cover rather than actual constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_domestication_claim, empirical, 'Whether constitutional term-limiting actually reduces extraction relative to extraconstitutional emergency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dictatorship_term_limited__constitutional_emergency_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dict_emerg_tr_t0, dictatorship_term_limited__constitutional_emergency_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dict_emerg_tr_t5, dictatorship_term_limited__constitutional_emergency_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(dict_emerg_tr_t10, dictatorship_term_limited__constitutional_emergency_reading, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(dict_emerg_be_t0, dictatorship_term_limited__constitutional_emergency_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dict_emerg_be_t5, dictatorship_term_limited__constitutional_emergency_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(dict_emerg_be_t10, dictatorship_term_limited__constitutional_emergency_reading, base_extractiveness, 10, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(dict_emerg_su_t0, dictatorship_term_limited__constitutional_emergency_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dict_emerg_su_t5, dictatorship_term_limited__constitutional_emergency_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(dict_emerg_su_t10, dictatorship_term_limited__constitutional_emergency_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dictatorship_term_limited__constitutional_emergency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dictatorship_term_limited__constitutional_emergency_reading, dictatorship_precedent_for_caesarism_reading).
narrative_ontology:affects_constraint(dictatorship_term_limited__constitutional_emergency_reading, dictatorship_suspension_paradox_reading).

% DUAL FORMULATION NOTE:
% The dictatorship_term_limited kernel decomposes into three constraint stories corresponding to three readings. This story instantiates the constitutional_emergency_reading. Sibling stories represent the precedent_for_caesarism_reading and the suspension_paradox_reading. Each reading has its own extractiveness value, beneficiary/victim structure, and perspectives. The network links show that the three readings contend over the same kernel (the constitutional dictatorship) and that structural features of one reading create pressure on the others: the constitutional reading's success (widespread adoption of term-limited emergency clauses) influences but does not foreclose the caesarism reading (historical precedent remains structurally available); the constitutional reading and paradox reading coexist rather than foreclose (both see the same mechanism; they differ on whether the paradox is resolved).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dictatorship_term_limited__constitutional_emergency_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
