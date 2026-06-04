% ============================================================================
% CONSTRAINT STORY: dictatorship_term_limited__precedent_for_caesarism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dictatorship_term_limited__precedent_for_caesarism_reading, []).

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
 *   constraint_id: dictatorship_term_limited__precedent_for_caesarism_reading
 *   human_readable: Dictatorship as Rehearsal for Caesarism: The Precedent Compounding Reading
 *   domain: legal/constitutional/political_doctrine
 *
 * SUMMARY:
 *   The dictatorship of the Roman Republic presents a paradox: an office
 *   constitutionally bounded and temporarily granted that, through a series
 *   of well-behaved incumbents, accumulated precedent enabling its eventual
 *   perpetuation. This constraint is ONE READING of the contested kernel of
 *   'term-limited dictatorship.' The reading instantiated here is the
 *   precedent-for-caesarism interpretation: each Cincinnatus-like dictator
 *   who surrendered power on schedule normalized the constitutional form,
 *   stockpiling legitimacy that a future dictator could exploit. Sulla
 *   extended the dictatorship's scope; Caesar made it perpetual. The
 *   extraction mechanism is diachronic — invisible in any single use but
 *   compounding across the series. Suppression is deferred, not avoided: the
 *   Senate and people repeatedly assent to each temporary grant, unaware that
 *   each assent makes refusal of the next grant more difficult. The
 *   beneficiary is the eventual perpetual dictator, who simply executes the
 *   form that centuries of good precedent legitimated. The victim is the
 *   Republic itself, understood as the principle of rule by many with
 *   constrained executive authority.
 *
 * KEY AGENTS:
 *   - The Cincinnatus Dictators (mythic and historical): Institutional beneficiaries (arbitrage/immediate) — execute emergency powers, surrender on schedule, experience the constraint as coordination
 *   - The Senate and Magistracy: Organized institutional actors (constrained/biographical) — grant dictatorship, believe they control its scope and duration, benefit from crisis resolution, gradually lose power to refusal
 *   - Sulla: Institutional actor (powerful/biographical) — extends dictatorship beyond earlier precedents, still surrenders but normalizes a broader form
 *   - Caesar: Institutional actor (powerful/biographical) — breaks the pattern, makes dictatorship perpetual, exploits the accumulated precedent
 *   - The Citizen Body: Powerless actors (trapped/generational) — cannot exit the precedent structure, see each dictator appear and disappear, unaware of the accumulation
 *   - The Republican Principle: Abstract victim (powerless/civilizational) — norm of res publica eroded by each dictator, whether well-behaved or not
 *   - The Analytical Observer: Civilizational witness (analytical/analytical) — sees the diachronic accumulation only when the full sequence is examined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dictatorship_term_limited__precedent_for_caesarism_reading, 0.68).
domain_priors:suppression_score(dictatorship_term_limited__precedent_for_caesarism_reading, 0.72).
domain_priors:theater_ratio(dictatorship_term_limited__precedent_for_caesarism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dictatorship_term_limited__precedent_for_caesarism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dictatorship_term_limited__precedent_for_caesarism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dictatorship_term_limited__precedent_for_caesarism_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dictatorship_term_limited__precedent_for_caesarism_reading, tangled_rope).
narrative_ontology:human_readable(dictatorship_term_limited__precedent_for_caesarism_reading, "Dictatorship as Rehearsal for Caesarism: The Precedent Compounding Reading").
narrative_ontology:topic_domain(dictatorship_term_limited__precedent_for_caesarism_reading, "legal/constitutional/political_doctrine").

domain_priors:requires_active_enforcement(dictatorship_term_limited__precedent_for_caesarism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dictatorship_term_limited__precedent_for_caesarism_reading, '6e85c02e-b82b-4d9c-9f17-561179ce99c8').
narrative_ontology:cs_kernel_codification('6e85c02e-b82b-4d9c-9f17-561179ce99c8', formalized).
narrative_ontology:cs_authority_grounding('6e85c02e-b82b-4d9c-9f17-561179ce99c8', extraction).
narrative_ontology:cs_interpretation_layer_present('6e85c02e-b82b-4d9c-9f17-561179ce99c8').
narrative_ontology:cs_reading_relation('6e85c02e-b82b-4d9c-9f17-561179ce99c8', dictatorship_term_limited__constitutional_emergency_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e85c02e-b82b-4d9c-9f17-561179ce99c8', dictatorship_term_limited__suspension_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('6e85c02e-b82b-4d9c-9f17-561179ce99c8', foundational, precedent_accumulation_enables_perpetuation).
narrative_ontology:cs_axiom_status(precedent_accumulation_enables_perpetuation, holdable).
narrative_ontology:cs_axiom_grounding('6e85c02e-b82b-4d9c-9f17-561179ce99c8', precedent_accumulation_enables_perpetuation, empirically_contingent).
narrative_ontology:cs_axiom('6e85c02e-b82b-4d9c-9f17-561179ce99c8', foundational, diachronic_trap_invisibility).
narrative_ontology:cs_axiom_status(diachronic_trap_invisibility, holdable).
narrative_ontology:cs_axiom_grounding('6e85c02e-b82b-4d9c-9f17-561179ce99c8', diachronic_trap_invisibility, deontological).
narrative_ontology:cs_reference_frame('6e85c02e-b82b-4d9c-9f17-561179ce99c8', bounded_emergency_executive).
narrative_ontology:cs_drift_state('6e85c02e-b82b-4d9c-9f17-561179ce99c8', perpetual_caesarism, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('6e85c02e-b82b-4d9c-9f17-561179ce99c8', '').
narrative_ontology:cs_kernel_id(dictatorship_term_limited__precedent_for_caesarism_reading, dictatorship_term_limited).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dictatorship_term_limited__precedent_for_caesarism_reading, eventual_perpetual_dictator).
narrative_ontology:constraint_victim(dictatorship_term_limited__precedent_for_caesarism_reading, republican_institutions_across_time).
narrative_ontology:constraint_victim(dictatorship_term_limited__precedent_for_caesarism_reading, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZEN BODY (SNARE) — Trapped across generations. Each temporary dictatorship appears constitutional (bounded, emergency-justified, ultimately surrendered); each surrender normalizes the form for the next dictator. Citizens cannot exit the accumulated precedent structure. Suppression rises as each 'good' use — Cincinnatus-like restraint — stockpiles legitimacy that a 'bad' dictator will exploit. Maximum experienced extraction because the trap is diachronic (working across time horizons), not visible synchronically.
constraint_indexing:constraint_classification(dictatorship_term_limited__precedent_for_caesarism_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE TEMPORARY DICTATOR (GOOD FAITH) (ROPE) — Each temporary dictator appears to experience genuine coordination: emergency powers solve the immediate problem, are surrendered on schedule, and the Republic is restored. The constraint looks like coordination from this perspective — solving collective action problems during crisis. But this perspective is temporally myopic: the good dictator does not see (or chooses not to see) how their restraint stockpiles legitimacy for their successor.
constraint_indexing:constraint_classification(dictatorship_term_limited__precedent_for_caesarism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SENATE AND MAGISTRACY (TANGLED ROPE) — Constrained by the emergency's genuine reality but also benefiting from the institution of dictatorship as a safety valve. The Senate retains agency to grant and revoke the office; they believe they can control the duration and scope. But they are locked into the escalating precedent: each successful temporary dictatorship makes it harder to refuse the next one. They benefit from the coordination function (crisis resolution) but are slowly victimized by the extraction function (power consolidation). The mixed classification reflects this duality.
constraint_indexing:constraint_classification(dictatorship_term_limited__precedent_for_caesarism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FUTURE PERPETUAL DICTATOR (ANTICIPATORY) (ROPE) — From the perspective of the eventual perpetual dictator (the one who breaks the pattern), the institution appears as pure coordination: a legitimated form for concentrating power during crisis, the precedent for it accepted by the Senate and people, the scope and duration escalating with each use. Each 'good' predecessor normalized it further. The dictator who makes it perpetual does not see themselves as extracting — they are merely completing what the institution logically contains. Arbitrage: they escape the return-commission while predecessors did not.
constraint_indexing:constraint_classification(dictatorship_term_limited__precedent_for_caesarism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE REPUBLICAN PRINCIPLE (CIVILIZATIONAL/HISTORICAL) (SNARE) — The abstract commitment to res publica (the public thing, rule by the many, term limits on power) is itself the victim. Each temporary dictatorship, no matter how well-behaved, erosodes this principle by normalizing its negation. The principle is powerless because it is abstract — it has no agent to enforce it, no exit option except to be abandoned entirely. By the time the principle's advocates recognize the trap, the precedent chain is too heavy to break.
constraint_indexing:constraint_classification(dictatorship_term_limited__precedent_for_caesarism_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the universal historical perspective, the dictatorship constraint combines genuine coordination (emergency powers solve real crises) with compounding extraction (precedent accumulates across dictators). The extraction is not visible in any single use — it is only visible when the series is examined diachronically. Each temporary dictator believes they are coordinating; only the analyst sees the accumulation of precedent that makes perpetuation inevitable. The constraint is Tangled Rope because the coordination function is real AND the extraction function is real; they are woven together.
constraint_indexing:constraint_classification(dictatorship_term_limited__precedent_for_caesarism_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dictatorship_term_limited__precedent_for_caesarism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dictatorship_term_limited__precedent_for_caesarism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dictatorship_term_limited__precedent_for_caesarism_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dictatorship_term_limited__precedent_for_caesarism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dictatorship_term_limited__precedent_for_caesarism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, compounding. The initial extractiveness of a Cincinnatus-like dictator is low (0.15) — the office appears to operate as emergency coordination, not extraction. But extractiveness compounds across the series as precedent accumulates. By the fourth iteration (Caesar), base extractiveness reaches 0.68. This is the signature of the reading: extraction is not a property of any single dictator but of the accumulated chain. Suppression (0.72): High and rising. Early dictatorships appear to have low suppression — the Senate and people voluntarily grant the office, believing it temporary. But suppression accumulates as refusal becomes politically costly (Senate cannot appear to obstruct emergency response) and as each precedent makes the form more normal. By the perpetuation point, suppression is near-total: the Senate cannot refuse without appearing to violate the Republic's own precedent. Theater ratio (0.55): Moderate. The dictatorship has a genuine coordination function (emergencies are real) and a genuine extraction function (precedent accumulation is real). The theater is neither minimal (it is not pure coordination) nor maximal (it is not a degraded ritual). The mixture of genuine coordination with hidden extraction is characteristic of Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps are extreme and reveal the reading's logic. From the Cincinnatus dictator's perspective, the constraint is Rope — genuine coordination with bounded cost. From the Senate's perspective, it is Tangled Rope — coordination with rising extraction across time. From the citizen body's perspective, it is Snare — no escape from accumulated precedent. From the future perpetual dictator's perspective, it is Rope — they simply inherit a legitimate form. From the republican principle's perspective, it is Snare — the principle is eroded by each dictator, powerless to prevent it. Only the analytical observer sees the Tangled Rope classification: genuine coordination woven with compounding extraction, visible only diachronically. The gaps reveal that this is not a constraint whose nature can be understood from the perspective of any single participant or even any single time point — it is only intelligible across the series.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options, varying across perspectives. The Cincinnatus dictator has d ≈ 0.15 (institutional/arbitrage beneficiary: low extraction experienced). The Senate has d ≈ 0.50 (organized/constrained: mixed, benefits from safety valve but loses agency to precedent). The citizen body has d ≈ 0.95 (powerless/trapped: maximum extraction, no exit). The future perpetual dictator anticipates d ≈ 0.05 (will arbitrage the form, experiencing it as pure coordination). The analytical observer has d ≈ 0.72 (analytical/analytical: sees the structure from outside any participant's frame). The formula χ = ε × f(d) × σ(S) applies at each perspective, producing the observed variance in experienced extractiveness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cincinnatus_authenticity,
    'Were the well-behaved dictators (Cincinnatus paradigm) genuinely surrendering power from principled restraint, or strategically deferring extraction to consolidate precedent for future use?',
    'Biographical and contextual analysis of each dictator''s post-office career, statements, and actions; comparison with perpetual dictators'' pre-perpetuation behavior patterns',
    'If genuine restraint: the extraction function emerges only across the series, not within individual choices. If strategic deferral: extraction is visible in the individual dictator''s calculus. The reading''s coherence depends on treating good dictators as authentically restrained; if they are strategists, the chain becomes a conscious conspiracy rather than an accumulated precedent trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cincinnatus_authenticity, empirical, 'Whether Cincinnatus-model restraint was authentic or strategic deferral').

omega_variable(
    legitimacy_stockpiling_mechanism,
    'What is the precise mechanism by which a single dictator''s restraint creates legitimacy that a future dictator can exploit?',
    'Analysis of constitutional precedent doctrine, Senate deliberation records, and public opinion shifts across successive dictatorships; identification of the specific precedent-citation chains that justify escalation',
    'If mechanism is formal legal precedent (judges citing earlier dictators): the extraction is procedurally visible and contestable. If mechanism is normalization (cultural adaptation to the form): extraction is diffuse and harder to challenge. If mechanism is Senate institutional capture (senators benefit from the safety valve and rationalize it): extraction is hidden in structural incentives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_stockpiling_mechanism, empirical, 'The mechanism of precedent accumulation across dictators').

omega_variable(
    alternative_emergency_institutionalization,
    'Could the Republic have constitutionalized emergency powers in a form that prevented escalation (e.g., truly term-bounded, with non-renewable office, power transfer to rotating magistrates)?',
    'Comparative historical analysis of other republics'' emergency mechanisms; structural modeling of constraints that would break the escalation chain',
    'If viable alternatives existed: the perpetuation is a choice within the dictatorship institution, not an inevitable consequence. The reading shifts: extraction is chosen by dictators and enabled by Senate, not compelled by precedent logic. If no viable alternative: the dictatorship form itself contains the seed of perpetuation, and restraint is irrelevant — the form will inevitably be abused.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_emergency_institutionalization, conceptual, 'Whether alternative emergency institutions could have prevented dictatorship escalation').

omega_variable(
    contested_reading_boundary,
    'At what point in the historical sequence does this reading''s account cease to describe accumulated precedent and become predictive of perpetuation?',
    'Identification of the specific historical moment when the precedent chain becomes sufficiently heavy that perpetuation becomes structurally likely rather than merely possible',
    'This omega routes the committer problem: the reading describes a historical phenomenon (Cincinnatus followed by Sulla, then Caesar) OR makes a structural claim (dictatorship logically contains perpetuation). The boundary determines which reading the engine should match to in a contested interpretation context.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_reading_boundary, conceptual, 'Boundary between historical account and structural claim about dictatorship logic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dictatorship_term_limited__precedent_for_caesarism_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(dict_prec_be_t0, dictatorship_term_limited__precedent_for_caesarism_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(dict_prec_be_t1, dictatorship_term_limited__precedent_for_caesarism_reading, base_extractiveness, 1, 0.22).
narrative_ontology:measurement(dict_prec_be_t2, dictatorship_term_limited__precedent_for_caesarism_reading, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(dict_prec_be_t3, dictatorship_term_limited__precedent_for_caesarism_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(dict_prec_be_t4, dictatorship_term_limited__precedent_for_caesarism_reading, base_extractiveness, 4, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dict_prec_su_t0, dictatorship_term_limited__precedent_for_caesarism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(dict_prec_su_t1, dictatorship_term_limited__precedent_for_caesarism_reading, suppression_requirement, 1, 0.45).
narrative_ontology:measurement(dict_prec_su_t2, dictatorship_term_limited__precedent_for_caesarism_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(dict_prec_su_t3, dictatorship_term_limited__precedent_for_caesarism_reading, suppression_requirement, 3, 0.7).
narrative_ontology:measurement(dict_prec_su_t4, dictatorship_term_limited__precedent_for_caesarism_reading, suppression_requirement, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dictatorship_term_limited__precedent_for_caesarism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dictatorship_term_limited__precedent_for_caesarism_reading, dictatorship_term_limited__constitutional_emergency_reading).
narrative_ontology:affects_constraint(dictatorship_term_limited__precedent_for_caesarism_reading, dictatorship_term_limited__suspension_paradox_reading).

% DUAL FORMULATION NOTE:
% The dictatorship_term_limited kernel has three structurally distinct readings, each producing a different constraint story with different ε values and type classifications. The precedent_for_caesarism_reading (this file) instantiates extraction-via-precedent-accumulation (ε=0.68, Tangled Rope diachronically, Rope synchronically per individual dictator). The constitutional_emergency_reading instantiates successful bounding of emergency (lower ε, Rope or Scaffold, synchronized across time). The suspension_paradox_reading instantiates logical paradox of legal suspension (ε varies, possibly Mountain from the philosophical perspective). All three readings share the same kernel (the office of dictatorship) but interpret its significance and mechanism differently. They coexist as live competing interpretations held by different doctrinal communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dictatorship_term_limited__precedent_for_caesarism_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
