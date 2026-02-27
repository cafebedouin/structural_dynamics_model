% ============================================================================
% CONSTRAINT STORY: ukr_mobilization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ukr_mobilization, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ukr_mobilization
 *   human_readable: Ukrainian Mobilization Law and Conscription Enforcement
 *   domain: political/military/labor
 *
 * SUMMARY:
 *   Ukraine's mobilization law (enacted February 2022, expanded through 2024)
 *   mandates military conscription for all able-bodied males aged 18-60, with
 *   criminal penalties for refusal and a de facto border prohibition on male
 *   emigration. The constraint operates as a snare: the Ukrainian state
 *   extracts military labor from its citizens through coercive enforcement
 *   backed by the sovereign monopoly on legitimate violence. The state
 *   justifies the constraint through existential threat (Russian invasion),
 *   which is structurally real but creates a temporal ambiguity: is this a
 *   permanent snare or a time-bounded scaffold? The constraint exhibits low
 *   theater (0.38) because the coercion is direct and transparent — the state
 *   clearly enforces the law through visible mechanisms (border patrols,
 *   military police, prosecution). This transparency paradoxically increases
 *   suppression (0.72) by eliminating plausible deniability. The constraint's
 *   extractiveness (0.68) reflects the state's nearly complete appropriation
 *   of conscripted labor without compensation beyond survival benefits. From
 *   the conscripted soldier's perspective, the constraint is pure extraction:
 *   no exit, no agency, no alternative. From NATO's perspective, the
 *   constraint is coordination: a shared defense mechanism that benefits all
 *   parties. From the analytical observer's view, the constraint is tangled
 *   rope: it solves a genuine coordination problem (collective defense)
 *   through an extractive mechanism (coercive conscription). The unresolved
 *   mandatrophy concerns whether wartime exigency justifies permanent snare
 *   classification or whether the constraint should be scaffolded with a
 *   sunset clause.
 *
 * KEY AGENTS:
 *   - Ukrainian Male Citizens (18-60): Primary victims (powerless/trapped) — no legal exit from conscription; border prohibition prevents emigration; prosecution for refusal
 *   - Ukrainian State Military: Primary beneficiary (organized/constrained) — obtains mobilized forces; also constrained by enforcement burden and international law pressure
 *   - NATO Alliance: Secondary beneficiary (institutional/arbitrage) — benefits from Ukrainian military capability; abundant exit options; subsidized security
 *   - Border Enforcement Apparatus: Enforcement agent (organized/constrained) — implements conscription law; maintains border closure for males; operates under state authority
 *   - Civilian Labor Markets: Structural victim (moderate/constrained) — loses productive population; economic output declines; reconstruction capacity impaired
 *   - International Human Rights Framework: Organized observer (organized/mobile) — documents coercion; attempts accountability; limited enforcement leverage against warring state
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes both genuine coordination function and extractive mechanism; notes temporal ambiguity on sustainability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ukr_mobilization, 0.68).
domain_priors:suppression_score(ukr_mobilization, 0.72).
domain_priors:theater_ratio(ukr_mobilization, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ukr_mobilization, extractiveness, 0.68).
narrative_ontology:constraint_metric(ukr_mobilization, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ukr_mobilization, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ukr_mobilization, snare).
narrative_ontology:human_readable(ukr_mobilization, "Ukrainian Mobilization Law and Conscription Enforcement").
narrative_ontology:topic_domain(ukr_mobilization, "political/military/labor").

domain_priors:requires_active_enforcement(ukr_mobilization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ukr_mobilization, ukrainian_state_military_apparatus).
narrative_ontology:constraint_beneficiary(ukr_mobilization, western_military_coalition_partners).
narrative_ontology:constraint_victim(ukr_mobilization, conscriptable_male_citizens).
narrative_ontology:constraint_victim(ukr_mobilization, border_crossing_refugees).
narrative_ontology:constraint_victim(ukr_mobilization, economic_development_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED MALE CITIZEN (SNARE) — No legal exit from mobilization without criminal penalties or flight. Military service is compulsory; refusal results in prosecution, asset seizure, or international warrant status if fleeing. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.95. Extraction is total and enforced through state monopoly on legitimate violence.
constraint_indexing:constraint_classification(ukr_mobilization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BORDER-TRAPPED REFUGEE (SNARE) — Males aged 18-60 are prohibited from leaving the country. Border guards (acting as enforcement agents of the state) physically prevent departure. Exit is illegal; trapped within the mobilization apparatus. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.95. Suppression is maximal and transparent.
constraint_indexing:constraint_classification(ukr_mobilization, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: UKRAINIAN MILITARY COMMAND (TANGLED ROPE) — Benefits from mobilization (expansion of forces, resource allocation, operational capability). Also constrained by enforcement requirements (maintaining morale, managing desertion, coordinating with NATO). Coordination function: mobilization supplies trained soldiers. Extraction function: conscription transfers private labor to state control. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.20. Low effective extraction because the military sees itself as both organizer and constrained actor.
constraint_indexing:constraint_classification(ukr_mobilization, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NATO AND WESTERN ALLIANCE (ROPE) — Benefits from Ukrainian military capability and deterrent against Russian expansion. Experiences Ukrainian mobilization as coordination: shared defense against a common threat. Exit options abundant (support can shift). d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.07. Negative effective extraction from their perspective — the mobilization subsidizes their security without requiring their own conscription.
constraint_indexing:constraint_classification(ukr_mobilization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: UKRAINE'S ECONOMIC DEVELOPMENT (SNARE) — Mobilization removes productive males from civilian labor markets (agriculture, manufacturing, services, reconstruction). Economic output declines; gender-imbalanced labor force impairs long-term growth. No exit from this structural impact. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.76. Extraction is indirect but severe: the constraint transfers human capital from productive to destructive use.
constraint_indexing:constraint_classification(ukr_mobilization, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL HUMAN RIGHTS FRAMEWORK (SCAFFOLD) — Organizations (UN, ICC, Human Rights Watch) document mobilization coercion and enforce accountability mechanisms. Theater of enforcement (investigations, indictments) is low — structural constraints on compliance are high (state sovereignty, wartime exception claims). Sunset clause: mobilization is justified only for duration of existential threat. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.49. Moderate effective extraction because the framework has organizational capacity but limited enforcement leverage.
constraint_indexing:constraint_classification(ukr_mobilization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Observes both genuine coordination function (mobilization enables collective defense against invasion) AND extractive mechanism (state transfers private labor to military without consent or compensation beyond survival). The observer notes that the constraint's justification (existential threat) is real and structural — not theater. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.94. High effective extraction even from analytical view because suppression (0.72) is so high that no perspective experiences meaningful alternatives.
constraint_indexing:constraint_classification(ukr_mobilization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ukr_mobilization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ukr_mobilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ukr_mobilization, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ukr_mobilization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ukr_mobilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.68): The state extracts essentially all productive labor from conscripted males without meaningful compensation beyond survival. The extraction is nearly complete. The value is high (0.68 vs. maximum 1.0) rather than maximum because some soldiers receive combat bonuses, family support, or social status that partially offsets the coercion. Over the interval, extractiveness increased from 0.45 (initial mobilization with some volunteer component) to 0.68 (full coercive enforcement with border closure), reflecting the state's progressive hardening of mobilization mechanisms. Suppression (0.72): Suppression is high but not maximal. Legal alternatives (conscientious objector status, medical exemptions, contract buyouts) exist on paper but are rarely granted or are prohibitively expensive. Border enforcement is strict for males but permeable for females, international advocates, and the wealthy. Some groups (medical personnel, essential workers) receive deferrals. The 0.72 value reflects substantial but not total suppression of exit options. Theater Ratio (0.38): Low theater. The mobilization law is enforced through direct, transparent mechanisms: border patrols deny male departure; military conscription offices issue draft notices; courts prosecute refusers. There is minimal performative activity — the state does not stage elaborate legitimacy theater. The low theater reflects that the constraint's justification (existential military threat) is widely accepted, reducing the need for narrative legitimation work. Theater has slightly increased (0.28 → 0.38) as the conflict duration stretches the 'existential threat' narrative and the state requires increasing rhetorical effort to maintain mobilization commitment.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a stark perspectival gap between the conscripted victim (snare with d≈0.92, f(d)≈1.40, χ≈0.95) and the beneficiary institutional actors (rope or tangled rope with d≈0.08-0.35, f(d)≈-0.08-0.30, χ≈-0.07-0.20). The conscripted soldier experiences total extraction: no exit, no choice, no alternative future. The military command experiences coordination: mobilization solves the collective action problem of military readiness and provides operational resources. NATO experiences subsidized security: Ukrainian conscription defends NATO's eastern flank without NATO bearing the conscription cost. The victim and beneficiary inhabit fundamentally different constraint universes — this is not a perspectival difference in degree but in kind. The analytical observer sees both truths simultaneously: the snare is real (coercive, extractive, backed by violence) AND the rope is real (genuine coordination function, shared existential threat, mutual defense benefit). The mandatrophy emerges here: can a mechanism be both a genuine response to existential threat AND a snare? The framework resolves this by noting that snare and rope are not opposite but intersecting: a tangled rope is snare + rope. The Ukrainian mobilization is a tangled rope observed by military strategists and a pure snare observed by conscripts. The analytical perspective reveals both.
 *
 * DIRECTIONALITY LOGIC:
 *   Conscripted male citizens: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum structural extraction. Cannot exit legally (conscription is mandatory); cannot exit practically (border closure for males); refusal is criminalized. No alternatives present. Border-trapped refugees: Victim + trapped → d≈0.95, f(d)≈1.42. Slightly higher than conscripted soldiers because the border mechanism itself is purely coercive (no legal exemptions). Military command: Beneficiary + constrained → d≈0.35, f(d)≈0.30. Moderate benefit (mobilization supplies forces) but constrained by enforcement burden, international pressure, and need to maintain morale. Not fully arbitrage because the state is also bound by its own mobilization commitment. NATO alliance: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net subsidy receiver. Can exit (stop supporting Ukraine, shift alliance focus) but chooses not to. Negative effective extraction from their view. Civilian labor market: Victim + constrained → d≈0.78, f(d)≈1.12. Structural victim (loses labor force) but has some constrained options (female labor participation, automation, foreign workers). Not fully trapped. International human rights framework: Observer + mobile → d≈0.50, f(d)≈0.65. Symmetric position — can document and pressure but has limited enforcement leverage against a sovereign state in existential conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the Snare vs. Coordination ambiguity by classifying it as tangled rope at the analytical level while acknowledging that specific agents perceive it as pure snare (victims) or coordination (beneficiaries). The mandatrophy is: Can you conscript your population to save your state from invasion? The framework resolves this by noting that the answer depends on the temporal boundary. If 'existential threat' is permanent, the constraint is a permanent snare — conscription becomes the normal state, extraction becomes normalized, and the constraint likely persists as a piton after the war ends. If 'existential threat' is time-bounded and demobilization will occur, the constraint is a scaffold — temporary coercion justified by temporary necessity, with a planned exit. The current classification (tangled rope, snare when viewed by victims) holds because: (1) the coordination function is real (Ukrainian mobilization genuinely enables collective defense against Russian invasion); (2) the extraction is real (state takes labor without meaningful consent); (3) the temporal uncertainty is real (nobody knows whether demobilization will occur post-conflict). The high mandatrophy_resolved value reflects that the framework successfully models the constraint without collapsing into false naturalization (mountain) or pure extraction (snare-only). The constraint is what it appears to be: a state enforcing coercive conscription through genuine existential necessity, justified in present but temporally uncertain in duration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_threat_threshold,
    'What empirical conditions would falsify the ''existential threat'' justification for total mobilization coercion?',
    'Assess Russian military capacity to occupy/annex territory. Monitor diplomatic off-ramps (cease-fire, territorial settlement). Measure Ukrainian military effectiveness at different force levels. Timeline: when battlefield stabilizes or Russia sustains heavy losses.',
    'If threat is truly existential: mobilization remains a snare but justified snare (coordination function under duress). If threat recedes: constraint becomes pure extraction snare without coordination rationale — mandatrophy deteriorates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_threat_threshold, empirical, 'Whether existential threat threshold justifies total conscription coercion').

omega_variable(
    border_enforcement_alternative,
    'Could a voluntary professional military supplemented by incentivized mobilization (paid enlistment, temporary service limits, exit options) achieve equivalent military objectives with lower suppression?',
    'Comparative military effectiveness analysis. Historical precedent: how did Ukraine''s pre-2022 professional military perform? Model force composition scenarios with different conscription ratios.',
    'If voluntary system sufficient: suppression is contingent policy choice (0.72 → could be 0.40). Constraint reclassifies from pure snare toward tangled rope or scaffold. If professional insufficient: suppression is structurally necessary — true snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(border_enforcement_alternative, empirical, 'Whether volunteer/incentivized mobilization can substitute for coercive conscription').

omega_variable(
    post_war_demobilization_credibility,
    'Does Ukraine have institutional credibility to demobilize and restore exit options after conflict ends? Or will wartime conscription architecture become permanent?',
    'Examine post-conflict demobilization in comparable states (Israel, South Korea, Vietnam). Assess Ukrainian legislative debate on sunset clauses. Monitor international pressure for demobilization timelines.',
    'If demobilization credible: scaffold sunset is real — snare is time-bounded. If demobilization uncertain: snare may persist indefinitely — constraint reclassifies toward permanent piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_war_demobilization_credibility, preference, 'Whether post-conflict demobilization architecture will restore citizen exit options').

omega_variable(
    diaspora_flight_substitution,
    'Are males fleeing to diaspora (Poland, EU, Canada) functionally exempting themselves from mobilization, and does this create a hidden exit option that underestimates actual freedom?',
    'Quantify border crossing flows by gender and age. Track Ukrainian expat military enlistment rates. Measure Ukrainian government enforcement of extradition or asset seizure against diaspora.',
    'If flight is functional exit: effective suppression lower than 0.72 — constraint is more constrained/mobile than trapped. If Ukrainian government successfully pressures diaspora to return: suppression increases, snare becomes tighter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diaspora_flight_substitution, empirical, 'Whether diaspora mobility functions as a hidden exit option from conscription').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ukr_mobilization, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukrmob_theater_t0, ukr_mobilization, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ukrmob_theater_t12, ukr_mobilization, theater_ratio, 12, 0.35).
narrative_ontology:measurement(ukrmob_theater_t24, ukr_mobilization, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(ukrmob_extract_t0, ukr_mobilization, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ukrmob_extract_t6, ukr_mobilization, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(ukrmob_extract_t24, ukr_mobilization, base_extractiveness, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ukr_mobilization, enforcement_mechanism).
narrative_ontology:affects_constraint(ukr_mobilization, ukrainian_labor_market_collapse).
narrative_ontology:affects_constraint(ukr_mobilization, diaspora_brain_drain).
narrative_ontology:affects_constraint(ukr_mobilization, russian_mobilization_countermeasure).
narrative_ontology:affects_constraint(ukr_mobilization, nato_credibility_commitment).

% DUAL FORMULATION NOTE:
% Ukrainian mobilization is a snare (from conscript perspective) that solves a tangled rope (from state/coalition perspective). It decomposes into: (1) mobilization as coercive labor extraction (ε≈0.68, snare), and (2) mobilization as collective defense coordination (ε≈0.35, tangled rope). These are not two observables of one constraint but two structurally distinct mechanisms that happen to be enforced by the same legal apparatus. The framework models them as a single story with perspectival divergence rather than network decomposition because the enforcement mechanism is unified — but future analysis may decompose the labor extraction from the defense coordination into separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ukr_mobilization, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
