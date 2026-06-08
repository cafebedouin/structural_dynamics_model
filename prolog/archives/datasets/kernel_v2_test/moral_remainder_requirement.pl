% ============================================================================
% CONSTRAINT STORY: moral_remainder_requirement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moral_remainder_requirement, []).

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
 *   constraint_id: moral_remainder_requirement
 *   human_readable: Moral Remainder Requirement in Dirty Hands Ethics
 *   domain: political_philosophy/normative_ethics/applied_ethics
 *
 * SUMMARY:
 *   The moral remainder requirement in dirty hands ethics creates a
 *   structural tension between the need for coherent practical guidance and
 *   the desire to preserve absolutist moral intuitions. The constraint
 *   emerged in political philosophy (Walzer 1973, Nagel 1978) as an attempt
 *   to reconcile the intuition that certain acts (torture, killing innocents)
 *   are absolutely wrong with the recognition that political leaders
 *   sometimes must perform them. The remainder requirement — that agents must
 *   experience remorse/guilt even for acts that were right and necessary —
 *   distinguishes dirty hands theory from threshold deontology (which permits
 *   violations above a threshold with no remainder) and from consequentialism
 *   (which has no absolute constraints). The constraint exhibits tangled rope
 *   structure: it genuinely coordinates a real philosophical problem (how to
 *   take moral constraints seriously while acknowledging tragic necessity)
 *   but embeds substantial extraction (the logical incoherence prevents
 *   practical resolution, and the mandatory guilt extracts psychological cost
 *   without providing decision guidance). The theater ratio has risen over
 *   the interval as the literature has become increasingly self-referential,
 *   rehearsing the paradox without resolving it. Recent developments
 *   (experimental philosophy, formal decision theory, threshold deontology)
 *   represent potential sunset pathways, though the constraint remains
 *   institutionally entrenched.
 *
 * KEY AGENTS:
 *   - Practical Decision-Makers: Primary victims (powerless/trapped) — political leaders, emergency responders, triage physicians who must act under the constraint and bear the mandatory guilt
 *   - Coherent Justification: Abstract victim (powerless/trapped) — the epistemic good of logical coherence, which cannot organize or exit
 *   - Applied Ethicists: Mixed position (moderate/constrained) — benefit from the coordination function but bear extraction cost from the incoherence
 *   - Philosophical Gatekeepers: Primary beneficiaries (institutional/arbitrage) — maintain the dirty hands literature as a distinct research program
 *   - Absolutist Intuitions: Beneficiary group (powerless/identity_locked) — agents whose moral identity is constituted through absolutist commitments
 *   - Coherentist Reform Coalition: Organized agents (organized/mobile) — developing alternatives with sunset logic
 *   - Dirty Hands Literature: Institutional entity (institutional/arbitrage) — the academic subfield as a self-perpetuating structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moral_remainder_requirement, 0.48).
domain_priors:suppression_score(moral_remainder_requirement, 0.62).
domain_priors:theater_ratio(moral_remainder_requirement, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moral_remainder_requirement, extractiveness, 0.48).
narrative_ontology:constraint_metric(moral_remainder_requirement, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(moral_remainder_requirement, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moral_remainder_requirement, tangled_rope).
narrative_ontology:human_readable(moral_remainder_requirement, "Moral Remainder Requirement in Dirty Hands Ethics").
narrative_ontology:topic_domain(moral_remainder_requirement, "political_philosophy/normative_ethics/applied_ethics").

domain_priors:requires_active_enforcement(moral_remainder_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moral_remainder_requirement, absolutist_intuitions).
narrative_ontology:constraint_beneficiary(moral_remainder_requirement, moral_seriousness_signaling).
narrative_ontology:constraint_beneficiary(moral_remainder_requirement, philosophical_gatekeepers).
narrative_ontology:constraint_victim(moral_remainder_requirement, coherent_justification).
narrative_ontology:constraint_victim(moral_remainder_requirement, practical_decision_makers).
narrative_ontology:constraint_victim(moral_remainder_requirement, consequentialist_frameworks).
narrative_ontology:constraint_vindicates(moral_remainder_requirement, deontological_residue_thesis).
narrative_ontology:constraint_vindicates(moral_remainder_requirement, moral_tragedy_irreducibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICAL DECISION-MAKER (SNARE) — Political leaders, emergency responders, triage physicians facing genuine tragic choices with no time for philosophical reconciliation. Trapped by the requirement to both act decisively AND carry permanent moral stain. The remainder requirement extracts psychological cost (mandatory guilt) while providing no decision guidance. Cannot exit the framework because institutional norms enforce remorse-as-legitimacy.
constraint_indexing:constraint_classification(moral_remainder_requirement, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: APPLIED ETHICIST (TANGLED ROPE) — Professional philosophers working in bioethics, military ethics, political theory. Benefits from the remainder requirement as a coordination mechanism (distinguishes dirty hands from mere consequentialism, preserves moral seriousness) but also bears extraction cost (the incoherence makes practical guidance difficult, career risk in challenging the orthodoxy). Constrained exit: can critique the framework but faces professional marginalization for abandoning it entirely.
constraint_indexing:constraint_classification(moral_remainder_requirement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHILOSOPHICAL GATEKEEPER (ROPE) — Senior scholars, journal editors, tenure committees who maintain the dirty hands literature as a distinct research program. Primary beneficiaries: the remainder requirement creates a stable coordination point that differentiates their subfield from both pure consequentialism and threshold deontology. Arbitrage exit: can move between frameworks as needed, face no cost for the incoherence because they are not the ones making the decisions.
constraint_indexing:constraint_classification(moral_remainder_requirement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COHERENTIST REFORM COALITION (SCAFFOLD) — Philosophers developing alternatives: threshold deontology (no remainder), sophisticated consequentialism (agent-relative constraints without incoherence), virtue ethics (character-based rather than act-based). See the remainder requirement as a transitional confusion that will be resolved as the field matures. Mobile exit: can adopt alternative frameworks without career destruction. Sunset logic: as formal decision theory and experimental philosophy mature, the incoherence will become untenable.
constraint_indexing:constraint_classification(moral_remainder_requirement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ABSOLUTIST INTUITION (MOUNTAIN) — Agents whose moral psychology is constituted through absolutist commitments (torture is always wrong, killing innocents is always wrong) but who also recognize tragic necessity. Identity-locked: cannot abandon the absolutist frame without dissolving their moral identity, yet cannot deny that some violations are necessary. Experiences the remainder requirement as an immutable feature of moral reality itself — the guilt IS the price of maintaining moral seriousness. This is a false summit: the constraint naturalizes a contingent philosophical construction.
constraint_indexing:constraint_classification(moral_remainder_requirement, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 6: DIRTY HANDS LITERATURE (PITON) — The academic subfield as an institutional entity. The remainder requirement's original function (reconciling absolutist intuitions with political necessity) has atrophied into a self-referential literature that mostly cites itself. High theater ratio: papers rehearse the paradox without resolving it, maintaining the genre through ritual rather than progress. Persists through institutional inertia (established journals, conference panels, graduate seminars) despite providing little practical guidance.
constraint_indexing:constraint_classification(moral_remainder_requirement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The remainder requirement genuinely coordinates a real problem (how to take moral constraints seriously while acknowledging tragic necessity) but embeds substantial extraction (the incoherence prevents practical resolution, the mandatory guilt extracts psychological cost without decision guidance). The coordination function is real: dirty hands theory distinguishes itself from both pure consequentialism (no constraints) and threshold deontology (constraints with no remainder). The extraction is also real: the logical incoherence (an act is both right and wrong) makes the framework unusable for actual decision-making, and the remorse requirement extracts ongoing psychological cost from agents who had no better option.
constraint_indexing:constraint_classification(moral_remainder_requirement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_remainder_requirement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moral_remainder_requirement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moral_remainder_requirement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moral_remainder_requirement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(moral_remainder_requirement, TR),
    TR >= 0.70.

:- end_tests(moral_remainder_requirement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The remainder requirement extracts psychological cost (mandatory guilt) from practical decision-makers while providing no decision guidance, and the logical incoherence (an act is both right and wrong) makes the framework difficult to operationalize. However, extraction is not maximal because the constraint does coordinate a genuine philosophical problem and some agents (gatekeepers, absolutist intuition holders) genuinely benefit. The value reflects that roughly half the constraint's operation is extractive overhead rather than coordination function. Suppression (0.62): Moderate-high. Significant barriers to exit include: institutional enforcement (political legitimacy requires performing remorse), professional norms (applied ethics careers depend on engaging the literature), identity lock (absolutist intuitions cannot be abandoned without dissolving moral identity), and the absence of widely-accepted alternatives. Suppression has increased over the interval as the literature became more entrenched. Theater ratio (0.58): Moderate-high. Much of the dirty hands literature rehearses the paradox without resolving it — papers cite the canonical texts (Walzer, Nagel, Williams), acknowledge the incoherence, and then proceed as if the framework were usable. The theater has increased as the subfield matured into a self-referential genre, though recent work in experimental philosophy and formal ethics represents genuine functional engagement. The slight decline in the final measurement reflects growing coherentist pressure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon appears differently depending on the observer's position. Practical decision-makers see pure extraction (snare) — they bear the guilt requirement with no benefit. Applied ethicists see mixed coordination and extraction (tangled rope) — the framework both enables and constrains their work. Philosophical gatekeepers see coordination (rope) — the remainder requirement creates a stable research program. The coherentist coalition sees a temporary problem with a sunset (scaffold) — alternatives are maturing. Absolutist intuition holders see an immutable moral reality (mountain) — but this is a false summit, naturalizing a contingent philosophical construction. The dirty hands literature sees its own degraded ritual (piton) — the function has atrophied into self-reference. The analytical observer sees the full structure: genuine coordination (distinguishing dirty hands from consequentialism and threshold deontology) embedded with substantial extraction (incoherence prevents practical use, mandatory guilt extracts cost without guidance).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Practical decision-makers are victims with trapped exit → high d → high experienced extraction (snare). Applied ethicists are both beneficiaries (coordination function) and victims (incoherence cost) with constrained exit → moderate d → moderate extraction (tangled rope). Philosophical gatekeepers are beneficiaries with arbitrage exit → low d → low or negative extraction (rope). Absolutist intuition holders are beneficiaries (the constraint preserves their moral framework) but identity-locked → moderate d because they cannot exit the frame even though it benefits them (mountain, but false summit). The coherentist coalition is organized with mobile exit → low d (scaffold). The dirty hands literature as institutional entity is a beneficiary with arbitrage exit → low d (piton, but classification derives from theater gate rather than directionality). The analytical observer sees both coordination and extraction with analytical exit → moderate d (tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The moral remainder requirement exhibits mandatrophy in its purest form: the original mandate (reconcile absolutist intuitions with political necessity) has been achieved insofar as the framework exists and is widely discussed, but the constraint persists despite failing to provide coherent practical guidance. The incoherence (an act is both right and wrong) was present from the beginning, but the literature has not resolved it — instead, the paradox has become the genre's defining feature. The mandate has outlived its function because the coordination role (maintaining a distinct research program) now dominates the original justification (providing usable ethical guidance). The theater ratio trajectory shows this clearly: rising from 0.35 to 0.62 as the literature became more self-referential, with a slight recent decline as coherentist pressure mounts. The constraint is resolved as tangled rope rather than pure snare because the coordination function remains real — dirty hands theory does distinguish itself from alternatives and does capture something about the phenomenology of tragic choice — but the extraction is substantial and growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remainder_vs_regret_distinction,
    'Is the moral remainder a genuine normative residue (the act remains wrong even though justified) or merely rational regret (wishing the situation had been different)?',
    'Conceptual analysis distinguishing normative remainder from non-normative regret; empirical psychology of moral emotions in tragic choice scenarios',
    'If genuine normative residue: the incoherence is real and dirty hands theory is structurally unstable. If merely regret: threshold deontology subsumes dirty hands without remainder, and the constraint collapses to a coordination mechanism with no extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remainder_vs_regret_distinction, conceptual, 'Whether moral remainder is normative residue or rational regret').

omega_variable(
    absolutist_intuition_naturalization,
    'Are absolutist moral intuitions (torture is always wrong, killing innocents is always wrong) natural features of human moral psychology, or culturally constructed commitments that vary across populations?',
    'Cross-cultural experimental philosophy; developmental psychology of moral absolutes; historical variation in deontological commitments',
    'If natural: the remainder requirement reflects an immutable feature of moral cognition (mountain from more perspectives). If constructed: the constraint is a contingent philosophical artifact that benefits those who hold absolutist commitments (snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_intuition_naturalization, empirical, 'Whether absolutist intuitions are natural or constructed').

omega_variable(
    practical_guidance_threshold,
    'At what level of logical incoherence does a normative framework become unusable for practical decision-making?',
    'Case studies of actual decision-makers using dirty hands reasoning vs threshold deontology vs consequentialism; measurement of decision paralysis, post-decision regret, and action-guidance clarity',
    'If threshold is low: dirty hands theory is already past the usability boundary and the extraction is severe. If threshold is high: agents can operate with the incoherence and the extraction is moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_guidance_threshold, empirical, 'Incoherence threshold for practical usability').

omega_variable(
    remorse_enforcement_mechanism,
    'What institutional mechanisms enforce the remorse requirement, and how much of the enforcement is internalized vs external?',
    'Analysis of political legitimacy discourse (leaders who show no remorse vs those who perform guilt); professional ethics codes requiring acknowledgment of moral cost; psychological studies of internalized vs performed remorse',
    'If primarily external: the constraint is a social norm that can be challenged (lower suppression). If primarily internalized: the constraint is identity-locked and suppression is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remorse_enforcement_mechanism, empirical, 'Whether remorse enforcement is external or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moral_remainder_requirement, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moral_rem_theater_1970, moral_remainder_requirement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(moral_rem_theater_1985, moral_remainder_requirement, theater_ratio, 15, 0.48).
narrative_ontology:measurement(moral_rem_theater_2000, moral_remainder_requirement, theater_ratio, 30, 0.58).
narrative_ontology:measurement(moral_rem_theater_2015, moral_remainder_requirement, theater_ratio, 45, 0.62).
narrative_ontology:measurement(moral_rem_theater_2026, moral_remainder_requirement, theater_ratio, 56, 0.58).

% Extraction over time
narrative_ontology:measurement(moral_rem_extract_1970, moral_remainder_requirement, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(moral_rem_extract_1985, moral_remainder_requirement, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(moral_rem_extract_2000, moral_remainder_requirement, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(moral_rem_extract_2015, moral_remainder_requirement, base_extractiveness, 45, 0.52).
narrative_ontology:measurement(moral_rem_extract_2026, moral_remainder_requirement, base_extractiveness, 56, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(moral_rem_suppress_1970, moral_remainder_requirement, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(moral_rem_suppress_2000, moral_remainder_requirement, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(moral_rem_suppress_2026, moral_remainder_requirement, suppression_requirement, 56, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moral_remainder_requirement, identity_coordination).

% DUAL FORMULATION NOTE:
% The moral remainder requirement is downstream of the logical coherence paradox (an act cannot be both right and wrong in the same respect). The upstream constraint has its own extractiveness reflecting the general problem of logical incoherence in normative frameworks; the remainder requirement has its own extractiveness reflecting the specific application to dirty hands ethics and the mandatory guilt mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(moral_remainder_requirement, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
