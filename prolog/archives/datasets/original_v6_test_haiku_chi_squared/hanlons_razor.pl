% ============================================================================
% CONSTRAINT STORY: hanlons_razor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanlons_razor, []).

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
 *   constraint_id: hanlons_razor
 *   human_readable: Hanlon's Razor: Attribution Heuristic Constraining Malice Detection
 *   domain: social/cognitive
 *
 * SUMMARY:
 *   Hanlon's Razor ('Never attribute to malice that which is adequately
 *   explained by stupidity') is a heuristic that constrains causal inference
 *   about human intentions. It operates as a social constraint by suppressing
 *   accountability investigation: instead of asking 'Did they intend this
 *   outcome?' the razor prescribes 'Assume they didn't.' The constraint shows
 *   distinct readings across different structural positions. For genuinely
 *   incompetent actors, the razor provides coordination value
 *   (mistake-tolerance without shame). For institutions perpetuating
 *   negligence, the razor provides pure extraction value (accountability
 *   suppression). For harm recipients and truth-seeking epistemic
 *   communities, the razor provides pure extraction (suppression of
 *   legitimate intent analysis). The constraint exhibits high theater (widely
 *   cited in folk psychology, HR training, conflict resolution) but declining
 *   functionality (as institutional complexity increases, attributing
 *   negligence to incompetence becomes less tenable). The extractiveness has
 *   risen over the interval as organizations have become more sophisticated
 *   at using the razor to avoid accountability while maintaining the
 *   appearance of good faith.
 *
 * KEY AGENTS:
 *   - Harm Recipients: Primary victims (powerless/trapped) — suppressed from demanding intent investigation; cannot exit the razor frame
 *   - Epistemic Clarity / Truth-Seeking Communities: Primary victims (powerless/trapped) — prevented from diagnosing structural malice through pattern analysis
 *   - Genuinely Incompetent Actors: Mixed role (moderate/constrained) — benefit from mistake-tolerance coordination but constrained by inability to distinguish from strategic negligence
 *   - Negligence-Perpetuating Institutions: Primary beneficiaries (institutional/arbitrage) — use razor to suppress accountability investigation while maintaining good-faith appearance
 *   - Harm-Accountability Advocates: Organized challengers (organized/constrained) — see razor as useful bootstrap for mistake-tolerance but requiring sunset as complexity increases
 *   - Folk Psychology Establishment: Institutional maintainers (institutional/arbitrage) — sustain razor through citation and training; largely performative enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a social choice (accountability suppression) as a cognitive limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanlons_razor, 0.52).
domain_priors:suppression_score(hanlons_razor, 0.68).
domain_priors:theater_ratio(hanlons_razor, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanlons_razor, extractiveness, 0.52).
narrative_ontology:constraint_metric(hanlons_razor, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hanlons_razor, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanlons_razor, tangled_rope).
narrative_ontology:human_readable(hanlons_razor, "Hanlon's Razor: Attribution Heuristic Constraining Malice Detection").
narrative_ontology:topic_domain(hanlons_razor, "social/cognitive").

domain_priors:requires_active_enforcement(hanlons_razor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanlons_razor, structural_incompetence_actors).
narrative_ontology:constraint_beneficiary(hanlons_razor, institutional_negligence_perpetrators).
narrative_ontology:constraint_victim(hanlons_razor, harm_recipients).
narrative_ontology:constraint_victim(hanlons_razor, epistemic_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HARM RECIPIENT (SNARE) — Trapped in Hanlon's frame: cannot exit attribution regime; cannot demand investigation of structural malice because the razor preemptively classifies malicious systems as merely incompetent. Experiences suppression of alternatives (demand accountability, investigate intent, seek redress). d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.73.
constraint_indexing:constraint_classification(hanlons_razor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EPISTEMIC CLARITY / TRUTH-SEEKING (SNARE) — Trapped by the razor's suppression of intent analysis. When systems are designed to extract value through harm but disguised as incompetence, the razor prevents diagnosis. No exit from this constraint — it operates as a meta-level filter on permissible explanations. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.75.
constraint_indexing:constraint_classification(hanlons_razor, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GENUINELY INCOMPETENT ACTOR (TANGLED ROPE) — Benefits from Hanlon's Razor: actual mistakes are given generous interpretation, enabling organizational learning without punitive shame-spiral. But also constrained: the razor provides cover for actors who are *strategically* negligent — culpable ignorance disguised as honest mistake. This agent experiences both coordination (mistake-tolerance) and extraction (protection of willful negligence). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(hanlons_razor, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL NEGLIGENCE PERPETUATOR (ROPE) — Benefits from Hanlon's Razor as a pure coordination mechanism: 'Never assume malice, assume incompetence' enables institutions to avoid accountability investigations while maintaining the appearance of good faith. Solves the institutional problem of how to tolerate inevitable human error. But the classification hinges on the fiction that institutional negligence is merely human error at scale, not structural extraction. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(hanlons_razor, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ACCOUNTABILITY ADVOCATES (SCAFFOLD) — Organized actors (victim advocates, investigative journalists, safety researchers) see Hanlon's Razor as a temporary coordinating heuristic useful for building systems that tolerate human error, but requiring a sunset clause: as systems become more complex and stakes higher, the razor's blanket suppression of intent analysis must give way to structural auditing (intent revealed through pattern analysis, feedback loops, design choices). Sees the blade becoming less sharp as institutions mature. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(hanlons_razor, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FOLK PSYCHOLOGY / INSTITUTIONAL WISDOM (PITON) — Hanlon's Razor is widely cited in organizational culture, HR training, conflict resolution literature, and popular philosophy as a core norm. But its actual enforcement is largely theatrical: organizations invoke the razor when convenient (suppress malice investigations) and abandon it when inconvenient (pursue deliberate fraud). theater_ratio=0.65 reflects this performance-functionality gap. The razor persists through inertia and status as folk wisdom rather than through demonstrable effectiveness in reducing harm or improving coordination. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.03.
constraint_indexing:constraint_classification(hanlons_razor, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Claims that Hanlon's Razor reflects an immutable cognitive fact: humans are poorly equipped to infer intent from action, so attributing malice is systemically unreliable. Therefore, the razor encodes a fundamental epistemic limit — we *cannot* reliably distinguish malice from incompetence at scale. But the structural data (ε=0.52, suppression=0.68) contradicts this: the constraint is not about cognitive capacity but about institutional suppression of intent investigation. ε=0.52 shows this is extractive, not just epistemically cautious. The mountain classification is a FALSE SUMMIT — the 'cognitive limit' framing naturalizes what is actually a contingent social choice to avoid accountability investigation.
constraint_indexing:constraint_classification(hanlons_razor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanlons_razor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanlons_razor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanlons_razor, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hanlons_razor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanlons_razor, TR),
    TR >= 0.70.

:- end_tests(hanlons_razor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The razor suppresses a legitimate epistemic tool (intent investigation) that would benefit harm recipients and truth-seeking. But it also provides genuine coordination value (mistake-tolerance) for organizations that need to function despite inevitable human error. The net extractiveness reflects that the suppression is not total — determined actors can investigate intent through structural analysis, pattern detection, and forensic methods. The razor raises the cost and social friction of accountability investigation but does not make it impossible. Suppression (0.68): High. Strong institutional normalization against intent investigation ('Don't assume malice') creates genuine barriers to accountability mechanisms. Social cost of violating the norm (appearing uncharitable, accusatory, paranoid) suppresses alternatives. But suppression is not total — investigative journalism, safety researchers, and legal systems sometimes override the razor. Theater ratio (0.65): Moderate-high. The razor is widely cited in organizational culture and training as a norm of good faith and charitable interpretation. But enforcement is selective: invoked to suppress investigation of institutional negligence, abandoned when investigating individual fraud. The theatrical performance has increased as organizations have become more sophisticated at weaponizing the razor for reputation management.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a stark perspectival divergence. The institution and the incompetent actor see Rope (coordination enabling mistake-tolerance). The harm recipient and truth-seeker see Snare (pure suppression of accountability). The accountability advocates see Scaffold (temporary tool requiring sunset as systems mature). The folk psychology establishment sees Piton (degraded institution maintained through inertia). The analytical observer risks seeing Mountain (cognitive limit) — but the structural data reveals this as a false summit. The perspectival gap reflects that Hanlon's Razor has genuinely different structural effects depending on whether you benefit from suppressed accountability (beneficiary perspective) or bear the cost (victim perspective). The analytical observer's false summit is the key diagnostic: the appearance of cognitive naturalism ('we simply cannot reliably infer intent') masks a social choice ('we choose not to investigate intent because accountability is costly').
 *
 * DIRECTIONALITY LOGIC:
 *   Harm recipients: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — no exit, no agency, pure suppression of accountability. Epistemic clarity: Victim + trapped → d≈0.95, f(d)≈1.42. Near-maximum extraction — abstract collective good completely suppressed by the razor's frame. Genuinely incompetent actor: Mixed beneficiary + victim, constrained → d≈0.55, f(d)≈0.75. Moderate extraction — benefits from mistake-tolerance but also victimized by inability to distinguish themselves from negligent actors. Institutional negligence perpetuator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary — uses razor to suppress accountability while maintaining good-faith appearance. Accountability advocates: Organized + constrained → d≈0.50, f(d)≈0.65. Moderate extraction — see the problem but have agency to challenge it (scaffold perspective). Folk psychology establishment: Institutional + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary but piton classification comes from theater gate, not chi. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. False summit — naturalizes social choice as cognitive limit.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL CASE FOR MANDATROPHY RESOLUTION: Hanlon's Razor exemplifies the mandatrophy between 'coordination heuristic' and 'extraction mechanism.' The resolution lies in structural decomposition: the razor functions as Rope for some actors (mistake-tolerance coordination for organizations that need to function despite inevitable error) and as Snare for others (suppression of accountability investigation for institutions that benefit from obscured intent). The classification is not ambiguous — it is genuinely different depending on structural position. For the harm recipient (powerless/trapped), the razor is Snare: it suppresses the only tool available for holding powerful actors accountable (pattern analysis revealing intent through repeated beneficial outcomes). For the organization implementing mistake-tolerance (moderate/constrained), the razor is Tangled Rope: it provides real coordination value (mistake-tolerance, psychological safety) but also enables strategic negligence to hide under incompetence. For the negligence perpetuator (institutional/arbitrage), the razor is Rope from their perspective but masks Snare extraction from the victim's perspective. The mandatrophy is resolved by recognizing that these are not conflicting readings of one constraint but different structural constraints linked by the same heuristic. The analyticial observer's Mountain classification is false — it naturalizes the razor as inevitable when the actual structure is institutional choice. The evidence: organizations that implement robust intent investigation (e.g., incident investigation in aviation, safety analysis in industrial systems) do not suffer from excessive malice accusations. They function better by distinguishing culpable intent from honest mistake — using the razor to set the burden of proof for intent investigation (high), not to suppress investigation entirely (current frame).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_distinguishability_threshold,
    'At what scale or complexity does intent become epistemically indistinguishable from incompetence, and is this a cognitive limit or a choice to stop investigating?',
    'Forensic analysis of outcomes consistent with malice (e.g., regulatory arbitrage, systemic bias in ''mistakes'', design choices that predictably enable harm); comparison of institutional behavior under accountability vs non-accountability regimes',
    'If cognitive limit: Hanlon''s Razor is a valid Mountain or Rope. If social choice: Hanlon''s Razor is a suppression mechanism (Snare/Tangled Rope). Current evidence suggests strong social choice component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_distinguishability_threshold, empirical, 'Whether intent distinguishability is a cognitive limit or institutional choice').

omega_variable(
    feedback_loop_malice_detection,
    'Do patterns of repeated ''mistakes'' that systematically benefit certain actors constitute de facto malice sufficient to justify accountability investigation regardless of stated intent?',
    'Time-series analysis of institutional ''errors'': correlation between error patterns and beneficiary status; counterfactual: would the same error rate persist if mistake-maker bore the full cost of the error?',
    'If pattern-based detection valid: Hanlon''s Razor suppresses a legitimate epistemic tool (structural accountability). If patterns are only post-hoc correlation: Hanlon''s Razor correctly restricts premature attribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_loop_malice_detection, empirical, 'Whether repeated beneficial mistakes reveal hidden malice').

omega_variable(
    organizational_negligence_culpability,
    'Is institutional negligence (e.g., ''failure to implement known safety measures'') categorically different from malice, or does the distinction collapse under structural analysis?',
    'Organizational analysis of negligence cases: were known harms ignored despite available remediation? Were decision-makers insulated from consequences? Did negligence predictably benefit decision-makers?',
    'If negligence is distinct: Hanlon''s Razor provides valid epistemic hygiene. If negligence collapses into structural malice: the razor enables culpable ignorance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_negligence_culpability, conceptual, 'Relationship between institutional negligence and structural malice').

omega_variable(
    accountability_asymmetry_under_razor,
    'Does Hanlon''s Razor create systemic asymmetry where institutional actors face reduced accountability (generous intent interpretation) while powerless actors face strict accountability (intent policed harshly)?',
    'Comparative analysis of institutional vs individual malice prosecution; investigation of which actor classes benefit from generous intent interpretation in legal/organizational proceedings',
    'If asymmetry exists: Hanlon''s Razor is a vehicle for extraction (Snare). If applied symmetrically: Razor is genuine coordination mechanism (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accountability_asymmetry_under_razor, empirical, 'Whether Hanlon''s Razor applies asymmetrically to institutions vs individuals').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanlons_razor, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanlon_tr_t0, hanlons_razor, theater_ratio, 0, 0.45).
narrative_ontology:measurement(hanlon_tr_t5, hanlons_razor, theater_ratio, 5, 0.58).
narrative_ontology:measurement(hanlon_tr_t10, hanlons_razor, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(hanlon_be_t0, hanlons_razor, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hanlon_be_t5, hanlons_razor, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(hanlon_be_t10, hanlons_razor, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanlons_razor, enforcement_mechanism).
narrative_ontology:affects_constraint(hanlons_razor, accountability_suppression).
narrative_ontology:affects_constraint(hanlons_razor, culpable_ignorance).
narrative_ontology:affects_constraint(hanlons_razor, institutional_negligence_norm).

% DUAL FORMULATION NOTE:
% Hanlon's Razor decomposes into two distinct constraints: (1) mistake-tolerance-as-coordination (Rope/Tangled Rope, ε≈0.30-0.40) operating for genuinely incompetent actors in organizations that need psychological safety; (2) accountability-suppression-as-extraction (Snare, ε≈0.60+) operating for institutions using the razor to preempt intent investigation. These are not observables of one constraint but structurally different mechanisms linked by the same heuristic. The extractiveness value (0.52) represents a blend; full structural analysis requires decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanlons_razor, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
