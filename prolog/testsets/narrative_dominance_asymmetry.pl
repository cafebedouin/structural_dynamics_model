% ============================================================================
% CONSTRAINT STORY: narrative_dominance_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_narrative_dominance_asymmetry, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: narrative_dominance_asymmetry
 *   human_readable: Narrative Dominance Asymmetry
 *   domain: epistemology/political_economy/communication
 *
 * SUMMARY:
 *   Narrative dominance asymmetry is the structural mechanism by which some
 *   framings of reality become institutionally backed and epistemically
 *   authoritative while others remain marginalized despite potentially equal
 *   or superior evidential standing. This constraint exhibits the full range
 *   of DR classifications depending on the observer's structural position
 *   relative to narrative authority. The asymmetry is not merely descriptive
 *   (some narratives are more widely believed) but extractive: dominant
 *   narratives concentrate epistemic authority, capture the right to
 *   interpret reality, and systematically delegitimize alternatives. The
 *   mechanism combines institutional gatekeeping (who gets media access,
 *   publishing platforms, educational authority), resource asymmetry (funding
 *   for certain narratives exceeds funding for alternatives), and
 *   internalized suppression (agents internalize the dominant frame as
 *   'truth' rather than 'institutional choice'). Over the past four decades,
 *   extractiveness has increased as information technology has created larger
 *   epistemically-unified populations while simultaneously enabling more
 *   sophisticated narrative coordination. The theater ratio (0.64) reflects
 *   that substantial institutional labor goes into maintaining narrative
 *   coherence without necessarily improving truth-tracking: narrative
 *   authority is maintained through ritual, repetition, institutional
 *   prestige, and delegitimization of alternatives more than through
 *   empirical validation. The constraint satisfies Tangled Rope criteria:
 *   genuine coordination function (dominant narratives do coordinate large
 *   populations effectively), asymmetric extraction (dominance is enforced
 *   against alternatives), and active enforcement (institutional mechanisms
 *   continuously delegitimize counter-narratives).
 *
 * KEY AGENTS:
 *   - Institutional Narrative Authority: Primary beneficiary (institutional/arbitrage) — controls distribution channels, defines what counts as credible, captures epistemic authority benefits
 *   - Subaltern Narratives: Primary victim (powerless/trapped) — lived experience contradicts dominant frame; cannot exit without losing social participation; carry dual burden of experience plus psychological cost of falsifying frame
 *   - Counter-Narrative Coalition: Secondary victim (moderate/constrained) — organized resistance faces resource barriers, marginalization, and delegitimization; genuine coordination exists but extraction exceeds benefit
 *   - Captured Intellectual Class: Secondary beneficiary with extraction (institutional/constrained) — scholars and cultural producers benefit from institutional backing but constrained by institutional boundaries; experience mixed coordination and extraction
 *   - Degraded Counter-Hegemonic Institutions: Performative actors (institutional/arbitrage) — formal opposition institutions maintain theater of alternative while lacking functional independence; persist through inertia and ideological commitment
 *   - Epistemic Commons: Abstract victim (powerless/trapped) — collective interest in truth-tracking cannot exit or organize; maximum extraction burden from false or suboptimal narratives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(narrative_dominance_asymmetry, 0.58).
domain_priors:suppression_score(narrative_dominance_asymmetry, 0.68).
domain_priors:theater_ratio(narrative_dominance_asymmetry, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(narrative_dominance_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(narrative_dominance_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(narrative_dominance_asymmetry, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(narrative_dominance_asymmetry, tangled_rope).
narrative_ontology:human_readable(narrative_dominance_asymmetry, "Narrative Dominance Asymmetry").
narrative_ontology:topic_domain(narrative_dominance_asymmetry, "epistemology/political_economy/communication").

domain_priors:requires_active_enforcement(narrative_dominance_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(narrative_dominance_asymmetry, narrative_authority_holders).
narrative_ontology:constraint_beneficiary(narrative_dominance_asymmetry, institutional_framers).
narrative_ontology:constraint_victim(narrative_dominance_asymmetry, epistemic_commons).
narrative_ontology:constraint_victim(narrative_dominance_asymmetry, subaltern_narratives).
narrative_ontology:constraint_victim(narrative_dominance_asymmetry, reality_baseline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBALTERN NARRATIVE (SNARE) — Trapped agents whose lived experience contradicts the dominant narrative have no exit from the constraint's suppression. Cannot move to alternative narrative space without losing social/economic participation. Maximum extraction: the subaltern bearer must carry both their own experience AND the psychological cost of the dominant frame that contradicts it. No coordination benefit perceived — only extraction.
constraint_indexing:constraint_classification(narrative_dominance_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COUNTER-NARRATIVE COALITION (TANGLED ROPE) — Organized groups attempting alternative framings face both coordination benefits (coalition-building, shared epistemic labor) and extraction (marginalization, resource scarcity, delegitimization). Constrained by institutional barriers to narrative amplification and career risk of heterodox positions. Genuine coordination function exists (counter-narratives do coordinate distributed observation and interpretation) alongside asymmetric extraction (effort exceeds impact achieved).
constraint_indexing:constraint_classification(narrative_dominance_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL NARRATIVE AUTHORITY (ROPE) — Primary beneficiary. Dominant institutions experience the constraint as a coordination mechanism: establishing a shared narrative frame enables collective action, policy alignment, and resource coordination. Net beneficiary position with low experienced extraction — the constraint subsidizes this agent's epistemic authority. Arbitrage exit: institutions can shift narratives opportunistically without structural cost.
constraint_indexing:constraint_classification(narrative_dominance_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPTURED INTELLECTUAL CLASS (TANGLED ROPE) — Scholars, journalists, and cultural producers embedded in institutional frameworks face mixed experience. Genuine coordination function: shared narrative frames enable scholarly communities, publishing networks, and knowledge accumulation. But also extraction: epistemic authority is concentrated in institutions that own distribution channels. Constrained exit: alternative narratives face resource and audience barriers even when intellectually robust. The intellectual class both benefits from and bears costs within the constraint.
constraint_indexing:constraint_classification(narrative_dominance_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DEGRADED COUNTER-HEGEMONIC INSTITUTIONS (PITON) — Historical counter-narratives (state media, alternative press, ideological institutions) often degrade into performative theater maintaining legitimacy claims rather than functional epistemic alternatives. Theater ratio high: formal opposition exists and goes through narrative motions but lacks genuine independent fact-gathering capacity. Persists through institutional inertia and ideological commitment rather than functional verification. The theater of opposition substitutes for actual narrative competition.
constraint_indexing:constraint_classification(narrative_dominance_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing the narrative asymmetry as inherent to human cognition: dominant narratives persist because they are more cognitively tractable, emotionally compelling, or better coordinated. This perspective sees narrative dominance as an immutable feature of social epistemology itself. However, structural data reveals this as a false summit — the dominance is enforced through institutional power, not cognitive necessity. The engine will detect this as naturalization of a contingent power arrangement.
constraint_indexing:constraint_classification(narrative_dominance_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narrative_dominance_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(narrative_dominance_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narrative_dominance_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(narrative_dominance_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(narrative_dominance_asymmetry, TR),
    TR >= 0.70.

:- end_tests(narrative_dominance_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The dominant narrative captures epistemic authority, controls interpretation of reality, and requires subaltern agents to internalize the frame as truth. But the extraction is not maximal (0.70+) because counter-narratives exist, some agents reject the frame, and institutional dominance is contested. The value reflects that extraction is significant and increasing but not total. Suppression (0.68): High. Multiple suppression mechanisms operate: institutional gatekeeping (publishing, media access), resource scarcity (funding concentrated in dominant frame), career risk (heterodox positions marginalize practitioners), and internalized suppression (agents believe the dominant frame rather than resist it). But suppression is not total — alternative spaces exist (internet, alternative media, academic margins). Theater ratio (0.64): Moderate-high. Substantial institutional labor maintains the narrative frame through ritual affirmation, repetition, institutional prestige-signaling, and delegitimization theater rather than through empirical validation. As dominant narratives face counterevidence, theater increases: more institutional energy goes into narrative maintenance when factual standing weakens. The rising trajectory (0.42 → 0.64) reflects increasing theater as dominant narratives face challenges.
 *
 * PERSPECTIVAL GAP:
 *   The most significant perspectival gap exists between the institutional authority (Rope at 0.65 directionality, negative chi) and the subaltern agent (Snare at 0.95 directionality, maximum chi). Both occupy the same constraint; one experiences coordination, the other pure extraction. This gap is not an epistemic illusion — it reflects real structural differences. The institutional authority genuinely benefits from the dominant narrative's coordination function. The subaltern agent genuinely bears extraction. The captured intellectual (Tangled Rope) occupies a liminal position: they benefit from institutional backing but constrained by institutional boundaries. Their classification differs from both the authority and the subaltern precisely because their structural position mixes benefit and extraction. The counter-narrative coalition (Tangled Rope) differs from subalterns because they have organized agency — they are not powerless — but still constrained. The degraded counter-hegemonic institution (Piton) performs opposition but without functional independence — theater substitutes for genuine alternative. The gap between these perspectives reveals that narrative dominance is not a monolithic phenomenon but a heterogeneous constraint with multiple structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is determined by their structural position relative to narrative authority: who controls interpretation, who bears the cost of frames they don't author, who can exit. Institutional authorities (beneficiaries with arbitrage exit) derive low d — they subsidize themselves. Subaltern agents (victims with trapped exit) derive high d — they bear maximum extraction. Counter-narrative coalitions (victims with constrained exit) derive intermediate d — they have some agency and some benefit but face barriers. Captured intellectuals (mixed beneficiary/victim status with constrained exit) derive intermediate d — their position is ambiguous but constrained. The derivation chain prioritizes beneficiary/victim status, then exit options. An agent can be a beneficiary of dominant narrative coordination but still face extraction if their exit is constrained (captured intellectual: institutional beneficiary but narrative-constrained). Conversely, an agent can be a nominal victim but have arbitrage-equivalent exit (dissidents with alternative media platforms). The directional values feed into f(d) which scales effective extractiveness. A powerless agent with d=0.95 experiences χ approximately 1.42 times base extractiveness; an institutional beneficiary with d=0.05 experiences χ approximately -0.12 times base extractiveness (they are subsidized by the constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine coordination (dominant narratives do coordinate large populations) from extractive enforcement (alternatives are suppressed through institutional power). The Tangled Rope classification captures both functions: coordination is real and valuable; extraction is also real and costly. The false summit (Mountain perspective) arises when the coordination function is mistaken for natural law — 'narrative dominance is inevitable because it serves coordination.' The response is structural: if dominance is merely for coordination, decentralized alternatives given equal resources should achieve equivalent coordination. The omega variable on counter-narrative scalability directly tests this. If decentralized narratives fail to scale despite equal resources, the dominance may have genuine coordination advantages. If they scale, dominance is maintenance of institutional power, not coordination necessity. The increasing extractiveness and theater ratio over the measurement interval indicate this is not a static coordination mechanism but an evolving extraction system. As dominant narratives face counterevidence, institutional labor (theater) increases to maintain dominance. This pattern is characteristic of extraction mechanisms protecting against alternatives, not coordination mechanisms optimizing collective action. The mandatrophy is resolved by treating narrative dominance as a real coordination mechanism that has accumulated extraction layers — Tangled Rope is the appropriate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrative_suppression_mechanism,
    'Is suppression of alternative narratives structural (institutional gatekeeping, resource barriers) or internalized (narrative authority internalized as truth by subaltern agents)?',
    'Post-barrier suppression tracking: when institutional barriers are removed (internet access, publishing platforms), does suppression persist? Longitudinal study of narrative adoption post-democratization.',
    'If primarily structural: subaltern agents can rapidly mobilize alternatives when barriers fall. If internalized: suppression persists even after institutional removal — agents carry the dominant frame internally. This determines whether constraint is primarily material (snare with external barriers) or cognitive (snare with internalized frames).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_suppression_mechanism, empirical, 'Whether narrative suppression is structural or internalized').

omega_variable(
    counter_narrative_epistemic_parity,
    'Do counter-narratives achieve observational equivalence with dominant narratives when given equal institutional resources and amplification?',
    'Controlled comparison: provide alternative narratives with equivalent media budgets, distribution channels, and institutional backing; measure adoption rates and perceived credibility compared to dominant frame.',
    'If parity achieved: constraint is primarily institutional power asymmetry (Tangled Rope). If dominant narrative persists: either dominant narrative has genuine epistemic advantages or agents have internalized authority structures (closer to Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_narrative_epistemic_parity, empirical, 'Whether alternative narratives achieve parity with institutional support').

omega_variable(
    narrative_fiction_entanglement,
    'What proportion of dominant narratives'' persistence derives from their utility for coordination vs. their fictive power (emotional resonance, narrative coherence, mythic appeal)?',
    'Cross-cultural comparison: do coordination-efficient narratives dominate even when empirically false? Historical analysis of narratives that persist despite falsification. Psychological testing of narrative adoption under uncertainty.',
    'If utility-driven: dominant narratives are optimized for coordination function (Rope perspective partially valid). If fiction-driven: dominance derives from affective capture rather than functional necessity (Snare with internalized suppression).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrative_fiction_entanglement, conceptual, 'Proportion of narrative dominance explained by coordination utility vs. fictive power').

omega_variable(
    counter_narrative_scalability,
    'Do decentralized counter-narratives (distributed social media, community-generated accounts) scale to coordinate large populations as effectively as centrally-controlled dominant narratives?',
    'Case studies of decentralized narrative coordination; measurement of alignment achieved, information propagation speed, and collective action capacity; comparison to centrally-controlled narrative outcomes.',
    'If decentralized narratives scale: constraint is primarily institutional power maintenance (extractive but not structurally necessary). If they fail to scale: dominant narrative may have genuine coordination advantages despite extractiveness (Tangled Rope justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_narrative_scalability, empirical, 'Whether decentralized narratives can achieve coordination at scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(narrative_dominance_asymmetry, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nda_tr_t0, narrative_dominance_asymmetry, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nda_tr_t20, narrative_dominance_asymmetry, theater_ratio, 20, 0.55).
narrative_ontology:measurement(nda_tr_t40, narrative_dominance_asymmetry, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(nda_be_t0, narrative_dominance_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nda_be_t20, narrative_dominance_asymmetry, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(nda_be_t40, narrative_dominance_asymmetry, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(narrative_dominance_asymmetry, identity_coordination).
narrative_ontology:boltzmann_floor_override(narrative_dominance_asymmetry, 0.12).
narrative_ontology:affects_constraint(narrative_dominance_asymmetry, epistemic_authority_concentration).
narrative_ontology:affects_constraint(narrative_dominance_asymmetry, reality_interpretation_monopoly).
narrative_ontology:affects_constraint(narrative_dominance_asymmetry, subaltern_epistemic_exclusion).

% DUAL FORMULATION NOTE:
% Narrative dominance asymmetry is a meta-constraint that operates across multiple domain-specific constraints. It is upstream of epistemic authority concentration (institutional power to define credible narratives) and reality interpretation monopoly (control over what counts as valid evidence). The extractiveness values differ: dominance asymmetry (0.58) is moderate-high; epistemic authority concentration (likely 0.42) focuses more narrowly on institutional gatekeeping; reality interpretation monopoly (likely 0.72) captures the deepest form of extraction (control over facticity itself). These three form a constraint family linked by narrative mechanism: dominance enables authority concentration enables interpretation control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(narrative_dominance_asymmetry, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
