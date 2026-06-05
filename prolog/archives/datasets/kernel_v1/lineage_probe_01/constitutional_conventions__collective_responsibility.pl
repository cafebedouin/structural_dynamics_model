% ============================================================================
% CONSTRAINT STORY: constitutional_conventions__collective_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_conventions__collective_responsibility, []).

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
 *   constraint_id: constitutional_conventions__collective_responsibility
 *   human_readable: Cabinet Collective Responsibility: Ministers' Public Dissent Suppressed by Career Extraction
 *   domain: political/legal/constitutional_conventions
 *
 * SUMMARY:
 *   The convention of collective cabinet responsibility requires ministers to
 *   defend in public any decision they opposed in private — or resign. This
 *   constraint manufactures executive coherence by suppressing public
 *   ministerial dissent. The Prime Minister (PM) benefits from a unified
 *   cabinet front that prevents daily media fragmentation. Dissenting
 *   ministers bear the cost: they must either suppress honest positions or
 *   exit (resignation). The constraint exhibits genuine coordination function
 *   (cabinet government is genuinely harder without collective
 *   responsibility) coupled with asymmetric extraction (ministers lose voice,
 *   PM gains control). This is neither pure coordination (Rope) nor pure
 *   extraction (Snare) but a hybrid (Tangled Rope): the same mechanism that
 *   coordinates cabinet action also extracts ministerial honesty. The
 *   constraint's theater ratio (0.55) reflects that unified cabinet
 *   presentation is partly functional coordination and partly performative
 *   narrative — the PM physically depends on cabinet unity for government
 *   continuity, but much of what appears as unity is performed
 *   dissent-suppression rather than genuine agreement. The measurement
 *   trajectory shows suppression requirement increasing over the past 40
 *   years as media fragmentation makes cabinet discipline harder to maintain
 *   and thus requires stronger enforcement.
 *
 * KEY AGENTS:
 *   - Dissenting Minister: Moderate power / constrained exit — faces binary choice between public silence and resignation; bears extraction of honest representation
 *   - Prime Minister / Executive Core: Institutional power / arbitrage exit — benefits from unified presentation; coordinates government; experiences constraint as pure Rope
 *   - Same-Party Backbench MP: Powerful within party / constrained exit — constrained by party discipline; benefits from party coherence; mixed Tangled Rope experience
 *   - Opposition Parties: Powerful when out of power / constrained when in power — cannot exploit ministerial dissent because norm will bind them in future; generational interest in cabinet stability
 *   - Parliament (Assembly): Institutional / arbitrage — formal power to interrogate individual ministers degraded; collective responsibility reduces accountability capacity (Piton)
 *   - Analytical Observer: Analytical / civilizational — risks naturalizing contingent institutional choice as inherent feature of cabinet government (false summit candidate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_conventions__collective_responsibility, 0.58).
domain_priors:suppression_score(constitutional_conventions__collective_responsibility, 0.68).
domain_priors:theater_ratio(constitutional_conventions__collective_responsibility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_conventions__collective_responsibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_conventions__collective_responsibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_conventions__collective_responsibility, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_conventions__collective_responsibility, tangled_rope).
narrative_ontology:human_readable(constitutional_conventions__collective_responsibility, "Cabinet Collective Responsibility: Ministers' Public Dissent Suppressed by Career Extraction").
narrative_ontology:topic_domain(constitutional_conventions__collective_responsibility, "political/legal/constitutional_conventions").

domain_priors:requires_active_enforcement(constitutional_conventions__collective_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_conventions__collective_responsibility, 'f1003613-c11c-46d2-90bd-67976002fc39').
narrative_ontology:cs_kernel_codification('f1003613-c11c-46d2-90bd-67976002fc39', formalized).
narrative_ontology:cs_authority_grounding('f1003613-c11c-46d2-90bd-67976002fc39', lineage).
narrative_ontology:cs_interpretation_layer_present('f1003613-c11c-46d2-90bd-67976002fc39').
narrative_ontology:cs_reading_relation('f1003613-c11c-46d2-90bd-67976002fc39', constitutional_conventions__ministerial_responsibility, influences).
narrative_ontology:cs_reading_relation('f1003613-c11c-46d2-90bd-67976002fc39', constitutional_conventions__royal_assent_convention, coexists_with).
narrative_ontology:cs_reading_relation('f1003613-c11c-46d2-90bd-67976002fc39', constitutional_conventions__salisbury_convention, coexists_with).
narrative_ontology:cs_axiom('f1003613-c11c-46d2-90bd-67976002fc39', foundational, prime_minister_coherence_requires_public_unity).
narrative_ontology:cs_axiom_status(prime_minister_coherence_requires_public_unity, holdable).
narrative_ontology:cs_axiom_grounding('f1003613-c11c-46d2-90bd-67976002fc39', prime_minister_coherence_requires_public_unity, instrumental).
narrative_ontology:cs_axiom('f1003613-c11c-46d2-90bd-67976002fc39', foundational, dissent_suppression_justifiable_by_executive_necessity).
narrative_ontology:cs_axiom_status(dissent_suppression_justifiable_by_executive_necessity, holdable).
narrative_ontology:cs_axiom_grounding('f1003613-c11c-46d2-90bd-67976002fc39', dissent_suppression_justifiable_by_executive_necessity, instrumental).
narrative_ontology:cs_reference_frame('f1003613-c11c-46d2-90bd-67976002fc39', unified_cabinet_authority).
narrative_ontology:cs_drift_state('f1003613-c11c-46d2-90bd-67976002fc39', contemporary_fragmentary_media, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f1003613-c11c-46d2-90bd-67976002fc39', '').
narrative_ontology:cs_kernel_id(constitutional_conventions__collective_responsibility, constitutional_conventions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_conventions__collective_responsibility, prime_minister).
narrative_ontology:constraint_beneficiary(constitutional_conventions__collective_responsibility, executive_coherence).
narrative_ontology:constraint_beneficiary(constitutional_conventions__collective_responsibility, party_discipline).
narrative_ontology:constraint_victim(constitutional_conventions__collective_responsibility, dissenting_ministers).
narrative_ontology:constraint_victim(constitutional_conventions__collective_responsibility, parliamentary_transparency).
narrative_ontology:constraint_victim(constitutional_conventions__collective_responsibility, democratic_deliberation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING MINISTER (SNARE) — Faces a genuine binary: defend a decision they privately opposed or resign. Career cost of resignation is severe (loss of ministerial salary, access, visibility, future advancement). Suppression is structural — the minister cannot speak their honest position without exiting the cabinet. Public dissent equals automatic dismissal. The constraint extracts honest representation from the minister and delivers it to executive coherence.
constraint_indexing:constraint_classification(constitutional_conventions__collective_responsibility, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIME MINISTER / EXECUTIVE CORE (ROPE) — Benefits from unified cabinet presentation; experiences the constraint as pure coordination. The PM can govern effectively only if the cabinet appears unified. From the PM's perspective, the constraint solves a genuine collective action problem: without collective responsibility, every cabinet decision would leak dissent, paralyzing executive action. The PM has arbitrage options (dissolve cabinet, reshuffle, appeal to party loyalty) and net benefit from the constraint.
constraint_indexing:constraint_classification(constitutional_conventions__collective_responsibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SAME-PARTY BACKBENCH MP (TANGLED ROPE) — Constrained by party discipline: cannot openly join dissenting minister without party penalty. But also benefits from party coherence (party discipline that silences ministers also prevents opposition fragmentation, protecting the backbencher's own seat). Exit cost is party expulsion or marginalization (constrained, not trapped — backbencher can survive outside the party structure). Mixed extraction: forced conformity on dissenting positions, but genuine coordination benefit from party unity.
constraint_indexing:constraint_classification(constitutional_conventions__collective_responsibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: OPPOSITION PARTIES (TANGLED ROPE) — Constrained by the same collective responsibility norm applying to their own future cabinets. Cannot easily weaponize ministerial dissent because doing so establishes precedent that will bind them when in government. Also benefits from cabinet stability in that orderly government handoff is easier than managing a fractured cabinet. Extraction: cannot hold government fully accountable for individual dissenting views (must challenge the unified position). Coordination: shared interest in cabinet stability across partisan transitions.
constraint_indexing:constraint_classification(constitutional_conventions__collective_responsibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PARLIAMENT AS ASSEMBLY (PITON) — The constraint is largely performative from Parliament's institutional perspective. Collective responsibility prevents Parliament from interrogating dissenting cabinet views, but Parliament retains formal impeachment power (rarely used, historically deprecated). The constraint persists through constitutional custom and media enforcement rather than through Parliament's active gatekeeping. Theater ratio is high: Parliament's theoretical power to force individual ministerial accountability has atrophied while the performative unity presentation remains. The ritual of Prime Minister's Questions assumes unified cabinet; ministerial testimony assumes defense of collective positions. Piton classification: the performance (unified cabinet narrative) persists despite the functional accountability mechanism having degraded.
constraint_indexing:constraint_classification(constitutional_conventions__collective_responsibility, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, cabinet coherence might appear as an immutable feature of executive governance: any executive must present a unified face or collapse under internal contradiction. This perspective naturalizes the constraint as inherent to the very concept of cabinet government. The engine's false summit detector will flag this — the constraint has clear beneficiaries (PM, executive) and victims (dissenting ministers, transparency), indicating it is constructed rather than natural.
constraint_indexing:constraint_classification(constitutional_conventions__collective_responsibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_conventions__collective_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_conventions__collective_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_conventions__collective_responsibility, TR),
    TR >= 0.70.

:- end_tests(constitutional_conventions__collective_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts public dissent suppression from dissenting ministers and delivers unified narrative to the PM. The extraction is genuine and material (career cost of dissent is substantial), but the mechanism is not pure coercion — resignation is available and some ministers do resign. The value sits between pure Snare (ε ≥ 0.66) and pure Rope (ε ≤ 0.45) because the constraint serves both genuine coordination (cabinet government requires unity) and extraction (PM political power increases with visible unity). Suppression (0.68): High. The career cost of dissent is severe — resignation from cabinet typically ends political advancement. Alternative platforms for dissenting ministers are weak (backbench visibility is lower; media coverage of resigned ministers' positions is limited). Some dissent leaks (memoirs, media, interviews) but the primary suppression mechanism (public silence or resignation) is structurally enforced. Theater ratio (0.55): Moderate. Unified cabinet presentation serves genuine functional purposes (preventing daily media fragmentation, signaling coherence to foreign actors, maintaining party unity for legislative votes) but also contains significant performative elements (ministers defending positions they privately opposed, prime minister projecting harmony that may mask deep disagreement). The ratio has increased slightly over the interval as media fragmentation makes suppression harder to maintain but more necessary to avoid appearing chaotic.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival gap: Prime Minister sees Rope (pure coordination), Dissenting Minister sees Snare (pure extraction with high suppression), Opposition sees Tangled Rope (mixed coordination and extraction with future-norm effects), Parliament sees Piton (degraded accountability function), Backbench sees Tangled Rope (mixed constraint and benefit), and Analytical view risks false-summit Mountain (naturalizing contingent choice). The gap reveals that the same structural mechanism is experienced as functional coordination by beneficiaries and as extractive suppression by targets. The gap is not resolvable by better information — it is structural. The beneficiary genuinely experiences coordination (cabinet government cannot function without unity); the target genuinely experiences extraction (dissent suppression has real career costs). Both perspectives are accurate descriptions of the constraint's function from their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position: beneficiary vs. victim status, and exit cost. Prime Minister is beneficiary with high exit optionality (arbitrage) → d ≈ 0.05 → f(d) ≈ -0.12 (negative effective extraction, experiences constraint as subsidy). Dissenting Minister is victim with high exit cost (constrained → trapped if we model the career cost as near-insurmountable) → d ≈ 0.85-0.95 → f(d) ≈ 1.15-1.42 (high effective extraction). Opposition parties occupy ambiguous position (beneficiary when in government, victim when out; future orientation makes them partly beneficiary) → d ≈ 0.50-0.55. Parliament loses its interrogation function but retains formal supremacy (constrained institutional position) → d ≈ 0.60. The perspectival gap in d values produces the perspectival gap in classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that Tangled Rope is the canonical analytical classification (combining genuine coordination function with asymmetric extraction), while Snare, Rope, and Piton are legitimate perspectival readings from different structural positions. The mandatrophy resolves into: 'Which agent are you measuring from, and what is their structural relationship to the constraint?' The analytical observer (perspective 6) is asked whether the constraint is natural law or constructed — the presence of clear beneficiaries (PM, executive coherence) and victims (dissenting ministers, transparency) answers: constructed. FSM signature fires if beneficiaries are declared on a mountain; here beneficiaries are declared on tangled_rope (correct type), so no FSM gate is triggered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resigned_minister_visibility,
    'Do ministers who resign on principle gain sufficient platform to articulate their dissent, or does the constraint''s suppression persist even post-exit through media narrative control?',
    'Historical case analysis: compare media coverage and parliamentary visibility of resigned ministers'' dissenting positions vs. in-cabinet defenders of the same positions. Measure whether exit actually enables public expression of the suppressed view.',
    'If visibility is substantial post-exit: exit is a genuine high-cost option (constrained, not trapped) and chi is lower. If visibility is suppressed: exit does not enable suppressed dissent and the constraint is closer to trapped (d higher, chi higher, classification closer to Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resigned_minister_visibility, empirical, 'Whether resigned ministers retain platform for dissent after exit').

omega_variable(
    dissent_as_loyalty_signal,
    'Is ministerial dissent-suppression enforced primarily through explicit PM authority or through internalized party loyalty norms where ministers self-suppress to signal commitment?',
    'Qualitative analysis of ministerial memoirs and interviews; comparison of suppression in high-loyalty vs low-loyalty cabinets; observation of whether explicit PM warnings precede most resignations or whether ministers resign preemptively.',
    'If explicit enforcement: suppression is structural (remains at 0.68). If internalized: suppression may be identity-locked (ministers see unified position as expressing their true beliefs after internalization) and the exit_options should be identity_locked rather than constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissent_as_loyalty_signal, empirical, 'Mechanism of dissent suppression: explicit enforcement vs. internalized loyalty').

omega_variable(
    collective_responsibility_as_natural_law,
    'Is collective responsibility inherent to the very concept of cabinet government, or is it a contingent institutional choice adopted to solve a particular governance problem?',
    'Comparative institutional analysis: do cabinet systems in other democracies (Germany, Spain, Australia) enforce equivalent suppression? Historical analysis: did earlier British cabinets operate under different norms? Counterfactual: could cabinet government function without public dissent suppression?',
    'If inherent: mountain classification (perspective 6) is correct. If contingent: mountain is a false summit, engine reclassifies to tangled_rope or snare, FSM signature fires. The kernel contest itself implies contingency — reading this as contingent institutional choice rather than natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_responsibility_as_natural_law, conceptual, 'Whether collective responsibility is inherent law or contingent institutional choice').

omega_variable(
    private_dissent_legitimacy,
    'Is the right to private dissent a genuine concession (ministers may oppose privately but must defend publicly) or a performative fiction (private dissent is monitored and penalized if it enables resignations)?',
    'Documentary evidence: memoirs, leaks, interviews about whether private dissent is actually tolerated or whether dissenting ministers are marked for removal. Observation of pattern: do ministers who privately dissent tend to resign, be reshuffled, or be retained?',
    'If private dissent is genuinely tolerated: extractiveness is lower (0.50-0.55 range) and some genuine coordination function exists (Tangled Rope). If private dissent is fiction: extractiveness is higher (0.60-0.68) and the constraint is closer to pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_dissent_legitimacy, empirical, 'Whether private dissent is tolerated or monitored/penalized').

omega_variable(
    kernel_reading_contingency,
    'This constraint is ONE READING of the constitutional_conventions kernel. Other readings interpret the same underlying commitment (cabinet authority, ministerial responsibility) differently. Is collective responsibility THE necessary reading of cabinet government or ONE possible instantiation?',
    'Examine the sibling readings (ministerial_responsibility, royal_assent_convention, salisbury_convention). Each reading of the constitutional kernel grounds legitimacy in different claims about accountability and executive power. Collective responsibility emphasizes PM coherence; ministerial responsibility emphasizes individual accountability. The contest between readings is not resolvable empirically but rather reveals alternative committer positions.',
    'This omega documents the kernel-level contest. If collective responsibility reading is challenged by ministerial responsibility reading, the constraint''s classification depends on which reading''s authority is accepted. The engine uses cs_structure to model this: reading_relations declare how this reading stands relative to siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Kernel reading: collective responsibility vs. ministerial responsibility as competing readings of cabinet governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_conventions__collective_responsibility, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coll_resp_tr_t0, constitutional_conventions__collective_responsibility, theater_ratio, 0, 0.5).
narrative_ontology:measurement(coll_resp_tr_t20, constitutional_conventions__collective_responsibility, theater_ratio, 20, 0.52).
narrative_ontology:measurement(coll_resp_tr_t40, constitutional_conventions__collective_responsibility, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(coll_resp_be_t0, constitutional_conventions__collective_responsibility, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(coll_resp_be_t20, constitutional_conventions__collective_responsibility, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(coll_resp_be_t40, constitutional_conventions__collective_responsibility, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(coll_resp_su_t0, constitutional_conventions__collective_responsibility, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(coll_resp_su_t20, constitutional_conventions__collective_responsibility, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(coll_resp_su_t40, constitutional_conventions__collective_responsibility, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_conventions__collective_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_conventions__collective_responsibility, constitutional_conventions__ministerial_responsibility).
narrative_ontology:affects_constraint(constitutional_conventions__collective_responsibility, constitutional_conventions__royal_assent_convention).
narrative_ontology:affects_constraint(constitutional_conventions__collective_responsibility, constitutional_conventions__salisbury_convention).

% DUAL FORMULATION NOTE:
% Collective responsibility is ONE reading of the constitutional_conventions kernel. Each reading produces a distinct constraint with its own ε, beneficiary/victim structure, and classification. Ministerial responsibility reading has lower extractiveness (focuses on individual accountability rather than PM coherence). Royal assent has different suppression mechanisms (legal veto vs. social norm). Salisbury convention operates on different institutional actors (Lords vs. Cabinet). All four readings are linked through the shared kernel and affect each other's authority and institutional weight.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_conventions__collective_responsibility, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
