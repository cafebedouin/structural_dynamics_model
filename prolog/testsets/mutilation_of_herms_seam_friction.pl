% ============================================================================
% CONSTRAINT STORY: mutilation_of_herms_seam_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mutilation_of_herms_seam_friction, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mutilation_of_herms_seam_friction
 *   human_readable: Mutilation of the Herms: Outer-Inner Bandwidth Mismatch in Religious-Political Friction
 *   domain: ancient_politics/religious_legal_friction
 *
 * SUMMARY:
 *   In 415 BCE, on the eve of the Sicilian Expedition, herms (sacred boundary
 *   stones) throughout Athens were mutilated overnight. The subsequent
 *   prosecutions, eventually targeting Alcibiades's circle, reveal a deep
 *   structural friction between two mismatched containers: the high-bandwidth
 *   outer container (the assembly and courts, capable of defining and
 *   prosecuting complex political-religious crimes) and the low-bandwidth
 *   inner container (religious practices with no formal amendment procedure
 *   to acknowledge kernel drift). Alcibiades's circle was operationally
 *   introducing philosophical reinterpretation and modified ritual practice
 *   that lacked any legitimate procedural channel in the outer container.
 *   Because the outer container had no procedure for acknowledging
 *   inner-kernel changes, all drift appeared as violation. The herms
 *   prosecutions processed these operational changes as crimes rather than as
 *   adaptations. The constraint operates at the seam where outer-container
 *   law meets inner-kernel reality: prosecution suppresses acknowledged drift
 *   by eliminating those most visibly conducting it, while the root problem
 *   (the missing amendment procedure) remains unaddressed. The constraint is
 *   tangled rope from the prosecutor's perspective (genuine religious defense
 *   + political extraction), snare from Alcibiades's circle (prosecuted for
 *   operational changes with no legitimate legitimization channel), rope from
 *   traditional religion (genuine ritual continuity defended), and piton from
 *   the perspective of religious law formalism (theater replacing function as
 *   kernel drift continues).
 *
 * KEY AGENTS:
 *   - Alcibiades's Circle: Primary victim (powerful/constrained) — introducing religious kernel changes operationally; prosecuted under violation law with no amendment channel
 *   - Prosecutorial Faction: Primary beneficiary (institutional/arbitrage) — coordinates religious defense while extracting political advantage; can redirect prosecution as political value shifts
 *   - Traditional Religious Establishment: Secondary beneficiary (institutional/mobile) — genuine interest in ritual continuity; no extraction suffered, mobile exit
 *   - Athenian Assembly: Conflicted institutional actor (powerful/mobile) — coordinates collective piety through trial ritual; mechanism suppresses acknowledgment of drift by processing only as crime
 *   - Procedural Legitimacy Channel: Reified victim (powerless/trapped) — the missing formal mechanism for amending religious practices; cannot be created without exposing the outer-inner conflict
 *   - Religious Law Formalism: Institutional mechanism (institutional/arbitrage) — operates as performative theater; theater increases as kernel drift continues unaddressed
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing structural bandwidth mismatch as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mutilation_of_herms_seam_friction, 0.58).
domain_priors:suppression_score(mutilation_of_herms_seam_friction, 0.72).
domain_priors:theater_ratio(mutilation_of_herms_seam_friction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mutilation_of_herms_seam_friction, extractiveness, 0.58).
narrative_ontology:constraint_metric(mutilation_of_herms_seam_friction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(mutilation_of_herms_seam_friction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mutilation_of_herms_seam_friction, tangled_rope).
narrative_ontology:human_readable(mutilation_of_herms_seam_friction, "Mutilation of the Herms: Outer-Inner Bandwidth Mismatch in Religious-Political Friction").
narrative_ontology:topic_domain(mutilation_of_herms_seam_friction, "ancient_politics/religious_legal_friction").

domain_priors:requires_active_enforcement(mutilation_of_herms_seam_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mutilation_of_herms_seam_friction, prosecutorial_faction).
narrative_ontology:constraint_beneficiary(mutilation_of_herms_seam_friction, traditional_religious_establishment).
narrative_ontology:constraint_victim(mutilation_of_herms_seam_friction, alcibiades_circle).
narrative_ontology:constraint_victim(mutilation_of_herms_seam_friction, procedural_legitimacy_channel).
narrative_ontology:constraint_victim(mutilation_of_herms_seam_friction, religious_kernel_adaptation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALCIBIADES'S CIRCLE (SNARE) — Operationally introducing religious kernel changes (philosophical reinterpretation, modified ritual practice) that lack formal amendment procedures in the outer container. The practices themselves are not crime; the constraint is that legitimate procedural bandwidth does not exist to acknowledge inner-kernel drift. Prosecuted under religious crime statutes designed to detect violation, not adaptation. Exit is constrained by social status and political vulnerability, but the core trap is structural: no mechanism exists to legitimize the changes they are operationally introducing. High suppression (execution risk, exile, property confiscation) with no procedural escape hatch.
constraint_indexing:constraint_classification(mutilation_of_herms_seam_friction, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: PROSECUTORIAL FACTION (TANGLED ROPE) — Coordinates genuine religious defense (maintains external appearance of ritual order, deters actual desecration, protects collective piety). Simultaneously extracts political advantage: the herms prosecution neutralizes a rival faction (Alcibiades's supporters) before the Sicilian Expedition and consolidates power by appearing as guardian of tradition. Both functions are real — the constraint genuinely coordinates religious order AND asymmetrically extracts political power. Active enforcement required (trials, testimony, verdicts). Beneficiary with arbitrage exit (faction leaders can shift focus, redefine targets, or wind down prosecution as political value changes).
constraint_indexing:constraint_classification(mutilation_of_herms_seam_friction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: TRADITIONAL RELIGIOUS ESTABLISHMENT (ROPE) — Genuine coordination function: maintains ritual continuity, defends sacred boundaries, sustains piety that underpins social cohesion. The herms prosecution coordinates real religious defense — prevents normalization of desecration and reinforces that sacred objects have binding force. No extraction from this perspective — the establishment benefits from ritual continuity and sees no asymmetric cost imposed by the prosecution. Mobile exit (temples, priesthoods, and religious practices persist across political regimes), so the constraint does not suppress the establishment's core function.
constraint_indexing:constraint_classification(mutilation_of_herms_seam_friction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: ATHENIAN ASSEMBLY (TANGLED ROPE) — Coordinates collective defense of shared sacred order through the trial process itself — assembly members deliberate, vote, and reaffirm collective commitment to piety. Simultaneously extracts political constraint on rivals: the prosecution mechanism is designed to process only violations or crimes, not procedural amendments or kernel drift. The outer container's inability to formally recognize inner-kernel changes means all drift appears as crime. This is not a bug in the assembly's design — it is the mechanism through which the outer container suppresses acknowledged drift. High suppression because formal amendment channels would require explicit constitutional change (very high-bandwidth procedure that would expose the conflict). Theater ratio moderately high: the trial ritual reasserts piety but cannot address the root kernel-drift problem.
constraint_indexing:constraint_classification(mutilation_of_herms_seam_friction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: PROCEDURAL LEGITIMACY CHANNEL (SNARE) — The absent mechanism for formally acknowledging inner-kernel religious changes. This perspective reifies the structural gap: no assembly procedure exists to recognize that religious practice evolves or that philosophical reinterpretation is not inherently criminal. The constraint extracts from this potential mechanism by suppressing its creation. Low-bandwidth formal procedures (sacrifice protocol, initiation requirements, boundary definitions) persist unchanged while high-bandwidth operational drift occurs, creating the seam where outer-container law meets inner-kernel reality. The missing mechanism is the victim: it cannot exit, cannot organize, cannot be amended because its non-existence IS the suppression mechanism.
constraint_indexing:constraint_classification(mutilation_of_herms_seam_friction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: RELIGIOUS LAW FORMALISM (PITON) — The constraint operates largely through ritualistic reaffirmation of piety rather than through functional religious renewal. The trial process is theater: testimony about blasphemy, oaths sworn before gods, collective votes to defend sacred order. The underlying kernel-drift problem (how should religious practice evolve?) is not addressed by the legal mechanism — the law processes only violations, not adaptations. Theater ratio 0.68 reflects that the herms prosecution ritual reasserts collective piety but cannot solve the seam-friction problem. The constraint persists through institutional inertia (law-courts are the established mechanism for religious matters) rather than through true coordination function. As kernel drift continues, the outer container's theater increases — more prosecutions, more elaborate oaths, more performative piety, while the underlying problem (legitimate amendment procedures) remains unaddressed.
constraint_indexing:constraint_classification(mutilation_of_herms_seam_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the seam-friction between low-bandwidth inner containers (religious practices) and high-bandwidth outer containers (legal procedure) is an immutable structural feature of how integrated systems handle internal change. All systems with layered governance face kernel drift: when inner-layer changes lack formal amendment channels in outer-layer procedure, the outer layer processes drift as crime. This appears as a natural law of organizational structure. However, this perspective risks naturalizing a contingent institutional arrangement. The constraint could be eliminated by adding formal amendment procedures to the outer container — a high-bandwidth mechanism explicitly designed to acknowledge and legitimize inner-kernel changes. The engine will flag this as a false summit: the 'natural law' cover story masks the prosecutorial faction's interest in suppressing acknowledged drift.
constraint_indexing:constraint_classification(mutilation_of_herms_seam_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mutilation_of_herms_seam_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mutilation_of_herms_seam_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mutilation_of_herms_seam_friction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mutilation_of_herms_seam_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mutilation_of_herms_seam_friction, TR),
    TR >= 0.70.

:- end_tests(mutilation_of_herms_seam_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The prosecutorial faction extracts political power through the herms trials — eliminating rivals, consolidating faction, redistributing confiscated property. But the extraction is not maximal because genuine religious coordination is also occurring: the outer container does defend ritual order and deter actual desecration. The measurement shows extractiveness rising from 0.35 (pre-crisis baseline, when kernel drift was not yet forcibly prosecuted) to 0.58 (post-herms crisis, when prosecution mechanism fully activated). Suppression (0.72): High. Multiple barriers prevent Alcibiades's circle from addressing the root problem: (1) no formal amendment procedure exists, so acknowledging kernel changes would require visible deviation from procedure (high-bandwidth constitutional change), (2) prosecution suppresses alternative voices through exile and property confiscation, (3) the outer container can process only violation or compliance, not adaptation. Theater ratio (0.68): Moderately high. The trial ritual — testimony about blasphemy, oaths of innocence, assemblies voting to reaffirm piety — is substantially performative. It reasserts collective religious commitment but does not address the seam-friction problem. The theater increases over the interval as prosecutions continue without resolving the underlying kernel-drift issue. By time_point 2, theater reaches 0.71, indicating the constraint has shifted toward degradation: prosecution continues despite root cause remaining unaddressed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates that outer-container prosecution can appear as either rope (pure coordination of religious defense) or tangled rope (coordination + political extraction) depending on the observer's proximity to the prosecution mechanism. To traditional religion, it is rope — genuine piety defense, no extraction from religious perspective. To prosecutors, it is tangled rope — genuinely coordinating religious order while extracting political advantage. To Alcibiades's circle, it is snare — prosecuted for changes that have no procedural legitimization channel. The gap reveals that the classification is not intrinsic to 'the herms prosecution' as an abstract event, but depends fundamentally on whether the observer benefits from the outer container's processing of inner-kernel drift as crime. The prosecutorial faction benefits (political advantage); traditional religion benefits (continuity defended); Alcibiades's circle loses (prosecuted without legitimate alternative channel); the missing amendment procedure loses (its non-existence is suppressed). These four perspectives generate four different classifications from identical structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value derives from their structural position relative to the extraction flow. Prosecutorial faction (institutional/arbitrage) gets low d ~0.15 (beneficiary with mobile exit); Alcibiades's circle (powerful/constrained) gets high d ~0.75 (victim with limited exit due to prosecution pressure, though not structurally trapped in the full sense); traditional religion (institutional/mobile) gets d ~0.35 (partial beneficiary, mobile exit, no asymmetric cost); assembly (powerful/mobile) gets d ~0.50 (symmetric position — both defends piety and extracts political advantage). The prosecutors' arbitrage exit is key: they can redefine the target (pivot from Alcibiades to others), scale the prosecution (intensive or mild enforcement), or wind down (redirect attention). This exit option differs fundamentally from Alcibiades's constrained position (prosecution targets him by status and association, not by conduct of his own choosing). The gap in exit options produces the directional asymmetry: extraction flows toward the less-mobile agent (Alcibiades's circle).
 *
 * MANDATROPHY ANALYSIS:
 *   SEAM-FRICTION EXEMPLAR: The mandatrophy is resolved by recognizing that this constraint does not classify as a single type but as a bandwidth-mismatch interface. The outer container (assembly law) and inner container (religious practice) have mismatched amendment bandwidth — the outer can process violation/compliance but not adaptation/drift. The constraint manifests as tangled rope from the prosecutor's and assembly's perspective (genuine religious coordination + political extraction) because the outer container generates both functions simultaneously through the same mechanism (trial prosecution). It manifests as snare from Alcibiades's circle's perspective because the inner-container drift they are operationally conducting lacks any formal procedural channel for legitimate amendment in the outer container. The mountain perspective (natural law) is a false summit: the constraint is not an immutable feature of how systems handle change, but a specific institutional choice to process drift as crime rather than adaptation. The constraint could be eliminated or fundamentally altered by adding a formal amendment procedure (high-bandwidth mechanism in the outer container explicitly designed to acknowledge and legitimize inner-kernel changes). The prosecutorial faction's interest in suppressing acknowledged drift is what sustains the constraint, not any natural law. The theater increase over the interval (0.68 → 0.71) signals that the outer container is becoming increasingly performative — prosecution continues despite the root problem remaining unaddressed. This is a diagnostic signature of a constraint approaching piton degradation: the mechanism persists through inertia and theater while its stated coordination function (religious defense) becomes less achievable through prosecution alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_vs_operational_drift,
    'Did Alcibiades''s circle deliberately introduce religious kernel changes, or were changes operationally drifting without coordinated intentional departure from tradition?',
    'Textual analysis of contemporary accounts (Thucydides, Xenophon, Andocides); reconstruction of actual religious practices attributed to the circle; comparison with documented traditional practice',
    'If intentional: the constraint is extraction with knowledge (Alcibiades deliberately circumvented procedures). If drift: the constraint is snare created by bandwidth mismatch (changes occurred without procedural acknowledgment). If mixed: Alcibiades operationalized drift others initiated (tangled responsibility structure).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intentionality_vs_operational_drift, empirical, 'Whether religious changes were deliberate innovation or operational drift').

omega_variable(
    amendment_procedure_feasibility,
    'Did Athens possess any formal procedure by which inner-container religious practices could be legitimately amended, or was outer-container law structured to prevent such acknowledgment?',
    'Historical analysis of Athenian constitutional procedures for religious reform; examination of prior religious amendments and how they were legitimated; comparison with democratic procedures for other types of change',
    'If amendment procedures existed but were bypassed: Alcibiades''s circle chose to avoid legitimate channels (extraction becomes more severe). If no procedures existed: the constraint is purely structural bandwidth mismatch (snare classification strengthened). If procedures required prohibitively high quorum: outer container was designed to suppress acknowledged drift (extraction mechanism confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_procedure_feasibility, empirical, 'Whether Athens possessed legitimate procedures for religious kernel amendment').

omega_variable(
    prosecution_precision_vs_factional_targeting,
    'Did the herms prosecution genuinely target religious crime, or was the religious offense the pretext for eliminating political rivals?',
    'Comparison of prosecution scope (total number accused, geographic spread, social status of accused) with severity of herms damage and actual participation; analysis of whether similar religious offenses by other factions were prosecuted at comparable intensity; examination of immediate political consequences (exile, property confiscation, redistribution)',
    'If genuine religious prosecution: extraction is coordination cost (snare classification shifts toward rope). If factional targeting: extraction is maximal (snare confirmed, prosecutor perspective extraction increases). If mixed: the constraint extracts political power through the mechanism of religious law (tangled_rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prosecution_precision_vs_factional_targeting, empirical, 'Whether prosecution targeted religious crime or used religion as pretext for political elimination').

omega_variable(
    kernel_drift_inevitability,
    'Is religious kernel drift in large, diverse city-states inevitable as population composition and philosophical sophistication change, or can outer-container law prevent drift through suppression?',
    'Long-term historical analysis: do suppressionist periods (high prosecution, strict enforcement) delay religious drift or merely mask it? Do cultures with formal amendment procedures handle drift more smoothly? Can populations with low bandwidth for acknowledged change return to uniform practice?',
    'If drift is inevitable: the constraint solves nothing, merely extracts through the prosecution mechanism (snare from analytical view). If suppression can delay drift: the constraint has genuine coordination function (tangled_rope confirmed). If formal amendment procedures eliminate conflict: the outer-inner bandwidth mismatch is the true constraint, not the prosecution itself (reframe constraint to focus on missing procedures, not on trial outcome).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_drift_inevitability, conceptual, 'Whether religious kernel drift is inevitable or suppressible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mutilation_of_herms_seam_friction, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herms_tr_t0, mutilation_of_herms_seam_friction, theater_ratio, 0, 0.48).
narrative_ontology:measurement(herms_tr_t1, mutilation_of_herms_seam_friction, theater_ratio, 1, 0.68).
narrative_ontology:measurement(herms_tr_t2, mutilation_of_herms_seam_friction, theater_ratio, 2, 0.71).

% Extraction over time
narrative_ontology:measurement(herms_be_t0, mutilation_of_herms_seam_friction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(herms_be_t1, mutilation_of_herms_seam_friction, base_extractiveness, 1, 0.58).
narrative_ontology:measurement(herms_be_t2, mutilation_of_herms_seam_friction, base_extractiveness, 2, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mutilation_of_herms_seam_friction, identity_coordination).
narrative_ontology:affects_constraint(mutilation_of_herms_seam_friction, athenian_religious_innovation_suppression).
narrative_ontology:affects_constraint(mutilation_of_herms_seam_friction, philosophical_drift_legal_prosecution).
narrative_ontology:affects_constraint(mutilation_of_herms_seam_friction, democratic_procedural_bandwidth_limits).

% DUAL FORMULATION NOTE:
% The herms mutilation constraint decomposes into three structurally distinct constraints with different ε values: (1) the outer-inner bandwidth mismatch (ε≈0.58, tangled rope, focuses on seam friction), (2) the herms desecration crime itself (ε≈0.72, snare, focuses on violation severity), and (3) the prosecution mechanism (ε≈0.45, tangled rope, focuses on political extraction through religious law). This story models the bandwidth-mismatch seam. The companion story on the herms desecration would focus on violation severity and temple profanation as crimes. The prosecution-mechanism story would focus on how trials become vehicles for political elimination. All three are linked: the desecration crime triggers the prosecution mechanism, which operates through the outer-container law's inability to process inner-kernel drift as anything other than violation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
