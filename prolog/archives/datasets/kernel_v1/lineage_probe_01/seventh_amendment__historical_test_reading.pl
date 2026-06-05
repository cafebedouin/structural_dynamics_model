% ============================================================================
% CONSTRAINT STORY: seventh_amendment__historical_test_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_seventh_amendment__historical_test_reading, []).

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
 *   constraint_id: seventh_amendment__historical_test_reading
 *   human_readable: Seventh Amendment: Historical Test Reading — 1791 Sorting Hat for Modern Claims
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The Seventh Amendment's jury guarantee applies to 'suits at common law.'
 *   For 235 years, federal courts have interpreted this to mean: if the claim
 *   would have existed and carried a jury right in 1791, the modern plaintiff
 *   gets a jury now. If the claim is an eighteenth-century equity, admiralty,
 *   or statutory innovation, no jury right. This reading creates a doctrinal
 *   sorting hat that maps every modern cause of action onto a historical
 *   template. A plaintiff suing for employment discrimination, securities
 *   fraud, intellectual property infringement, algorithmic discrimination, or
 *   data privacy violation must have the claim tested against what existed in
 *   1791. The doctrine suppresses jury rights for novel statutory claims by
 *   refusing to recognize them as functional analogues to historical
 *   common-law actions. The constraint exhibits tangled-rope structure:
 *   genuine coordination function (the historical test creates predictable,
 *   administrable categories for judges and defense counsel) coupled with
 *   asymmetric extraction (the beneficiary is the historical method and its
 *   institutional custodians; the victim is the novel claim that cannot
 *   squeeze into 1791 categories). The doctrine maintains itself through
 *   repeated application and doctrinal authority even as its functional
 *   utility declines. Suppression has increased over time (from 0.15 in 1791
 *   to 0.68 in 2010) as the gap between statutory modernity and historical
 *   baseline widened. Theater has increased (from 0.38 in 1980 to 0.58 in
 *   2026) as judges acknowledge the test's dysfunction while continuing to
 *   apply it. This reading is one of three competing interpretations of the
 *   Seventh Amendment kernel: the complexity exception reading (unresolved
 *   question of whether litigation can be too intricate for lay jurors), the
 *   reexamination clause reading (verdict protection from appellate
 *   second-guessing), and this reading (historical categorical sorting). All
 *   three remain live in contemporary doctrine.
 *
 * KEY AGENTS:
 *   - Plaintiff with Novel Statutory Claim: Powerless/trapped — claim cannot obtain jury right because no 1791 analogue exists; no structural exit from historical sorting hat
 *   - Modern Plaintiff's Bar (Statutory Expansion Coalition): Organized/constrained — pushing doctrinal boundaries through creative analogies and legislative override; constrained by persistent reversals and doctrinal resistance
 *   - Federal Judges and Defense Bar: Institutional/arbitrage — benefit from bright-line historical categorization; lower jury trial burden; arbitrage through complexity arguments and equity labeling
 *   - Doctrinal Interpretive Tradition: Institutional/analytical — maintains historical test through ritual application despite acknowledged dysfunction; carries itself forward through institutional inertia
 *   - Statutory Reframing Movement: Organized/constrained — creating sunset mechanism through statute-specific jury rights declarations; constrained by need to override doctrine one statute at a time
 *   - Analytical Observer (Natural Law Perspective): Analytical/analytical — risks naturalizing the historical baseline as constitutional immutability rather than recognizing interpretive choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(seventh_amendment__historical_test_reading, 0.52).
domain_priors:suppression_score(seventh_amendment__historical_test_reading, 0.68).
domain_priors:theater_ratio(seventh_amendment__historical_test_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(seventh_amendment__historical_test_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(seventh_amendment__historical_test_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(seventh_amendment__historical_test_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(seventh_amendment__historical_test_reading, tangled_rope).
narrative_ontology:human_readable(seventh_amendment__historical_test_reading, "Seventh Amendment: Historical Test Reading — 1791 Sorting Hat for Modern Claims").
narrative_ontology:topic_domain(seventh_amendment__historical_test_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(seventh_amendment__historical_test_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(seventh_amendment__historical_test_reading, 'eaa68d0a-9e6c-469d-8a78-2e653330b92c').
narrative_ontology:cs_kernel_codification('eaa68d0a-9e6c-469d-8a78-2e653330b92c', fixed_text).
narrative_ontology:cs_authority_grounding('eaa68d0a-9e6c-469d-8a78-2e653330b92c', lineage).
narrative_ontology:cs_interpretation_layer_present('eaa68d0a-9e6c-469d-8a78-2e653330b92c').
narrative_ontology:cs_reading_relation('eaa68d0a-9e6c-469d-8a78-2e653330b92c', seventh_amendment__complexity_exception_question, coexists_with).
narrative_ontology:cs_reading_relation('eaa68d0a-9e6c-469d-8a78-2e653330b92c', seventh_amendment__reexamination_clause_reading, influences).
narrative_ontology:cs_axiom('eaa68d0a-9e6c-469d-8a78-2e653330b92c', foundational, jury_right_historically_determined).
narrative_ontology:cs_axiom_status(jury_right_historically_determined, holdable).
narrative_ontology:cs_axiom_grounding('eaa68d0a-9e6c-469d-8a78-2e653330b92c', jury_right_historically_determined, conventional).
narrative_ontology:cs_axiom('eaa68d0a-9e6c-469d-8a78-2e653330b92c', foundational, id_1791_baseline_objective_measure).
narrative_ontology:cs_axiom_status(id_1791_baseline_objective_measure, holdable).
narrative_ontology:cs_axiom_grounding('eaa68d0a-9e6c-469d-8a78-2e653330b92c', id_1791_baseline_objective_measure, empirically_contingent).
narrative_ontology:cs_reference_frame('eaa68d0a-9e6c-469d-8a78-2e653330b92c', text_bound_historical_categorization).
narrative_ontology:cs_drift_state('eaa68d0a-9e6c-469d-8a78-2e653330b92c', modern_statutory_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eaa68d0a-9e6c-469d-8a78-2e653330b92c', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(seventh_amendment__historical_test_reading, seventh_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(seventh_amendment__historical_test_reading, historical_common_law_method).
narrative_ontology:constraint_beneficiary(seventh_amendment__historical_test_reading, traditional_tort_defense_bar).
narrative_ontology:constraint_victim(seventh_amendment__historical_test_reading, novel_statutory_claims).
narrative_ontology:constraint_victim(seventh_amendment__historical_test_reading, modern_regulatory_causes_of_action).
narrative_ontology:constraint_victim(seventh_amendment__historical_test_reading, cybersecurity_intellectual_property_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLAINTIFF WITH NOVEL STATUTORY CLAIM (SNARE) — A litigant suing under a statute enacted in 1965 (or 1995, or 2020) must have their claim sorted by an eighteenth-century test. No jury right because the claim has no precise eighteenth-century analogue. The plaintiff cannot restructure the claim or exit the jurisdiction; cannot obtain the fact-finder they would receive if their claim mapped to historical common law. The extraction is severe: the right is suppressed entirely, not merely constrained. Maximum experienced extraction from a powerless agent with no structural exit.
constraint_indexing:constraint_classification(seventh_amendment__historical_test_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERN PLAINTIFF'S BAR / STATUTORY EXPANSION COALITION (TANGLED ROPE) — Organized groups (consumer advocates, employment plaintiffs' counsel, statutory tort reformers) perceive the historical test as an extractive barrier that they are gradually coordinating to overcome. The barrier is real and suppressive (generates summary judgment dispositions); coordination exists (legislative expansion of statutory claims, pressure to recognize functional equivalences to historical forms). But the constraint also contains genuine coordination: the historical test creates predictable categorization. The exit option is constrained — the bar can file creative analogies and push appellate boundaries, but faces reversals and persistent doctrinal resistance. Neither pure extraction nor pure coordination.
constraint_indexing:constraint_classification(seventh_amendment__historical_test_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE BAR AND FEDERAL JUDICIARY (ROPE) — For judges and defense counsel, the historical test is a coordination mechanism. It creates bright-line categorization (common-law action = jury right; equity/admiralty analogue = no jury right). The test is functional: it delivers predictable outcomes, enables summary judgment, reduces jury trial burden on courts. These actors experience the constraint as pure coordination. Beneficiaries of the historical test: defense counsel (fewer jury trials = lower litigation exposure); federal judges (reduced jury trial docket). Exit options are arbitrage — they can migrate favorable claims toward equity labels or complexity arguments. Net experience: Rope.
constraint_indexing:constraint_classification(seventh_amendment__historical_test_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOCTRINAL INTERPRETIVE TRADITION (PITON) — From a civilizational standpoint, the historical test is increasingly theatrical. Judges regularly acknowledge that the test is poor policy, unworkable for modern statutory causes of action, and produces absurd outcomes (cybersecurity claims, algorithmic discrimination suits, data privacy torts all tested against 1791 precedent). Yet the interpretive tradition persists. Lower courts apply the test ritualistically; Supreme Court has not forcefully revised it; doctrinal scholars note the dysfunction while law schools teach the test as settled. Theater ratio high: the activity is mostly maintaining appearance of principled historical continuity while the functional categorization (jury trial availability) is driven by pragmatic appeals to complexity and manageability. The tradition carries itself forward through institutional inertia.
constraint_indexing:constraint_classification(seventh_amendment__historical_test_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: STATUTORY REFRAMING MOVEMENT (SCAFFOLD) — Organized reform efforts (statutory jury trial guarantees for specific causes of action, federal legislation explicitly providing jury rights in novel statutory claims) represent a sunset mechanism. Some statutes (labor law, consumer protection, patent law amendments) explicitly bypass the historical test by declaring jury rights. This creates a temporary scaffolding: legislators can override the historical test one statute at a time. The sunset logic: as more statutes explicitly declare jury rights, the historical test loses force through legislative override rather than judicial overruling. Low effective extraction because the organized movement has agency and a visible exit path.
constraint_indexing:constraint_classification(seventh_amendment__historical_test_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/analytical perspective, the historical test appears as an immutable constitutional constraint: the Seventh Amendment's text binds the courts to historical common-law categories. The amendment says 'suits at common law' shall preserve jury trials — this is interpreted as a fixed, natural constraint on judicial power. The judge has no discretion; the Constitution has spoken. The extraction and suppression appear not as policy choices but as inevitable consequences of the text's historical fixing. However, this perspective risks becoming a false summit: the identification of a 1791 baseline, the choice to exclude functional equivalences, and the decision to treat equity/admiralty as structurally different are all interpretive moves, not inevitable readings. The mountain naturalizes what is actually a contestable doctrinal choice.
constraint_indexing:constraint_classification(seventh_amendment__historical_test_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(seventh_amendment__historical_test_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(seventh_amendment__historical_test_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(seventh_amendment__historical_test_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(seventh_amendment__historical_test_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(seventh_amendment__historical_test_reading, TR),
    TR >= 0.70.

:- end_tests(seventh_amendment__historical_test_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint suppresses jury rights for novel statutory claims and confers advantages on defense counsel and judges through reduced jury trial docket. The extraction is not total — some claims receive creative historical analogies, and legislative override statutes bypass the test. But the baseline suppression is substantial and systemic. The extractiveness value reflects that the historical test genuinely does limit jury access for a growing class of statutory claims, while acknowledging that the constraint is partially permeable to doctrinal workarounds and legislative override. Suppression (0.68): High. The barrier to jury trial for novel claims is structural and sustained. Plaintiffs cannot restructure their statutory claim into a historical category; cannot exit the jurisdiction; cannot avoid the test through pleading; face consistent application across federal courts. The suppression is not absolute (legislative override exists; functional equivalences sometimes granted) but is formidable. Theater ratio (0.58): Moderate-high. The historical test is increasingly performative. Judges acknowledge its dysfunction in dicta while applying it mechanically. The test maintains appearance of principled constitutional fidelity while its actual work (managing jury trial docket, controlling verdict exposure) is driven by pragmatic considerations and complexity concerns that the test doesn't directly address. The theater has increased over time because the gap between 1791 and 2026 has widened, making the historical baseline increasingly anachronistic yet persistently invoked.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival differentiation. The powerless plaintiff sees a snare — the jury right is suppressed entirely, with no exit. The organized plaintiff's bar sees tangled rope — genuine coordination in the historical categories, but asymmetric extraction through suppression of novel claims. The defense bar and judiciary see rope — the historical test is a clean coordination mechanism that they benefit from. The doctrinal tradition sees a piton — the test persists through ritual application despite acknowledged dysfunction. The reform movement sees a scaffold — statutory overrides create a sunset mechanism. The natural law perspective risks seeing a mountain — the constitutional text binding the test as inevitable. The perspectival gap reveals that the constraint's classification depends entirely on the observer's structural position relative to the 1791 baseline and the modern statutory claim distinction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from the agent's structural relationship to the historical test. Plaintiffs with statutory claims are full targets (d ≈ 0.92) — they bear extraction and cannot exit. Judges and defense counsel are partial beneficiaries (d ≈ 0.10–0.25) — they benefit from the clean categorization and reduced jury trial burden. The plaintiff's bar occupies intermediate position (d ≈ 0.55) — they face suppression but have some organizational capacity to challenge and circumvent. The organized statutory reframing movement has modest arbitrage capacity (d ≈ 0.35) — they can achieve legislative override for specific statutes. The f(d) sigmoid maps these d values to experienced extractiveness, producing chi values that differentiate how severely each agent experiences the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this reading is one instantiation of a kernel contest. The 1791 historical test reading is distinct from the complexity exception reading (unresolved doctrinal pressure to exclude complex cases) and the reexamination clause reading (verdict protection from appellate reversal). Each reading addresses a different aspect of the Seventh Amendment and produces different classifications. The historical test reading's tangled-rope classification reflects its dual structure: genuine coordination in the bright-line categorization (the functional benefit to judges and defense counsel) coupled with asymmetric extraction (the suppression of jury rights for novel claims). This is not a mislabeling of coordination as extraction, nor extraction as coordination — it is an accurate diagnosis of hybrid constraint that provides coordination benefit to one set of actors while extracting from another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_equivalence_threshold,
    'At what level of functional similarity to a historical common-law action does a novel statutory claim qualify for jury protection under a modernized historical test?',
    'Comparative analysis: identify criteria courts use to classify borderline cases (employment discrimination vs. tort law analogy; securities fraud vs. common-law deceit). Survey state-court approaches to jury rights in modern statutory claims. Compare functional outcomes (bench vs. jury verdict rates) for historically mapped vs. unmap cases.',
    'If threshold is strict (very high similarity required): squeezes out most modern statutory claims, maintains high extraction. If threshold is loose (functional analogy sufficient): expands jury rights, reduces suppression, transforms constraint toward Rope. Classification depends on where the threshold is drawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_equivalence_threshold, conceptual, 'Functional equivalence threshold for novel statutory claims').

omega_variable(
    historical_baseline_accuracy,
    'Does the historical test accurately map 1791 common-law categories, or does it apply a retrospectively rationalized account of those categories filtered through modern doctrinal concerns?',
    'Deep historical analysis of 1791 pleading practice, equity/admiralty boundaries in colonial law, and how modern courts cite 1791 precedent. Examine whether courts cite actual 1791 sources or apply back-constructed categories invented post-hoc to reach preferred modern outcomes.',
    'If accurate: the test reflects genuine historical constraint (arguably mountain-like). If rationalized: the test is theatrical ritual disguised as historical fact — supports piton classification. If selectively cited: the test is manipulable doctrinal apparatus (supports tangled_rope or snare from victim perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_baseline_accuracy, empirical, 'Accuracy of the 1791 historical baseline').

omega_variable(
    reading_contest_relation_to_complexity_exception,
    'Is the historical test reading compatible with the complexity exception (unwritten doctrine that trials too complex for lay juries can be tried to judges), or does strict historical reading foreclose the exception?',
    'Analyze judicial opinions: do courts citing strict historical test also deploy complexity arguments? Is there a ceiling on complexity exception use as historical test becomes more rigorous? Historical test reading and complexity exception reading may be in tension (one reading forecloses the other) or coexist (different judges hold both). Survey appellate patterns 2010–2026.',
    'If foreclosed: reading_relations includes ''forecloses'' link to complexity_exception_question. If coexists: ''coexists_with''. If influences: complexity arguments succeed precisely where historical test allows them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_relation_to_complexity_exception, empirical, 'Logical relation between historical test and complexity exception').

omega_variable(
    reading_contest_relation_to_reexamination_clause,
    'Does prioritizing the historical test reading weaken or leave intact the reexamination clause protection? If jury trial is already suppressed by historical test, does the reexamination clause become merely a secondary safeguard on a dwindling right?',
    'Analyze appellate opinions: compare reexamination clause arguments in cases where jury trial was initially denied (historical test) vs. cases where jury trial was granted. Does reexamination clause work harder when jury trial access is broader? Is reexamination clause enforcement correlated with jury trial prevalence?',
    'If historical test suppression drains reexamination clause significance: historical test reading influences (weakens) reexamination clause reading. If reexamination clause remains robust independent of jury trial prevalence: readings coexist with separate work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_relation_to_reexamination_clause, empirical, 'Logical and functional relation between historical test and reexamination clause').

omega_variable(
    extractiveness_temporal_drift,
    'As the gap between 1791 and the present widens, and statutory causes of action proliferate, does the suppressiveness of the historical test increase over time, or do accommodations and functional equivalences stabilize it?',
    'Time-series analysis of jury trial rates in federal civil cases (1970–2026), categorized by claim type (historical common law, statutory, hybrid). Measure rate of decline in jury trials. Correlate with statutory proliferation. Identify inflection points where new statutes override historical test or new doctrinal accommodations emerge.',
    'If extractiveness rises: temporal measurements show suppression_requirement climbing — signals trajectory toward snare or constraint degradation. If stable: measurements plateau — supports current tangled_rope classification. If declining: measurements show suppression_requirement falling — supports scaffold reading (exit path opening).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_temporal_drift, empirical, 'Temporal drift in jury trial suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(seventh_amendment__historical_test_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1980, seventh_amendment__historical_test_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(theater_2000, seventh_amendment__historical_test_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement(theater_2026, seventh_amendment__historical_test_reading, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(extract_1800, seventh_amendment__historical_test_reading, base_extractiveness, 1800, 0.18).
narrative_ontology:measurement(extract_1950, seventh_amendment__historical_test_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(extract_2026, seventh_amendment__historical_test_reading, base_extractiveness, 2026, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1791, seventh_amendment__historical_test_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement(suppression_1938_frcp, seventh_amendment__historical_test_reading, suppression_requirement, 1938, 0.22).
narrative_ontology:measurement(suppression_1980, seventh_amendment__historical_test_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(suppression_2010, seventh_amendment__historical_test_reading, suppression_requirement, 2010, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(seventh_amendment__historical_test_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(seventh_amendment__historical_test_reading, seventh_amendment__complexity_exception_question).
narrative_ontology:affects_constraint(seventh_amendment__historical_test_reading, seventh_amendment__reexamination_clause_reading).

% DUAL FORMULATION NOTE:
% The Seventh Amendment kernel decomposes into three structurally distinct readings: (1) historical test (this file) — jury entitlement rationed by 1791 categories; (2) complexity exception — unresolved question of whether litigation too intricate for lay juries can be tried to judges; (3) reexamination clause — verdict protection from appellate second-guessing. Each reading has a different constraint_id and different epsilon value. They are siblings in the same kernel contest, not alternative measurements of one constraint. The readings compete in judicial doctrine: a judge may cite any of the three readings to reach the jury decision. All three affect each other through the doctrinal argument ecology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
