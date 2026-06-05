% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Clauses: Repudiation Reading (Duress Doctrine)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Treaty of Versailles (1919) presents a fundamental contestation over
 *   the legitimacy of a binding legal instrument. This constraint
 *   instantiates the REPUDIATION READING: the treaty was imposed under
 *   military duress (occupation, blockade, threat of invasion) and therefore
 *   creates no binding obligations on Germany. Under this reading, the
 *   treaty's reparations clauses, disarmament provisions, and territorial
 *   cessions are coercive extractions rather than legitimate settlements. The
 *   German state signed under duress and therefore owes no binding payment
 *   obligations — any payments are voluntary concessions, not legal duties.
 *   This reading directly forecloses the punitive-liability reading (which
 *   asserts the treaty legitimately punishes German aggression) and competes
 *   with the limited-responsibility reading (which accepts some obligations
 *   but at reduced scale). The repudiation reading maximizes the German
 *   state's structural freedom for non-compliance and creates legal
 *   justification for rearmament. Extractiveness (0.85) reflects that the
 *   treaty, under this reading, represents nearly complete extraction from
 *   the German state: territory, resources, sovereignty, and indefinite
 *   payment obligations, imposed without genuine consent. Suppression (0.78)
 *   reflects the military mechanisms (occupation, blockade) that made genuine
 *   negotiation impossible. The theater ratio (0.62) reflects the treaty's
 *   framing as a legitimate peace settlement through the League of Nations,
 *   masking the coercive mechanisms that produced it.
 *
 * KEY AGENTS:
 *   - German State: Primary victim (powerless/trapped) — faces military coercion; no exit from duress; all choice is illusory under threat
 *   - German Population: Secondary victim (powerless/trapped) — bears economic cost of reparations and disarmament; experiences intergenerational liability
 *   - Allied Powers Coalition: Institutional beneficiary (institutional/arbitrage) — extracts reparations, territorial gains, security guarantees through military victory; reframes extraction as legitimate compensation
 *   - German Political Elites: Organized actors (organized/constrained) — constrained by occupation but also by domestic politics; must choose between accepting humiliating treaty or continuing war
 *   - League of Nations Enforcement: Institutional actor (institutional/arbitrage) — supposed to legitimize the treaty through collective security, but enforcement capacity is performative
 *   - German Rearmament Movement: Powerful actor (powerful/mobile) — sees duress doctrine as justification for exit from treaty constraints; mobilizes opposition to reparations
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contested legal doctrine (duress) as universal law, missing the kernel reading contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.85).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.78).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Clauses: Repudiation Reading (Duress Doctrine)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, 'f4d8602f-c762-4e30-8653-ca7001de6c65').
narrative_ontology:cs_kernel_codification('f4d8602f-c762-4e30-8653-ca7001de6c65', fixed_text).
narrative_ontology:cs_authority_grounding('f4d8602f-c762-4e30-8653-ca7001de6c65', extraction).
narrative_ontology:cs_interpretation_layer_present('f4d8602f-c762-4e30-8653-ca7001de6c65').
narrative_ontology:cs_reading_relation('f4d8602f-c762-4e30-8653-ca7001de6c65', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('f4d8602f-c762-4e30-8653-ca7001de6c65', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('f4d8602f-c762-4e30-8653-ca7001de6c65', foundational, unilateral_duress_invalidates_treaties).
narrative_ontology:cs_axiom_status(unilateral_duress_invalidates_treaties, holdable).
narrative_ontology:cs_axiom_grounding('f4d8602f-c762-4e30-8653-ca7001de6c65', unilateral_duress_invalidates_treaties, deontological).
narrative_ontology:cs_axiom('f4d8602f-c762-4e30-8653-ca7001de6c65', secondary, military_blockade_negates_genuine_negotiation).
narrative_ontology:cs_axiom_status(military_blockade_negates_genuine_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('f4d8602f-c762-4e30-8653-ca7001de6c65', military_blockade_negates_genuine_negotiation, empirically_contingent).
narrative_ontology:cs_reference_frame('f4d8602f-c762-4e30-8653-ca7001de6c65', universal_contract_law_principle).
narrative_ontology:cs_drift_state('f4d8602f-c762-4e30-8653-ca7001de6c65', post_wwii_legal_consensus, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f4d8602f-c762-4e30-8653-ca7001de6c65', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GERMANY UNDER DURESS (SNARE) — The German state faces an impossible choice: sign the treaty under threat of continued military occupation and starvation blockade, or refuse and face invasion. No genuine alternatives exist. Trapped by military coercion, the state experiences maximum extraction: ceding territory, resources, sovereignty, and indefinite payment obligations. The suppression mechanism is military force; the extraction mechanism is the reparations clause that transfers indefinite wealth to creditor states. Exit is impossible.
constraint_indexing:constraint_classification(versailles_reparations_clauses__repudiation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GERMAN POLITICAL ELITES (TANGLED ROPE) — Constrained by military occupation and parliamentary politics, German elites see the treaty as embodying both coordination (mutual recognition of post-war borders and conflict resolution) and extraction (punitive reparations imposing indefinite liability). The treaty coordinates the end of war AND extracts indefinite wealth. Elites experience constraints from both directions: cannot escape without military defeat, but also cannot fully pay without economic collapse.
constraint_indexing:constraint_classification(versailles_reparations_clauses__repudiation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALLIED POWERS COALITION (ROPE) — From the institutional allied perspective, the treaty coordinates several functions: demilitarizes Germany, redistributes resources to compensate war losses, stabilizes European borders, and establishes collective security mechanisms. The reparations clause is framed as legitimate compensation rather than extraction. However, the repudiation reading directly challenges this framing — it asserts that the entire framework is coercive and therefore illegitimate.
constraint_indexing:constraint_classification(versailles_reparations_clauses__repudiation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GERMAN REARMAMENT AGENDA (SNARE) — A more powerful Germany with rearmament capacity sees the treaty as pure extraction preventing legitimate national defense. From this perspective, the suppression is the disarmament clause combined with reparations penalties, and the exit mechanism is military rearmament. This perspective is historically instantiated by the Nazi reading of the treaty — the repudiation reading creates structural space for maximalist exit strategies.
constraint_indexing:constraint_classification(versailles_reparations_clauses__repudiation_reading, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEAGUE OF NATIONS ENFORCEMENT (PITON) — The collective security mechanism enshrined in the League Covenant is largely theatrical: no enforcement authority exists beyond great-power collective will, which fragments immediately. The treaty's legitimacy mechanism (League enforcement of neutrality and collective security) is performative. The theater ratio is high because the League's enforcement capacity degrades rapidly after 1920, yet the treaty's legitimacy claim depends on League function.
constraint_indexing:constraint_classification(versailles_reparations_clauses__repudiation_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a natural-law sovereignty perspective, contracts signed under duress are categorically void — this is a universal principle of contract law. The treaty, signed under military threat and blockade, cannot create binding obligations. This appears as an immutable rule. However, the engine's false-summit detector will identify this as a claim that naturalizes a contested legal doctrine, not an actual law of nature or logic.
constraint_indexing:constraint_classification(versailles_reparations_clauses__repudiation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(versailles_reparations_clauses__repudiation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(versailles_reparations_clauses__repudiation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, TR),
    TR >= 0.70.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85): Very high. Under the repudiation reading, Germany experiences nearly complete extraction across multiple dimensions: territorial loss (Alsace-Lorraine, Polish corridor, colonial territory), economic extraction (reparations payments estimated at 132 billion gold marks — far exceeding Germany's annual GDP), military extraction (disarmament, war guilt admission), and political extraction (lost sovereignty over treaty terms). The extraction is comprehensive and indefinite. The baseline is high because the repudiation reading denies any legitimacy to the extraction — it is pure coercion, not a fair settlement. Suppression (0.78): High. The suppression mechanism is military: German delegates signed under threat of continued occupation and invasion. The Reparations Commission imposed payment schedules unilaterally. Suppression declined slightly over the interval (0.88 → 0.78) as the acute military occupation ended and Germany developed some capacity for non-compliance (through inflation and default), but suppression remained high because the treaty structure itself enforces compliance through the threat of reoccupation and asset seizure. Theater ratio (0.62): Moderate. The treaty is framed as a legitimate peace settlement negotiated through a peace conference and legitimized by the League of Nations — this is performative framing masking military coercion. However, the theater is not as high as a pure piton (where the constraint has no real function) — the treaty does coordinate post-war borders and establish collective security machinery, even if the coordination is asymmetric and extractive. The theater ratio increased slightly over the interval as the League's enforcement machinery proved defunct, revealing more of the treaty's coercive character beneath the legitimacy framing.
 *
 * PERSPECTIVAL GAP:
 *   The repudiation reading produces a maximal perspectival gap. From Germany's perspective (trapped, powerless), the treaty is pure snare — maximum extraction under military coercion with no exit. From the Allied perspective (institutional, arbitrage), the treaty is rope or tangled rope — legitimate coordination and compensation. From a rearmament perspective (powerful, mobile), the treaty is snare blocking legitimate national defense. From the League's enforcement perspective (institutional, arbitrage), the treaty should be rope (collective security) but proves to be piton (performative collective security without enforcement capacity). The analytical perspective risks seeing the duress doctrine as natural law (mountain), but this naturalizes a contested legal reading. The gap between the German trapped reading and the Allied institutional reading represents the fundamental kernel contest: is the treaty's legitimacy grounded in consent (which duress invalidates) or in victors' rights (which duress does not affect)?
 *
 * DIRECTIONALITY LOGIC:
 *   Germany (primary victim, trapped, powerless) derives d ≈ 0.95 from structural position: no exit options, no arbitrage, complete vulnerability to military coercion. The sigmoid f(d) ≈ 1.42 produces maximum experienced extractiveness chi. The Allied institutional perspective derives d ≈ 0.05 from beneficiary status and arbitrage exit: they can exit the reparations arrangement by negotiating Dawes or Young Plans. However, the repudiation reading inverts the entire directionality framing: the beneficiary (allies) are actually the coercive extractors, and their 'low d' misrepresents their structural role as powerful agents using military force. The repudiation reading therefore generates directionality_override entries that correct the automatic derivation to reflect the coercive power asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy at very high extractiveness (0.85) by anchoring to the kernel reading contest. The repudiation reading instantiates a complete rejection of binding obligations — mandatrophy is resolved by accepting that Germany has no duty to pay, hence the constraint can be classified as snare without requiring a beneficiary (the treaty produces no legitimate benefit, only coercive extraction). The mandatrophy is resolved through the legal doctrine: duress invalidates contracts, therefore reparations obligations are void. This is a preference-class resolution (it depends on accepting the duress doctrine), but once accepted, the classification becomes determinate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_doctrine_validity,
    'Does international law recognize duress as a grounds for invalidating treaties, and if so, what threshold of coercion meets the duress standard?',
    'Historical legal precedent analysis; examination of Vienna Convention on the Law of Treaties (Article 52) and pre-1919 state practice; philosophical debate over what constitutes ''duress'' vs legitimate use of military force in ending wars',
    'If duress doctrine applies to treaties signed under military occupation and blockade: repudiation reading''s premise (no binding obligation) is legally valid, and the constraint reclassifies from false summit (mountain) to genuine snare. If duress doctrine does not apply or requires higher threshold: the treaty remains binding, and Germany has legitimate (if unjust) payment obligations — reclassifies to tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duress_doctrine_validity, empirical, 'Whether duress doctrine applies to treaties signed under military occupation').

omega_variable(
    legitimacy_grounding_kernel_ambiguity,
    'Is the treaty''s legitimacy grounded in consent (duress invalidates it) or in victors'' rights under law of war (duress is irrelevant)?',
    'Examination of the Versailles treaty''s own framing and the allied powers'' post-hoc justifications; comparison to other post-war settlements (1815, 1871, 1945) and their legitimacy doctrines',
    'If consent-based: duress invalidates the treaty, extractiveness is maximized (0.85 justified). If victors''-rights-based: the treaty is binding regardless of duress, reframing extraction as legitimate punishment — reclassifies to tangled_rope or even rope. This is the fundamental kernel reading that separates repudiation from punitive-liability reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_grounding_kernel_ambiguity, conceptual, 'Whether treaty legitimacy is grounded in consent or in victors'' rights').

omega_variable(
    reparations_payment_capacity_threshold,
    'What payment rate constitutes economic viability for Germany, and does the treaty''s reparations schedule exceed that threshold?',
    'Economic historical analysis: German GDP, tax base, and capital stock 1919-1929; comparison to actual payment rates and capability; assessment of Dawes and Young Plan adjustments as evidence of original schedule infeasibility',
    'If original schedule is economically impossible: extraction mechanism is structural (Germany cannot escape through payment), confirming snare classification. If schedule is feasible: Germany''s non-payment becomes a choice rather than structural impossibility, weakening snare classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reparations_payment_capacity_threshold, empirical, 'Whether Versailles reparations schedule exceeds Germany''s economic capacity').

omega_variable(
    allied_unity_enforcement_capacity,
    'What was the allied powers'' actual collective capacity and willingness to enforce the treaty against German non-compliance?',
    'Historical analysis of League of Nations enforcement attempts, Ruhr occupation (1923), remilitarization response (1936), and broader great-power fragmentation post-1920',
    'If enforcement capacity is low and fragmented: suppression mechanism depends on Germany''s internalized legitimacy acceptance (cognitive capture) rather than external coercion, changing the suppression mechanism and possibly lowering suppression score. If enforcement capacity is credible: suppression is structural (threat of military retaliation), confirming high suppression (0.78).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_unity_enforcement_capacity, empirical, 'Allied enforcement capacity for Versailles treaty compliance').

omega_variable(
    kernel_reading_contest_structure,
    'Which kernel reading (repudiation, punitive_liability, limited_responsibility) represents the actual legal and political settlement that emerged post-1919?',
    'Comparative analysis of how the treaty was framed by signatories, how it was invoked in subsequent disputes, how Dawes and Young Plans reframed obligations, and which reading best predicts German political response',
    'If repudiation reading gains institutional acceptance: Germany''s rearmament and non-payment become legitimate defenses against illegitimate extraction, structurally enabling the rise of rearmament movement. If punitive reading prevails: Germany''s non-compliance becomes breach, justifying enforcement escalation. The kernel reading contest determines which political strategy (appeasement vs containment) becomes rational from each party''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Which Versailles reading (repudiation vs punitive vs limited) governs the actual settlement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vrc_repud_tr_t0, versailles_reparations_clauses__repudiation_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(vrc_repud_tr_t3, versailles_reparations_clauses__repudiation_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement(vrc_repud_tr_t6, versailles_reparations_clauses__repudiation_reading, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(vrc_repud_be_t0, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(vrc_repud_be_t3, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 3, 0.78).
narrative_ontology:measurement(vrc_repud_be_t6, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 6, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vrc_repud_su_t0, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(vrc_repud_su_t3, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 3, 0.82).
narrative_ontology:measurement(vrc_repud_su_t6, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, german_hyperinflation_monetary_policy).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, league_of_nations_collective_security).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, remilitarization_of_rhineland).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-constraint family. The kernel 'versailles_reparations_clauses' decomposes into three structurally distinct constraints, each with different ε values and classifications: repudiation_reading (ε=0.85, snare), punitive_liability_reading (ε=0.55, tangled_rope), limited_responsibility_reading (ε=0.35, tangled_rope). Each reading produces different empirical predictions about German behavior, Allied enforcement, and downstream institutional responses. The network links show how the readings contaminate each other: if the repudiation reading gains political traction, it delegitimizes the punitive reading's enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__repudiation_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
