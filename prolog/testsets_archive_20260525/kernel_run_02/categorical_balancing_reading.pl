% ============================================================================
% CONSTRAINT STORY: categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_categorical_balancing_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: categorical_balancing_reading
 *   human_readable: Categorical Balancing Reading of First Amendment Speech Protection
 *   domain: constitutional_law/speech_regulation
 *
 * SUMMARY:
 *   The categorical balancing reading of the First Amendment instantiates one
 *   normative approach to speech protection: the judiciary maintains defined
 *   categories of unprotected speech (obscenity, incitement, fighting words,
 *   true threats, commercial speech, etc.) and applies balancing tests within
 *   and across categories to determine protection levels. This reading is
 *   distinct from absolutism (no categories, near-total speech protection)
 *   and harm-limitation (balancing speech value against harm without fixing
 *   category memberships). The categorical approach claims to provide
 *   predictability, judicial manageability, and coherent doctrine, while
 *   critics argue it naturalizes judicial discretion and systematically
 *   disadvantages marginalized speakers whose speech falls into disfavored
 *   categories. As a constraint, categorical balancing exhibits tangled
 *   coordination-extraction structure: it does coordinate speech doctrine
 *   (provides some speakers with clear protection), but it extracts from
 *   those whose utterance falls outside protected categories. The
 *   constraint's extractiveness has risen over generational timescales as the
 *   judiciary has maintained categorical boundaries while speech practices
 *   have diversified. Theater ratio (0.68) reflects that categorical doctrine
 *   performs neutrality and predictability while actual decisions depend on
 *   fact-finding and judicial value judgment about category membership. This
 *   is a false-summit candidate: the analytical observer risks naturalizing
 *   categorical balancing as a necessary feature of any legal system, when
 *   comparative constitutional analysis reveals the categories are
 *   jurisdiction-specific and historically contingent.
 *
 * KEY AGENTS:
 *   - Institutional Judiciary (Majority Coalition): Primary beneficiary (institutional/arbitrage) — maintains interpretive control over category definitions and balancing tests
 *   - Disfavored Speaker: Primary victim (powerless/trapped) — falls within categorically-excluded speech; faces suppression with no exit mechanism
 *   - Speech-Protective Organizations: Secondary actor (moderate/constrained) — civil liberties groups benefit from having a legal framework to work within but bear costs of categorical exclusions
 *   - Marginalized Social Movements: Secondary actor (organized/constrained) — historically-marginalized groups whose speech falls into disfavored categories; have organizational capacity but systematic categorical disadvantage
 *   - Legal Predictability (Abstract Good): Degraded institutional goal (piton) — formal categories create appearance of predictability that masks ad hoc judicial reasoning
 *   - Lower Courts and Legal Bureaucracy: Institutional actor (institutional/constrained) — apply categorical doctrine performatively while following Supreme Court authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as necessary structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(categorical_balancing_reading, 0.58).
domain_priors:suppression_score(categorical_balancing_reading, 0.62).
domain_priors:theater_ratio(categorical_balancing_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(categorical_balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(categorical_balancing_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(categorical_balancing_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(categorical_balancing_reading, "Categorical Balancing Reading of First Amendment Speech Protection").
narrative_ontology:topic_domain(categorical_balancing_reading, "constitutional_law/speech_regulation").

domain_priors:requires_active_enforcement(categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(categorical_balancing_reading, formalized).
narrative_ontology:cs_authority_grounding(categorical_balancing_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(categorical_balancing_reading).
narrative_ontology:cs_kernel_id(categorical_balancing_reading, first_amendment_speech_protection).
narrative_ontology:cs_reading_relation(categorical_balancing_reading, absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation(categorical_balancing_reading, harm_limited_reading, influences).
narrative_ontology:cs_axiom(categorical_balancing_reading, foundational, judicial_categories_necessary_for_predictability).
narrative_ontology:cs_axiom_status(judicial_categories_necessary_for_predictability, holdable).
narrative_ontology:cs_axiom_grounding(categorical_balancing_reading, judicial_categories_necessary_for_predictability, instrumental).
narrative_ontology:cs_axiom(categorical_balancing_reading, foundational, judicial_integrity_requires_categorical_boundaries).
narrative_ontology:cs_axiom_status(judicial_integrity_requires_categorical_boundaries, holdable).
narrative_ontology:cs_axiom_grounding(categorical_balancing_reading, judicial_integrity_requires_categorical_boundaries, deontological).
narrative_ontology:cs_reference_frame(categorical_balancing_reading, categorical_exclusion_stability).
narrative_ontology:cs_drift_state(categorical_balancing_reading, contemporary_speech_diversity, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(categorical_balancing_reading, speech_categories_beneficiaries).
narrative_ontology:constraint_victim(categorical_balancing_reading, legal_predictability).
narrative_ontology:constraint_victim(categorical_balancing_reading, disfavored_categories).
narrative_ontology:constraint_victim(categorical_balancing_reading, excluded_speakers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(categorical_balancing_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(categorical_balancing_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(categorical_balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(categorical_balancing_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(categorical_balancing_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

constraint_indexing:constraint_classification(categorical_balancing_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(categorical_balancing_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(categorical_balancing_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(categorical_balancing_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(categorical_balancing_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(categorical_balancing_reading, TR),
    TR >= 0.70.

:- end_tests(categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The categorical balancing constraint extracts substantially from those whose speech falls into disfavored categories — they face criminalization or civil liability with no meaningful appeal once category status is fixed. The extraction is not at snare levels (0.66+) because the constraint does provide coordination benefits to some speakers and some speech remains protected. Theater ratio (0.68): High. The categorical framework performs predictability and neutrality, but actual decisions are contingent on fact-finding and judicial judgment about category membership (how does one determine whether speech constitutes 'incitement' or 'fighting words'?). The formalism of categorical doctrine obscures this contingency. Over the measurement interval (10 time units representing ≈50 years of doctrine), both extractiveness and theater have risen: categories have hardened as doctrine accumulated while speech practices diversified, and formal categorical reasoning has become more prominent in judicial opinions (possibly to mask ad hoc balancing). Suppression (0.62): Moderate-high. The constraint operates through category definition (judicial control of which utterances fall within categories), police enforcement (suppression of disfavored speech through arrest), and appellate review (courts upholding categorical determinations). Suppression is not total because some speech escapes categorization and appeals processes exist, but the categorical structure substantially limits alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The categorical balancing reading produces maximum perspectival divergence. From the judiciary's perspective (institutional/arbitrage), the constraint is coordination — it enables efficient doctrine-application and preserves interpretive authority. From the disfavored speaker's perspective (powerless/trapped), it is pure extraction — categorical membership determines suppression with no exit. From generational/organized perspectives, it is tangled rope — both the category system and the access to courts provide some coordination, while the categorical logic itself constrains options. The piton perspectives (legal predictability, lower courts) reveal that the constraint's performative aspects dominate its functional ones. The mountain perspective risks naturalizing the categorical structure as inevitable, when structural analysis shows it is maintained through active judicial enforcement and serves identifiable institutional interests. This perspectival range demonstrates why the categorical balancing reading requires explicit committer-frame specification: the constraint's identity is constituted through conflicting normative readings of the First Amendment's scope and purpose.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional judiciary (beneficiary, arbitrage exit) derives d ≈ 0.15–0.25: they benefit from the constraint and face no structural barriers to changing doctrine. Disfavored speakers (victim, trapped exit) derive d ≈ 0.90–0.98: they bear maximum extraction and cannot exit the categorical determination. Moderate actors like speech-protective organizations (constrained exit) derive d ≈ 0.65–0.75: they face institutional barriers to shifting doctrine but have resources and strategic options. Organized movements (constrained exit, mixed victim-beneficiary) derive d ≈ 0.62–0.72: they face suppression but also benefit from having a legal framework to litigate within. The perspectival gap emerges from these divergent directionalities: the judiciary experiences coordination (rope), disfavored speakers experience pure extraction (snare), and intermediate actors experience mixed structures (tangled rope). The piton perspectives derive from theater ratio (0.68) exceeding the piton gate (0.70 is the minimum for pure piton; this constraint is near the threshold, indicating performative institutional maintenance). The mountain perspective is a false summit: it naturalizes what the structural data shows is an institutional arrangement with identifiable beneficiaries and victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by acknowledging that categorical balancing is one normative reading of First Amendment speech protection, distinct from absolutism and harm-limitation. The constraint's structure reflects a specific institutional choice to maintain fixed categories of unprotected speech rather than (a) protecting all speech without category (absolutism) or (b) balancing speech against harms in each case without fixed categories (harm-limitation). The mandatrophy arises because each reading can claim legitimate constitutional grounding: categorical balancing claims predictability and manageability; absolutism claims fidelity to the Amendment's text; harm-limitation claims nuance and proportionality. The engine's mandatrophy resolution is perspectival: each perspective sees the constraint legitimacy differently. The judiciary sees legitimate coordination; disfavored speakers see illegitimate extraction. The resolution is not 'which reading is correct' but 'what are the structural trade-offs of adopting this reading rather than the alternatives.' The omega variables document these trade-offs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_balancing_ambiguity,
    'Is this constraint a system of fixed categorical exclusions, or a fact-dependent balancing test that uses category labels as heuristics?',
    'Empirical analysis of Supreme Court decisions: count how many cases flip categories based on factual context versus how many apply fixed category memberships. Assess whether the same conduct receives the same category label across cases with different factual configurations.',
    'If truly categorical (fixed memberships): constraint is a stable institutional rule, ε ≈ 0.45–0.55. If balancing-dependent (fluid categories): constraint is less predictable and more discretionary, potentially ε ≈ 0.62–0.72. The reading instantiates the categorical pole, but the ambiguity reveals whether the constraint''s actual operation matches its nominal structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_vs_balancing_ambiguity, empirical, 'Whether categorical exclusions are fixed or fluid through balancing.').

omega_variable(
    beneficiary_institutional_capture,
    'Does the judiciary maintain categorical balancing to preserve interpretive control over speech doctrine, or to maximize speech protection subject to unavoidable limits?',
    'Historical analysis of category evolution: track which categories have expanded (judiciary relaxing exclusions) versus contracted (judiciary tightening exclusions) over generational timescales. Identify whether category shifts respond to First Amendment values or to institutional power preservation.',
    'If institutional capture: constraint is primarily Snare/Tangled Rope from beneficiary perspective. If genuine speech-protective intent: constraint is Rope/Scaffold. The reading assumes partial capture, but the omega surfaces whether the constraint''s structure reflects genuine coordination failure or strategic exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_institutional_capture, conceptual, 'Whether judiciary maintains categorical balancing for speech protection or institutional control.').

omega_variable(
    falseness_of_mountain_classification,
    'Is the mountain classification (natural law view) a genuine characterization of structural necessity, or a naturalized institutional arrangement?',
    'Comparative constitutional analysis: examine whether other democracies achieve speech protection without categorical exclusions, or with different categorical boundaries. Assess whether the specific categories (obscenity, incitement, true threats) are culturally universal or jurisdiction-specific.',
    'If categories are contingent (jurisdiction-specific, historically variable): mountain classification is false summit. Categorical balancing becomes a constructed institutional choice, not natural law. This triggers false_summit_mountain signature in engine and elevates the constraint to snare or tangled_rope from beneficiary perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(falseness_of_mountain_classification, empirical, 'Whether categorical speech exclusions are natural law or constructed institutional arrangements.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'What distinguishes this categorical balancing reading from the absolutist and harm-limited readings? Which normative axioms are specific to categorical balancing, and which are shared across readings?',
    'Textual and doctrinal analysis: identify the minimum normative claim required to justify categorical balancing (distinct from absolutism and harm-limitation). Map how doctrine evolves when jurisdictions shift readings.',
    'If categorical balancing has no unique normative anchor: it collapses into harm-limited reading (both balance speech against harm; category is just a grouping mechanism). If it does have unique anchor: constraint''s identity is clear and the reading relations are well-specified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether categorical balancing is distinct normative reading or variant of harm-limitation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(categorical_balancing_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catbal_tr_t0, categorical_balancing_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(catbal_tr_t5, categorical_balancing_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(catbal_tr_t10, categorical_balancing_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(catbal_be_t0, categorical_balancing_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(catbal_be_t5, categorical_balancing_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(catbal_be_t10, categorical_balancing_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(categorical_balancing_reading, absolutist_reading).
narrative_ontology:affects_constraint(categorical_balancing_reading, harm_limited_reading).

% DUAL FORMULATION NOTE:
% The three First Amendment readings (categorical balancing, absolutist, harm-limited) form a kernel family where the categorical balancing reading is one instantiation of a shared kernel (the First Amendment text). The three readings have different ε values reflecting different structural relationships to judicial authority and beneficiary/victim alignment. Categorical balancing (this file, ε=0.58) is moderate extraction with institutional beneficiary; absolutist reading would have lower ε (less beneficiary extraction, more universal protection); harm-limited reading would have ε between these two. Each reading is a separate constraint story with its own perspectives and measurements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(categorical_balancing_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
