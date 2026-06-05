% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Persistence as Market-Equilibrium Coordination (Naturalization Reading)
 *   domain: economic_history/technology_standards/path_dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persists globally despite persistent claims
 *   that alternatives (Dvorak Simplified Keyboard, Colemak, etc.) are
 *   ergonomically or biomechanically superior. This constraint story
 *   instantiates ONE reading of a contested kernel: the naturalization
 *   reading. Under this reading, QWERTY persists not because of extraction by
 *   beneficiaries or because of path-dependent lock-in, but because it was
 *   adequate and became standard through fair competitive processes. The
 *   switching costs (human capital investment in muscle memory, coordination
 *   burden of changing standards) are real but represent genuine
 *   skin-in-the-game learning, not suppressed alternatives. From this
 *   perspective, alternatives like Dvorak failed to demonstrate sufficient
 *   empirical superiority to justify retraining costs. The constraint is a
 *   Rope (pure coordination) rather than Tangled Rope (coordination +
 *   extraction) or Snare (extraction with minimal coordination). The
 *   naturalization reading treats QWERTY as a stable market equilibrium
 *   outcome, not as a contingent institutional arrangement maintained through
 *   systematic suppression or as a path-dependent trap with a potentially
 *   superior alternative locked out.
 *
 * KEY AGENTS:
 *   - Typists: Moderate power, generationally constrained (skill investment lock). Benefit from QWERTY standardization through skill portability and employment flexibility. Bear switching costs if alternatives prove superior.
 *   - Keyboard manufacturers: Institutional power, globally arbitrageable. Benefit from QWERTY standardization through supplier networks and skill pools. No systematic motive to suppress alternatives — any manufacturer can adopt alternative layouts if market demand exists.
 *   - Dvorak advocates: Moderate power, locally identity-locked (early learning constitutive of professional identity). Perceive alternatives as suppressed by inertia; experience frustration at collective failure to adopt superior layouts (their empirical claim).
 *   - Analytical observer: Civilizational, universal scope. Assesses QWERTY persistence as market equilibrium outcome reflecting fair competition and adequate sufficiency of the standard.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.15).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Persistence as Market-Equilibrium Coordination (Naturalization Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '7d73d127-4b1d-4074-96e7-6959a8bb451c').
narrative_ontology:cs_kernel_codification('7d73d127-4b1d-4074-96e7-6959a8bb451c', fixed_text).
narrative_ontology:cs_authority_grounding('7d73d127-4b1d-4074-96e7-6959a8bb451c', practice).
narrative_ontology:cs_reading_relation('7d73d127-4b1d-4074-96e7-6959a8bb451c', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d73d127-4b1d-4074-96e7-6959a8bb451c', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('7d73d127-4b1d-4074-96e7-6959a8bb451c', foundational, dvorak_superiority_empirically_unestablished).
narrative_ontology:cs_axiom_status(dvorak_superiority_empirically_unestablished, holdable).
narrative_ontology:cs_axiom_grounding('7d73d127-4b1d-4074-96e7-6959a8bb451c', dvorak_superiority_empirically_unestablished, empirically_contingent).
narrative_ontology:cs_axiom('7d73d127-4b1d-4074-96e7-6959a8bb451c', foundational, qwerty_adequacy_sufficient_for_persistence).
narrative_ontology:cs_axiom_status(qwerty_adequacy_sufficient_for_persistence, holdable).
narrative_ontology:cs_axiom_grounding('7d73d127-4b1d-4074-96e7-6959a8bb451c', qwerty_adequacy_sufficient_for_persistence, instrumental).
narrative_ontology:cs_reference_frame('7d73d127-4b1d-4074-96e7-6959a8bb451c', fair_competition_equilibrium).
narrative_ontology:cs_drift_state('7d73d127-4b1d-4074-96e7-6959a8bb451c', contemporary_post_dvorak_studies, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7d73d127-4b1d-4074-96e7-6959a8bb451c', '2026-02-27T14:23:00Z').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TYPIST (ROPE) — QWERTY adoption represents genuine coordination on a standard that enabled skill portability and employment flexibility. Switching costs are real but reflect legitimate human capital investment in muscle memory and pattern recognition on QWERTY layouts. The constraint solves a genuine coordination problem (many-to-one skill mapping) with low coercive overhead. Classification: Rope. The typist benefits from standard stability; the arrangement benefits all parties who care about keyboard skill transferability.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__naturalization_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: MANUFACTURER (ROPE) — From the institutional perspective, QWERTY became the dominant standard through fair competitive process. Manufacturers that invested in QWERTY gained coordination benefits (suppliers, skill pools, aftermarket support), but these benefits are available to any competitor adopting the standard. No systematic extraction from manufacturers to users — both benefit from standardization. The arrangement is stable because the coordination benefit (universal skill) exceeds switching costs. Classification: Rope.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__naturalization_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — From the civilizational/analytical view, QWERTY persisted because it was adequate and alternatives (Dvorak, Colemak, etc.) failed to achieve sufficient superiority to overcome switching costs. The empirical claim: Dvorak's theoretical speed advantage over QWERTY was never conclusively demonstrated in field conditions with trained typists. The switching cost (retraining human muscle memory) was not justified by empirical performance gains. Under fair competition, QWERTY won. Classification: Rope. This reading treats the persistence as market equilibrium rather than lock-in or extraction.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__naturalization_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: DVORAK ADVOCATE (TANGLED ROPE) — From the advocate's perspective, alternative layouts offer genuine benefits (reduced finger travel, lower strain, ergonomic optimization) but are suppressed by coordination inertia and psychological identification with early-learned patterns. The advocate experiences this as coordinated extraction: their superior alternative is available but suppressed by collective action failure. However, this perspective requires accepting the empirical claim that Dvorak IS superior — a claim the naturalization reading explicitly contests. Classification: Tangled Rope, because the advocate sees both coordination (everyone learning Dvorak would solve the problem) and extraction (the collective choosing suboptimal outcome). The identity_locked exit reflects that early typing experience becomes constitutive of muscle memory and professional identity.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__naturalization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 5: NATURAL LAW VIEW (MOUNTAIN) — This reading risks naturalizing the market equilibrium outcome as immutable law: 'Under fair competition, the adequate standard wins out.' The false-summit detector will flag this if the naturalization reading declares beneficiaries or if the empirical claims prove contested. From a mathematical/economic theory perspective, market efficiency theorems suggest that in the long run, fair competition selects adequate or superior technologies. This perspective treats QWERTY persistence as a consequence of those theorems, not contingent institutional choices.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__naturalization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. Under the naturalization reading, QWERTY persistence reflects genuine adequacy rather than systematic extraction. The low value reflects that no identifiable beneficiary group maintains QWERTY through suppression of alternatives. Manufacturers compete on QWERTY because it is the standard, but any manufacturer can switch if alternatives prove superior. The switching cost (human capital retraining) is real but reflects the costs of coordination change, not coercive extraction. Suppression (0.12): Low. Barriers to Dvorak adoption are primarily coordination costs (retraining), not active suppression. No evidence in the naturalization reading of systematic marketing campaigns against alternatives or collusive manufacturer behavior. Theater ratio (0.25): Very low. QWERTY standardization is functional, not performative. The standard genuinely solves a coordination problem (many-to-one skill mapping). The arrangement is stable because it works, not because it is theatrical. Measurements show stable low theater and gradual rising extractiveness over the interval, reflecting that as QWERTY standardization deepened, some coordination overhead accumulated (more manufacturers, more rigidity in retraining), but without crossing into extraction or lock-in territory.
 *
 * PERSPECTIVAL GAP:
 *   The naturalization reading produces Rope classifications from all moderate-to-institutional perspectives, but the Dvorak advocate (Perspective 4) sees Tangled Rope because they accept the empirical claim that alternatives are superior. The analytical/natural-law perspective (Perspective 5) risks over-naturalizing the outcome as inevitable law rather than contingent market equilibrium. The perspectival gap is primarily about empirical disagreement (Is Dvorak actually superior?) rather than structural disagreement about the constraint's nature. If Dvorak's superiority is empirically established, the naturalization reading becomes false, and one of the sibling readings (lock-in or extraction) becomes more credible. If Dvorak's superiority is empirically contested or refuted, the naturalization reading stands.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the naturalization reading, no systematic beneficiary exists — both typists and manufacturers benefit from QWERTY standardization through reduced transaction costs and skill portability. The directionality (d) values are symmetric across beneficiary and victim groups (there are no victims under this reading). Both agents experience the constraint as coordination (low d → low f(d) → low χ). If evidence emerged of systematic suppression (manufacturer collusion, advertising campaigns against Dvorak, etc.), the directionality would shift — some agents would become clear targets, others clear beneficiaries, and d values would differentiate (some approaching 0.0 for beneficiaries, others approaching 1.0 for victims). The naturalization reading's empirical premise is that such evidence does not exist.
 *
 * MANDATROPHY ANALYSIS:
 *   The naturalization reading avoids mandatrophy by treating QWERTY as pure coordination (Rope) rather than attempting to classify it as both coordination and extraction simultaneously. The three sibling readings (extraction, lock-in, naturalization) represent genuine competing explanations, not different observational perspectives on a single constraint. The choice among them depends on empirical evidence about Dvorak's actual performance, historical records of manufacturer behavior, and switching cost magnitudes. The mandatrophy is resolved at the kernel level: the reading contest acknowledges that multiple theoretical framings are defensible and that empirical evidence will shift probability distributions across them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_empirical_superiority,
    'Is Dvorak objectively faster/more efficient than QWERTY for trained typists under real-world conditions, or is the empirical evidence inconclusive?',
    'Systematic meta-analysis of typing-speed studies comparing trained QWERTY and Dvorak typists under controlled conditions; longitudinal data on professional typists who switched to Dvorak (speed gain, error rates, long-term retention)',
    'If Dvorak is objectively superior: naturalization reading is empirically false; lock-in or extraction readings become more credible (ε rises). If evidence is inconclusive or negative: naturalization reading is supported (ε remains low); QWERTY persistence reflects fair competition outcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dvorak_empirical_superiority, empirical, 'Empirical status of Dvorak technical superiority claims').

omega_variable(
    switching_cost_magnitude,
    'What is the true switching cost (in training time, error rate, productivity loss) for a skilled QWERTY typist to learn Dvorak?',
    'Controlled retraining studies; longitudinal tracking of productivity during transition; comparison to other skill-transfer paradigms (ambidextrous handwriting, instrument switching in music)',
    'If switching cost >> Dvorak advantage: naturalization reading correct. If switching cost << Dvorak advantage: lock-in reading correct; inadequate justification exists for QWERTY persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_magnitude, empirical, 'Magnitude of learning costs for alternative layouts').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of a contested kernel: qwerty_persistence_mechanism. Which sibling reading (lock_in vs beneficiary_extraction) most plausibly describes QWERTY persistence?',
    'Historical analysis of QWERTY standardization (1870s-1920s): Did Sholes/Remington systematically suppress alternatives (extraction reading)? Or did QWERTY win fair competition but created coordination inertia (lock-in reading)? Or did QWERTY persist because it was adequate (naturalization reading)? The three readings coexist as live positions; empirical evidence on Dvorak performance and switching costs will shift probability distributions across them.',
    'Determines which constraint story is epistemically dominant. Naturalization reading ε=0.15 (Rope). Lock-in reading ε~0.40-0.50 (Tangled Rope / Snare). Extraction reading ε~0.50-0.70 (Tangled Rope / Snare). The three cannot coexist in a single explanatory framework; they represent genuine scholarly dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which theoretical reading of QWERTY persistence is correct').

omega_variable(
    beneficiary_invisibility,
    'Does the naturalization reading''s claim of ''no systematic beneficiary'' hold, or are there identifiable groups who benefited from QWERTY standardization and suppressed alternatives?',
    'Historical records of manufacturer marketing, advertising, and industry coordination (1900-1970s); archival evidence on Dvorak rejection by manufacturers and typing schools; longitudinal analysis of who profited from QWERTY standardization vs. who would have profited from alternatives',
    'If clear beneficiary groups identifiable: shifts reading toward extraction or lock-in (beneficiaries had motive and means to suppress alternatives). If no clear beneficiary: naturalization reading supported (persistence reflects genuine adequacy, not systematic extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_invisibility, empirical, 'Whether identifiable beneficiaries suppressed alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 1880, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_nat_theater_1880, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qwerty_nat_theater_1905, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(qwerty_nat_theater_1930, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(qwerty_nat_extract_1880, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qwerty_nat_extract_1905, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 25, 0.13).
narrative_ontology:measurement(qwerty_nat_extract_1930, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% QWERTY persistence is one kernel with three structurally distinct readings. This story (naturalization_reading, ε=0.15, Rope) treats QWERTY as market equilibrium and fair competition outcome. The lock_in_reading (ε~0.40-0.50, Tangled Rope) treats it as path-dependent coordination failure with a suppressed superior alternative. The extraction_reading (ε~0.50-0.70, Tangled Rope/Snare) treats it as beneficiary suppression. The three readings cannot coexist in a single constraint story; they represent fundamental disagreement about causal mechanisms, empirical superiority, and beneficiary intentionality. All three are linked via network.affects_constraints to document the kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
