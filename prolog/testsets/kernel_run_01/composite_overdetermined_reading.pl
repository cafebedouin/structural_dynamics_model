% ============================================================================
% CONSTRAINT STORY: composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_composite_overdetermined_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: composite_overdetermined_reading
 *   human_readable: Composite Overdetermined Decline: Legal Suppression and Honor Code Transformation in Dueling Systems
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   The composite overdetermined reading models dueling's decline across
 *   European aristocracies (17th–19th centuries) as a tangled interaction of
 *   two non-independent causal pathways: (1) legal suppression — states
 *   criminalized dueling, enforced prohibitions through execution/exile; (2)
 *   endogenous honor-code delegitimation — younger aristocratic generations
 *   increasingly questioned whether honor genuinely required lethal risk,
 *   whether reputation satisfaction required death. These mechanisms are not
 *   additive; they are causally entangled. Legal suppression accelerated
 *   delegitimation (if the law forbids dueling, perhaps dueling isn't
 *   essential to honor), while delegitimation made legal suppression
 *   politically feasible (public opinion shifted, enforcement became easier).
 *   The constraint exhibits both rope-breaking (legal pressure severing the
 *   coordination mechanism) and mountain erosion (the honor substrate's
 *   intrinsic legitimacy weakening). No single mechanism explains the full
 *   decline.
 *
 * KEY AGENTS:
 *   - Aristocratic duellists: Primary victims (powerless/trapped) — facing dual impossibility of honor code demands vs. legal prohibition, with no exit available
 *   - Honor-code tradition (collective): Secondary victim (organized/constrained) — coordination system undermined by both external suppression and internal delegitimation
 *   - Centralizing state: Primary beneficiary (institutional/arbitrage) — consolidates monopoly on legitimate violence; suppression strengthens state control
 *   - Bourgeois commercial order: Primary beneficiary (institutional/arbitrage) — dueling suppression removes unpredictable elite withdrawals, enables reliable contracting
 *   - Reformist intellectuals: Secondary actor (powerful/mobile) — advocate for honor-code replacement with rationalist alternatives; see suppression as temporary catalyst
 *   - Nostalgic revivalists: Degraded system maintainers (institutional/arbitrage) — attempt to preserve honor through ritualized substitutes; perceive system as already hollow
 *   - Analytical observer: Civilizational risk (analytical/analytical) — risks naturalizing a contingent state strategy as inherent feature of aristocratic systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(composite_overdetermined_reading, 0.58).
domain_priors:suppression_score(composite_overdetermined_reading, 0.72).
domain_priors:theater_ratio(composite_overdetermined_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(composite_overdetermined_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(composite_overdetermined_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(composite_overdetermined_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(composite_overdetermined_reading, "Composite Overdetermined Decline: Legal Suppression and Honor Code Transformation in Dueling Systems").
narrative_ontology:topic_domain(composite_overdetermined_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(composite_overdetermined_reading, distributed).
narrative_ontology:cs_authority_grounding(composite_overdetermined_reading, practice).
narrative_ontology:cs_kernel_id(composite_overdetermined_reading, honor_satisfaction_substrate).
narrative_ontology:cs_reading_relation(composite_overdetermined_reading, practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation(composite_overdetermined_reading, cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom(composite_overdetermined_reading, foundational, causal_entanglement_irreducibility).
narrative_ontology:cs_axiom_status(causal_entanglement_irreducibility, holdable).
narrative_ontology:cs_axiom(composite_overdetermined_reading, secondary, bidirectional_reinforcement_mechanism).
narrative_ontology:cs_axiom_status(bidirectional_reinforcement_mechanism, holdable).
narrative_ontology:cs_reference_frame(composite_overdetermined_reading, honor_substrate_functional_legitimacy).
narrative_ontology:cs_drift_state(composite_overdetermined_reading, post_enlightenment_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(composite_overdetermined_reading, centralizing_state).
narrative_ontology:constraint_beneficiary(composite_overdetermined_reading, bourgeois_commercial_order).
narrative_ontology:constraint_victim(composite_overdetermined_reading, aristocratic_honor_substrate).
narrative_ontology:constraint_victim(composite_overdetermined_reading, dueling_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARISTOCRATIC DUELLIST (SNARE) — Trapped by dual impossibility: honor code demands satisfaction for insult, yet law forbids dueling under pain of execution or exile. No exit available. The constraint extracts the duellist's agency: obey honor and die or flee; obey law and lose social standing. Suppression is total — both paths are catastrophic. Maximum experienced extraction.
constraint_indexing:constraint_classification(composite_overdetermined_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HONOR-CODE TRADITION AS COLLECTIVE AGENT (TANGLED ROPE) — The honor system coordinates social recognition and status differentiation among aristocrats (genuine coordination function). But it simultaneously extracts submission to a system where reputation hangs on willingness to risk death. Legal suppression constrains this system, but the deeper erosion comes from endogenous delegitimation: younger generations increasingly question whether honor demands death, whether reputation requires lethal risk. The tradition faces both external suppression and internal identity-frame dissolution.
constraint_indexing:constraint_classification(composite_overdetermined_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRALIZING STATE (ROPE) — The state benefits from dueling suppression: removes rival authority over violence, strengthens monopoly on legitimate force, reduces aristocratic deaths that drain elite talent. The suppression is coordination at the state level — it solves the state's problem of controlling violence. The state experiences the constraint as low extraction and moderate suppression to itself (high suppression FOR duellers, but the state is the beneficiary, not the target). Arbitrage exit available: the state can choose enforcement intensity.
constraint_indexing:constraint_classification(composite_overdetermined_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BOURGEOIS COMMERCIAL ORDER (ROPE) — Commercial interests benefit from dueling suppression: removes unpredictable elite withdrawals for honor crises, enables reliable contracting and partnership, reduces violence-driven social instability. This perspective sees dueling suppression as coordination that enables commerce. The extraction is low — the constraint aligns with commercial interests. The state and commerce are aligned beneficiaries.
constraint_indexing:constraint_classification(composite_overdetermined_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORMIST ELITE / ENLIGHTENMENT INTELLECTUALS (SCAFFOLD) — Progressive elites see honor-code suppression as a temporary coordination problem with a sunset: education in rationality and bourgeois reputation mechanisms will eventually replace honor satisfaction with professional standing. The constraint is temporarily coercive (high suppression) but structurally temporary — as alternative legitimacy systems spread, the need for coercive suppression declines. Theater ratio is low here because the reformists see genuine function: legal suppression is doing real work to break an obsolete system.
constraint_indexing:constraint_classification(composite_overdetermined_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: NOSTALGIC HONOR REVIVALIST (PITON) — Later traditionalists attempting to preserve honor codes in the face of legal suppression resort to ritualized substitutes: formal apologies, published retorts, dueling societies that meet in secrecy. These substitutes are substantially theatrical: they perform honor without functional replication satisfaction. The revivalist perceives the honor system as degraded, maintained through inertia and institutional memory rather than live legitimacy. High theater ratio reflects that dueling in the 19th century becomes performative — the functional honor mechanism is already gone.
constraint_indexing:constraint_classification(composite_overdetermined_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, honor systems are an inherent feature of aristocratic social structures; they cannot be 'eliminated' in principle, only transformed. Viewed this way, dueling is a natural eruption of honor codes — as unchangeable as the honor system itself. This perspective risks naturalizing what is actually a contingent causal entanglement (legal suppression + identity frame erosion). The false summit detector should flag this: beneficiaries exist (state, commerce), and the constraint is causally contingent, not naturally inevitable.
constraint_indexing:constraint_classification(composite_overdetermined_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(composite_overdetermined_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(composite_overdetermined_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(composite_overdetermined_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(composite_overdetermined_reading, TR),
    TR >= 0.70.

:- end_tests(composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the constraint's asymmetric burden. The state and commercial interests extract significant benefit (monopoly on violence, reliable elite cooperation, predictable contracting). The duellist and honor substrate bear full cost (death, exile, or surrender of identity). The value (0.58 not 0.72) reflects that extraction is partial — some reformist alternatives genuinely emerged (professional standing, academic prestige replaced lethal honor); the suppression mechanism is not pure extraction but enforces a difficult transition. Suppression (0.72): High, corresponding to the total impossibility of honor satisfaction within legal order. Both mechanisms enforce this: law makes dueling illegal (structural suppression); delegitimation makes honor demands themselves questionable (cognitive suppression). Theater ratio (0.38): Moderate, reflecting that the constraint exhibits genuine coordination function (state violence control, commercial order enabling) rather than pure performative maintenance. Early interval shows low theater (mechanism is functionally necessary); by late interval, honor ritual substitutes become more theatrical (dueling societies, formal apologies), raising theater ratio toward piton territory. The trajectory shows degradation from functional suppression toward ritualized theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap between snare and rope perspectives is maximal. A trapped duellist perceives the constraint as pure extraction with no exit (snare). The state perceives it as coordination mechanism for its own interests (rope). The honor-code tradition perceives it as both coordination (reputation differentiation) and extraction (lethal-risk backing). The reformist perceives a temporary problem with a known sunset (scaffold). This diversity across observations of the same structural phenomenon is diagnostic: the constraint's classification is fully observer-dependent, which is proper for a tangled_rope or snare, but the mountain perspective suggests the engine should flag false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this composite constraint is driven by the agent's structural relationship to BOTH mechanisms (legal suppression AND honor delegitimation). A duellist trapped by honor code demands and legal prohibition experiences maximum directionality toward target (d ≈ 0.95, powerless/trapped). A state official enforcing suppression experiences directionality toward beneficiary (d ≈ 0.05, institutional/arbitrage). A reformist intellectual advocating for honor replacement experiences moderate directionality (d ≈ 0.50, powerful/mobile) — not fully trapped but not fully benefiting. The entanglement of suppression and delegitimation creates a composite d-landscape: some agents experience extraction through legal force alone; others through honor-code collapse alone; most through the impossible combination of both.
 *
 * MANDATROPHY ANALYSIS:
 *   OVERDETERMINED READING RESOLUTION: This reading resolves the mandatrophy by accepting that dueling's decline was genuinely overdetermined — no single mechanism explains it completely. The composite constraint captures the causal entanglement: legal suppression and honor delegitimation reinforced each other, neither reducible to the other. The mandatrophy question ('Which is the real constraint?') has a deflationary answer: both are real, both operated, their interaction is the constraint. The tangled_rope classification reflects this: the constraint coordinates state interests and commercial order (rope function) while extracting from aristocratic practitioners and the honor substrate (asymmetric extraction). Sibling readings isolate one mechanism each (practice_decline_reading emphasizes suppression; cultural_contraction_reading emphasizes delegitimation); this reading accepts both causal chains and models their non-independence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_entanglement_decomposability,
    'Are legal suppression and honor-code delegitimation structurally independent mechanisms or genuinely non-decomposable causal pathways?',
    'Comparative historical analysis: regions with legal suppression but no delegitimation (whether honor persists); regions with delegitimation but minimal legal suppression (whether dueling declines anyway). Test whether removing one mechanism while keeping the other constant produces proportional decline.',
    'If fully independent: two separate constraints, each with its own ε. If genuinely entangled: single composite constraint requires the tangled_rope classification to capture both mechanisms. If partially separable: base story on the dominant mechanism, use omega to document the secondary pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_entanglement_decomposability, empirical, 'Whether legal suppression and honor delegitimation are independent or entangled causal mechanisms').

omega_variable(
    honor_substrate_transformation_vs_externally_imposed,
    'Did the honor code''s legitimacy erode endogenously (young aristocrats genuinely questioned it) or was delegitimation strategically induced by state/commercial interests?',
    'Textual analysis of aristocratic discourse (letters, diaries, satirical literature): track whether honor-code skepticism emerges before legal suppression intensifies, or appears synchronized with state campaigns. Analyze whether lower classes maintained honor codes despite lacking legal enforcement, vs. whether all strata delegitimized honor simultaneously.',
    'If endogenous: honor-code transformation is a genuine mountain erosion (something intrinsic to the system weakened). If externally induced: state and commerce manufactured delegitimation; the snare perspective dominates. If mixed: tangled_rope captures both mechanisms, with omegas specifying the mixture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_substrate_transformation_vs_externally_imposed, conceptual, 'Whether honor-code delegitimation was endogenous cultural shift or strategically induced').

omega_variable(
    functional_vs_performative_honor_boundary,
    'Can honor systems maintain functional status differentiation (reputation, deference, marriageability advantage) without lethal risk? Or does removing the mortality component collapse the entire honor mechanism?',
    'Structural analysis of non-lethal honor systems: professional licensing, academic prestige, clerical authority, diplomatic precedence. Do these systems produce equivalent reputational effects to dueling-backed honor? Can an aristocracy maintain status hierarchy without dueling? Historical tracking of substitute legitimacy mechanisms that emerged post-suppression.',
    'If honor can survive without dueling: the suppression mechanism is extractive (snare), not mountain-erosive. If honor intrinsically requires lethal backing: mountain erosion is real (substrate fundamentally transformed). If partial: tangled_rope (system has coordination function but extraction mechanism is the lethal-risk backing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_vs_performative_honor_boundary, empirical, 'Whether honor systems require lethal backing or can function with other reputation mechanisms').

omega_variable(
    reading_selection_alternative_mechanisms,
    'This reading presumes BOTH suppression and delegitimation operated. Sibling readings isolate one mechanism each. Which reading best captures the historical causality?',
    'Chronological correlation analysis: did legal suppression precede or follow honor-code skepticism in key regions? Did suppression intensity correlate with dueling decline, or did decline occur despite weak enforcement? Did regions with stronger legal enforcement see faster honor-code collapse?',
    'If suppression temporally and causally dominates: practice_decline_reading is more accurate (dueling fell because law made it costly, honor system would persist if unchecked). If delegitimation dominates: cultural_contraction_reading is more accurate (honor code became internally delegitimized; law merely formalized inevitable). If both causal chains are visible and non-reducible: composite_overdetermined_reading (this reading) is accurate, but must specify mixture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_alternative_mechanisms, empirical, 'Which causal mechanism (legal suppression vs. honor delegitimation vs. both) dominates dueling decline').

omega_variable(
    false_summit_risk_naturalizing_contingency,
    'Does the mountain perspective (honor as natural to aristocracy) naturalize what is actually a contingent institutional arrangement that could have persisted without state suppression?',
    'Counterfactual: regions with weaker state capacity where dueling persisted longer despite elite cultural change. Hypothetical: had the state not criminalized dueling, would honor systems have transformed into non-lethal variants (professional standing, academic prestige, diplomatic precedence) without external suppression?',
    'If the mountain perspective is accurate: honor codes are indeed a natural feature of hierarchical systems (resistant constraint). If naturalization is false: the mountain is a false summit (naturalizes the state''s contingent institutional choice to suppress through law + delegitimation campaigns). FSM engine evaluation should flag when beneficiaries (state, commerce) are present and constraint exhibits non-trivial suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_risk_naturalizing_contingency, conceptual, 'Whether honor systems are natural to aristocracy or contingent institutional arrangements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(composite_overdetermined_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compos_theater_early, composite_overdetermined_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(compos_theater_midpoint, composite_overdetermined_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(compos_theater_late, composite_overdetermined_reading, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(compos_extract_early, composite_overdetermined_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(compos_extract_midpoint, composite_overdetermined_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(compos_extract_late, composite_overdetermined_reading, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(composite_overdetermined_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(composite_overdetermined_reading, practice_decline_reading).
narrative_ontology:affects_constraint(composite_overdetermined_reading, cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_substrate kernel admits three structurally distinct readings: this composite story (both suppression and delegitimation), a practice_decline story (suppression dominates), and a cultural_contraction story (delegitimation dominates). All three are valid historical claims depending on the regional and temporal slice examined. They are linked as readings of the same kernel, not as independent constraints. Each reading produces a different ε and different beneficiary/victim structure for the same natural-language phenomenon ('dueling decline'). The ε-invariance principle applies: if changing which mechanism you emphasize changes ε, you are reading different constraints from the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
