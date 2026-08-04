% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: The Printing Press as Deterministic Cause of the Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological_determinism reading of a
 *   contested kernel about why the Reformation succeeded where earlier
 *   heretical movements (Hussites, Waldensians, Wycliffites) had been
 *   suppressed. This reading holds that the printing press functioned as an
 *   exogenous, near-physical constraint: once movable type collapsed the
 *   marginal cost of copying, censorship regimes calibrated to a
 *   manuscript-scarcity world became structurally incapable of suppressing
 *   vernacular scripture or reformist tracts, regardless of Church will or
 *   effort. On this reading the Church's suppression campaigns (indices, book
 *   burnings, printer executions) were not failed strategy but attempts to
 *   enforce a control regime that the underlying reproduction technology had
 *   already made physically obsolete — akin to attempting to dam a river with
 *   hand tools after the watershed had already shifted. Reformers and printer
 *   guilds are downstream beneficiaries of a capacity they did not create and
 *   could not have summoned by strategy alone; the vernacular-literate laity
 *   inherited access that no prior insurgent movement had possessed. This is
 *   presented as a Mountain: a structural fact about 15th-16th century
 *   reproduction economics that would have unfolded on roughly the observed
 *   timeline regardless of which particular reformers or printers were
 *   involved.
 *
 * KEY AGENTS:
 *   - protestant_reformers: Primary beneficiary (organized/mobile) — rode an exogenous capacity shift to distribute vernacular scripture and polemic at unprecedented speed
 *   - printer_guilds: Secondary beneficiary (organized/arbitrage) — commercial actors whose profit motive aligned with, but did not create, the capacity
 *   - catholic_church_authorities: Structural target of the constraint (institutional/trapped-in-role) — attempted suppression that this reading holds was foreclosed by the technology itself
 *   - vernacular_literate_laity: Diffuse beneficiary (powerless/moderate mobility) — gained access to scripture and pamphlet literature previously mediated exclusively by clergy
 *   - historians_of_technology: Analytical observer — assesses whether the causal claim is genuinely deterministic or retrospectively flattering to reformist historiography
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.28).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.15).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.28).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "The Printing Press as Deterministic Cause of the Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'a8ac621e-adca-4460-b6a0-defc2c802bfa').
narrative_ontology:cs_kernel_codification('a8ac621e-adca-4460-b6a0-defc2c802bfa', distributed).
narrative_ontology:cs_authority_grounding('a8ac621e-adca-4460-b6a0-defc2c802bfa', distributed).
narrative_ontology:cs_reading_relation('a8ac621e-adca-4460-b6a0-defc2c802bfa', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('a8ac621e-adca-4460-b6a0-defc2c802bfa', press_reformation_causation__mutual_shaping, influences).
narrative_ontology:cs_axiom('a8ac621e-adca-4460-b6a0-defc2c802bfa', foundational, technology_possesses_independent_causal_sufficiency).
narrative_ontology:cs_axiom_status(technology_possesses_independent_causal_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('a8ac621e-adca-4460-b6a0-defc2c802bfa', technology_possesses_independent_causal_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('a8ac621e-adca-4460-b6a0-defc2c802bfa', foundational, institutional_suppression_was_categorically_foreclosed_not_merely_outmatched).
narrative_ontology:cs_axiom_status(institutional_suppression_was_categorically_foreclosed_not_merely_outmatched, holdable).
narrative_ontology:cs_axiom_grounding('a8ac621e-adca-4460-b6a0-defc2c802bfa', institutional_suppression_was_categorically_foreclosed_not_merely_outmatched, empirically_contingent).
narrative_ontology:cs_reference_frame('a8ac621e-adca-4460-b6a0-defc2c802bfa', manuscript_scarcity_control_regime).
narrative_ontology:cs_drift_state('a8ac621e-adca-4460-b6a0-defc2c802bfa', post_gutenberg_diffusion_peak, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('a8ac621e-adca-4460-b6a0-defc2c802bfa', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, printer_guilds).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_literate_laity).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, technological_determinism_doctrine).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, media_ecology_causal_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Distribute vernacular scripture, sermons, and polemical tracts at a speed and scale no prior heretical movement achieved. On this reading, they did not create the underlying capacity — they inherited a reproduction-cost collapse already underway and organized around it. Their exit option is mobility across fragmented European jurisdictions, several of which tolerated printing even where others suppressed it, which is itself cited as evidence the underlying capacity, not any single sovereign's tolerance, was the operative factor.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).

% Commercial print shops profit from producing reformist material alongside indulgences, almanacs, and secular texts; profit motive is agnostic to doctrinal content. Can relocate operations across city-states and principalities with varying censorship regimes, arbitraging between jurisdictions — a mobility this reading treats as evidence that no single suppression effort could have closed off the capacity everywhere at once.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printer_guilds, beneficiary,
    organized, biographical, arbitrage, continental).

% Gain direct access to scripture and religious argument previously mediated almost exclusively through clergy and Latin literacy. Cannot individually produce or distribute texts, so their benefit is entirely a downstream receipt of a capacity shift they had no role in producing; their exit options remain locally constrained even as the information environment around them changes.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_literate_laity, beneficiary,
    powerless, generational, constrained, regional).

% Attempt to maintain doctrinal control through indices of forbidden books, printer licensing, book burnings, and executions of printers and distributors. On this reading their position is structurally trapped: their suppression apparatus was calibrated to a manuscript-scarcity world, and the reproduction-cost collapse left them administering a control regime that could no longer be technically enforced at the scale required, regardless of the resources or will committed to it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church_authorities, agenda_setter,
    institutional, civilizational, trapped, continental).

% Assess competing causal accounts of the press-Reformation relationship, including whether the determinism thesis (associated with Eisenstein) overstates technological causal sufficiency relative to revisionist book-history scholarship (Febvre, Grafton, Pettegree) that emphasizes contingency, regional variation in suppression efficacy, and reformer/printer strategic choice.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary Rope sense — this reading denies the outcome required deliberate multi-party coordination at all. Insofar as any coordination function exists, it is the press's function as a reproduction-cost-collapse mechanism that any party (reformist or Catholic) could in principle have used; on this reading its consequences simply favored decentralized vernacular distribution over centralized doctrinal control by the nature of the technology itself.
% TRANSFER_FUNCTION: No transfer in the extraction sense between named parties; the constraint (reproduction-cost collapse) shifts control over religious-textual distribution away from a centralized clerical/manuscript-guild monopoly and toward decentralized print-capable actors. This reading treats that shift as a structural fact of the technology's economics, not a transfer extracted by any party from another.
% ABSENT_VOICES: Manuscript scribes, illuminators, and the clerical scriptoria whose copying monopoly was structurally displaced are not represented as stakeholders here because their displacement is, on this reading, a side effect of the capacity shift rather than a targeted extraction — but a mutual_shaping or strategic_deployment reading might treat their displaced labor interest as more central. Regional Catholic territories that DID achieve partial, extended suppression (parts of Spain, Italy) are underweighted in this reading's 'inevitability' framing and would object that suppression was contingent and regionally variable, not categorically foreclosed.
% DISAPPEARANCE_RATIONALE: If the printing press had not existed (the relevant counterfactual, since the constraint itself cannot 'disappear' once instantiated), this reading holds the world rearranges substantially: reformist ideas would have propagated at manuscript-network speed, plausibly allowing existing suppression apparatus (as it did against Hussites and Waldensians) to contain the movement regionally. But this is contested precisely because it is the crux of the kernel disagreement — strategic_deployment and mutual_shaping readings would argue reformer/printer agency could have found alternative distribution channels, making the counterfactual less determinate than this reading claims.
% FOUNDING_PROBLEM: The determinism thesis was built to explain why the Reformation succeeded at civilizational scale where the Hussite, Waldensian, and Wycliffite reform movements had been geographically contained and eventually suppressed — what changed structurally between the 14th-century and 16th-century attempts.
% FOUNDING_PROBLEM_CORROBORATION: Elizabeth Eisenstein's 'The Printing Press as an Agent of Change' (1979) is the thesis's most cited scholarly corroboration and is itself written from outside any confessional beneficiary group, arguing on media-ecology grounds. However, revisionist book historians (Adrian Johns, Andrew Pettegree) explicitly outside the reformist historiographical tradition contest the strength of the causal claim, citing regional suppression successes and reformer strategic choices the determinism thesis underweights — so corroboration exists on both sides of the contest, and no single outside authority has settled it.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, contested).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) and rising only slightly over the interval: a genuine Mountain reading should show near-zero extraction, but this reading concedes a vindicated-proposition dynamic — determinist historiography itself has been used by confessional and media-studies traditions to extract intellectual authority (citation capital, disciplinary prestige) beyond what the bare causal claim warrants, hence the nonzero and slowly rising theater_ratio (0.10 to 0.20) as the narrative calcified into disciplinary orthodoxy in 20th-century media studies (Eisenstein's thesis). Suppression is authored low (0.15) because this reading does not claim active coercion maintains the narrative — it claims a natural-law-like inevitability, and dissent from the thesis (Febvre, Grafton, revisionist book-historians) has not been suppressed, only outvoted in prestige terms. Resistance is moderate (0.3) reflecting genuine ongoing historiographical contest with the sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers, printer guilds, and vernacular-literate laity are declared beneficiaries because on this reading they receive an exogenous capacity windfall — they did not construct the constraint (movable-type economics) and could not have summoned it by will; the engine's derivation should place them near the beneficiary end of directionality (low d) despite some of them (guilds) being commercially organized actors, because the causal story specifically denies their agency was the operative cause. The Church authorities are the structural target: on this reading, resistance was not merely costly but categorically futile, which is a much stronger claim than ordinary victimhood — futility, not mere cost-bearing, is what characterizes their position and is why no victims array is authored (a Mountain has no victims by construction; the Church's failed suppression is friction against natural law, not extraction from a party).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in one specific direction: it prevents crediting reformist strategic genius or printer commercial cunning with an outcome that (on this reading) would have occurred regardless of particular actors' intentions. Where strategic_deployment risks over-crediting agency (making the Reformation look like a successful plan rather than a capacity-driven cascade), this reading's risk runs the other way — it can launder confessional triumphalism into apparently neutral technological inevitability, which is exactly the FSM concern the omega variables above are built to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_narrative,
    'Is ''the press made censorship impossible'' a genuine technical/physical fact about reproduction economics circa 1450-1550, or a retrospectively constructed narrative that serves beneficiaries who prefer to be seen as riding an unstoppable historical force rather than as strategic actors who could have been suppressed?',
    'Comparative case study: the Ottoman and some Catholic-controlled territories DID suppress printing of vernacular/heretical material for extended periods with partial success (index of censorship efficacy by region/decade). If suppression efforts show measurable local success rates comparable to pre-press manuscript control, the determinism claim weakens toward strategic_deployment or mutual_shaping.',
    'If suppression was regionally effective, the ''inevitability'' premise collapses and this reading''s Mountain claim is unsupportable — the constraint would reclassify toward tangled_rope (technology as contested resource, not exogenous force). If suppression was genuinely and universally futile once print capacity crossed a threshold, the Mountain claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_narrative, empirical, 'Whether print-driven censorship failure was a physical/economic inevitability or a contingent, partially-successful contest.').

omega_variable(
    beneficiary_narrative_capture,
    'Do Protestant historiography and modern media-studies textbooks that repeat the determinism narrative do so because it is structurally accurate, or because it flatters the printers'' guilds (absolved of complicity) and the reformers (cast as riders of inevitable history rather than deliberate insurgents)?',
    'Trace citation lineage of the ''printing caused the Reformation'' claim to its earliest confessional historiography (16th-17th c. Protestant chroniclers) versus later secular media-studies scholarship (Eisenstein et al.); assess whether the causal claim tightened or loosened as confessional interest in the narrative declined.',
    'If the claim''s strength correlates with confessional interest rather than with independent archival evidence, this is a textbook false-summit case: a mountain claim with a beneficiary group visible underneath it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_narrative_capture, conceptual, 'Whether determinist historiography is independently evidenced or beneficiary-shaped.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This story is ONE of three readings of the press_reformation_causation kernel. Where exactly does the disagreement between technological_determinism, strategic_deployment, and mutual_shaping live?',
    'The disagreement is not about the historical events (all three readings agree the press existed, reformers used it, and censorship largely failed) but about the DIRECTION OF CAUSAL PRIORITY: determinism places the press''s material properties (movable type, exponential copy cost collapse) as the sufficient upstream cause with reformers as passive beneficiaries; strategic_deployment places reformer/printer intent as the sufficient cause with the press as neutral instrument; mutual_shaping denies either has causal priority and models a feedback loop. Resolving this requires either (a) a counterfactual test (would vernacular scripture distribution have occurred via manuscript networks absent the press, at what rate) or (b) accepting the readings as permanently coexisting interpretive frames rather than resolvable empirical claims.',
    'If (a) is answerable and shows manuscript networks could have achieved comparable distribution within a generation, determinism is falsified and strategic_deployment gains ground. If unanswerable, all three readings remain live and coexist as interpretive commitments rather than competing factual claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the sibling-reading disagreement in causal-priority direction, not in agreed historical fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__technological_determinism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causation__technological_determinism, theater_ratio, 20, 0.14).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causation__technological_determinism, theater_ratio, 40, 0.18).
narrative_ontology:measurement(pres_tr_t60, press_reformation_causation__technological_determinism, theater_ratio, 60, 0.2).
narrative_ontology:measurement(pres_tr_t80, press_reformation_causation__technological_determinism, theater_ratio, 80, 0.2).
narrative_ontology:measurement(pres_tr_t100, press_reformation_causation__technological_determinism, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__technological_determinism, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(pres_be_t20, press_reformation_causation__technological_determinism, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(pres_be_t40, press_reformation_causation__technological_determinism, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(pres_be_t60, press_reformation_causation__technological_determinism, base_extractiveness, 60, 0.28).
narrative_ontology:measurement(pres_be_t80, press_reformation_causation__technological_determinism, base_extractiveness, 80, 0.28).
narrative_ontology:measurement(pres_be_t100, press_reformation_causation__technological_determinism, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial claim 'the printing press caused the Reformation.' technological_determinism (this file) treats the press as an exogenous, near-Mountain constraint with reformers as downstream beneficiaries. strategic_deployment treats the press as neutral capacity strategically wielded by reformer/printer agency (likely tangled_rope or rope, agency-first). mutual_shaping denies causal priority to either side and models a feedback loop (likely rope or tangled_rope with bidirectional beneficiary/enforcement structure). Each carries a distinct ε consistent with its causal-priority claim; they are linked here per the ε-invariance decomposition principle rather than averaged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
