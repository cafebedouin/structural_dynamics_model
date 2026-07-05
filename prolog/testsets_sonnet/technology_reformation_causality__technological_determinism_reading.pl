% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press as Deterministic Cause of the Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological_determinism_reading of the
 *   technology_reformation_causality kernel: the claim that the printing
 *   press's mechanical reduction in copying cost made mass vernacular
 *   scripture distribution — and thereby the Reformation's scale, speed, and
 *   geographic spread — a near-inevitable consequence of the technology
 *   itself, largely independent of which particular reformers or printers
 *   acted. Under this reading the press functions structurally like a
 *   mountain: a fixed physical/economic fact (movable type's cost curve) that
 *   reformers, printers, and the Curia alike had to adapt to rather than a
 *   lever any party strategically wielded to produce the outcome. Two sibling
 *   readings of the same kernel are NOT represented here:
 *   beneficiary_agency_reading (reformers and printers deployed the press
 *   strategically; technology was tool not cause) and co_constitution_reading
 *   (technology and social actors co-evolved). Those are separate constraint
 *   stories with their own ε values and structural claims — this file does
 *   not average over them or describe their contest internally.
 *
 * KEY AGENTS:
 *   - print_shop_proprietors: beneficiary (moderate/mobile) — commercial gain treated as downstream of cost collapse
 *   - vernacular_bible_readership: beneficiary (powerless/constrained) — recipients of a structural access shift
 *   - reformist_clergy: beneficiary/payer (moderate/constrained) — theological content riding a technologically-set diffusion wave
 *   - roman_curia: payer (institutional/trapped) — loses interpretive monopoly as a mechanical entailment, not a political defeat
 *   - manuscript_copying_guilds: payer (moderate/trapped) — economically obsoleted by the cost structure
 *   - media_historians: observer (analytical) — evaluates the sufficiency of the technological account
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.08).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.05).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press as Deterministic Cause of the Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '7c30a706-0727-456b-83fe-90a88e823941').
narrative_ontology:cs_kernel_codification('7c30a706-0727-456b-83fe-90a88e823941', distributed).
narrative_ontology:cs_authority_grounding('7c30a706-0727-456b-83fe-90a88e823941', distributed).
narrative_ontology:cs_reading_relation('7c30a706-0727-456b-83fe-90a88e823941', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c30a706-0727-456b-83fe-90a88e823941', technology_reformation_causality__co_constitution_reading, influences).
narrative_ontology:cs_axiom('7c30a706-0727-456b-83fe-90a88e823941', foundational, technology_sets_outcome_ceiling_independent_of_agency).
narrative_ontology:cs_axiom_status(technology_sets_outcome_ceiling_independent_of_agency, holdable).
narrative_ontology:cs_axiom_grounding('7c30a706-0727-456b-83fe-90a88e823941', technology_sets_outcome_ceiling_independent_of_agency, empirically_contingent).
narrative_ontology:cs_axiom('7c30a706-0727-456b-83fe-90a88e823941', secondary, reformer_strategy_is_causally_secondary_to_cost_structure).
narrative_ontology:cs_axiom_status(reformer_strategy_is_causally_secondary_to_cost_structure, holdable).
narrative_ontology:cs_axiom_grounding('7c30a706-0727-456b-83fe-90a88e823941', reformer_strategy_is_causally_secondary_to_cost_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('7c30a706-0727-456b-83fe-90a88e823941', manuscript_scarcity_gatekeeping_framework).
narrative_ontology:cs_drift_state('7c30a706-0727-456b-83fe-90a88e823941', post_movable_type_diffusion, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('7c30a706-0727-456b-83fe-90a88e823941', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, print_shop_proprietors).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_bible_readership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, reformist_clergy).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, reformist_clergy).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, roman_curia).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, manuscript_copying_guilds).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, media_determinism_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, printing_press_causal_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate movable-type workshops that mechanically reduce the marginal cost of copying text by orders of magnitude relative to hand-copying. Under this reading their commercial success is a downstream consequence of a fixed cost-structure fact about the technology, not a strategic choice they made to unseat Church authority — the physics and economics of movable type would have produced the same production-cost collapse regardless of who operated the presses.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, print_shop_proprietors, beneficiary,
    moderate, biographical, mobile, regional).

% Newly literate and semi-literate laypeople gain access to printed vernacular scripture at a price point and volume that hand-copied manuscripts never approached. In this reading, their access is a mechanical entailment of the cost collapse itself; the reading positions them as recipients of a structural shift rather than agents who organized to obtain it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_bible_readership, beneficiary,
    powerless, generational, constrained, continental).

% Figures such as reform-minded clergy find their vernacular arguments propagate at a speed and scale that manuscript circulation could never sustain. Under the determinism reading their theological content is treated as riding a wave whose amplitude and reach were set by the press's cost structure, not by their rhetorical or organizational skill — a framing some of them and their heirs would dispute.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformist_clergy, beneficiary,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__technological_determinism_reading, reformist_clergy, payer).

% The centralized ecclesiastical authority that previously controlled scriptural interpretation through manuscript scarcity and clerical literacy monopoly now faces a mechanically-driven collapse of that scarcity. Under this reading, the Curia had no meaningful countermove — the technology's cost curve made loss of interpretive monopoly a matter of physical inevitability rather than a contest it could have won with better strategy or earlier suppression.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, roman_curia, payer,
    institutional, civilizational, trapped, continental).

% Scriptoria and professional scribes whose entire economic function — hand-copying texts at high per-unit cost — is rendered obsolete by the press's cost structure. They bear the transitional cost of the technology's mechanical superiority with no plausible route back; in this reading their displacement is a physical/economic fact, not a political defeat inflicted by any particular faction.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, manuscript_copying_guilds, payer,
    moderate, biographical, trapped, regional).

% Scholars who evaluate whether the printing press's production-cost collapse alone is sufficient to explain the Reformation's scale and timing, independent of the strategic choices reformers and printers made. They compare this reading against sibling readings that assign causal weight to agency and co-evolution.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the coordination sense — this reading treats the press's diffusion of vernacular scripture as a mechanical consequence of a fixed production-cost structure, not as a solution to a collective-action problem that any party organized to solve.
% TRANSFER_FUNCTION: The technology's cost collapse moves interpretive authority away from manuscript-scarcity gatekeepers (the Curia, copying guilds) toward anyone who can afford a printed vernacular text, as a physical entailment of the cost curve rather than a deliberate transfer engineered by any actor.
% ABSENT_VOICES: The sibling readings' proponents — historians who attribute causal weight to reformer strategy (beneficiary_agency_reading) or to reformer-press co-evolution (co_constitution_reading) — are not represented within this reading's own framework; this reading treats their causal claims as secondary to the technological fact.
% DISAPPEARANCE_RATIONALE: If the printing press's specific cost-collapse mechanism were absent, this reading holds the Reformation's scale and speed would not have occurred as they did — the press is load-bearing in its own causal account. Sibling readings dispute this counterfactual, holding that reformers would have found alternative distribution channels or that the technology alone was insufficient absent strategic deployment, which is why the verdict is contested at the kernel level even though this reading itself asserts world_rearranges.
% FOUNDING_PROBLEM: Manuscript-era scriptural interpretation was gatekept by an expensive, slow, low-volume copying process that structurally limited who could read scripture directly and forced reliance on clerical intermediaries.
% FOUNDING_PROBLEM_CORROBORATION: Media historians and economic historians of print (outside the beneficiary set of print-shop proprietors and reformist clergy) corroborate that manuscript-copying costs were the binding constraint pre-1450 and that movable type mechanically removed that constraint; this corroboration is independent of any theological or commercial interest in the outcome.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.03 rising to 0.08) because under this reading no party is extracting rents from the constraint's operation — the press's cost-reduction is a physical/economic fact, and the modest late-period rise reflects only the gradual commercial consolidation of print shops, not coercive extraction. Suppression is authored low (0.05) because a genuine mountain-type constraint does not require active enforcement to persist — the cost curve holds regardless of anyone's preference. Accessibility collapse is authored high (0.82) because once movable type existed, the option of returning to manuscript-scarcity economics as a viable interpretive gatekeeping mechanism collapsed almost completely — no institutional actor could restore the pre-press cost structure by fiat. Resistance is authored low-moderate (0.15) because while the Curia resisted the outcome (indices, suppression campaigns), it could not resist the underlying cost mechanics themselves, only their downstream religious content — this reading treats that as resistance to a symptom, not to the mountain itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (print shop proprietors, vernacular readership, reformist clergy) sit near the low end of directionality because the reading frames their gains as automatic entailments of the cost-structure fact rather than something extracted from any victim. Payers (the Curia, copying guilds) sit higher not because anything is extracted FROM them by a beneficiary agent, but because they bear the adjustment cost of a physical constraint shifting under them — this is closer to a mountain's 'no party collects from its operation' character than to snare-style extraction, which is why no victims array is authored: the guilds and Curia bear displacement cost, not extraction proper.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling by keeping the causal claim narrow: it is not claiming the press was designed to solve a coordination problem for reformers (that would smuggle in agency, contaminating this reading with the beneficiary_agency_reading's premises). It is claiming only that a fixed technological fact set the ceiling on possible outcomes. If evidence later shows the cost-reduction alone was insufficient without organized reformer distribution networks (per co_constitution_reading), this story's classification as mountain would need reconsideration — that is exactly the divergence the omega below is built to track.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    determinism_vs_agency_sufficiency,
    'Is the printing press''s production-cost reduction alone sufficient to explain the Reformation''s scale and speed, or was organized reformer/printer strategic action a necessary additional ingredient?',
    'Comparative historical analysis of regions/periods where movable-type printing existed without strong reformer distribution networks (e.g. early print culture prior to organized vernacular Bible campaigns) versus regions where both were present — if cost reduction alone predicts diffusion speed without strategic organization, the determinism reading gains support; if diffusion tracked organizational effort more than raw press availability, the beneficiary_agency_reading or co_constitution_reading is better supported.',
    'If agency/organization is shown necessary, this constraint should be reclassified away from mountain toward a co-constituted or agency-driven structure, and the beneficiaries declared here (print shop proprietors, readership) would need to be re-examined as active participants rather than downstream recipients — this is the central kernel-level contest this story deliberately does not resolve internally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinism_vs_agency_sufficiency, conceptual, 'Whether the technological cost-collapse alone, absent reformer/printer strategic agency, is causally sufficient for the Reformation''s observed scale and speed.').

omega_variable(
    natural_law_vs_constructed_beneficiary_structure,
    'Given that print_shop_proprietors and vernacular_bible_readership are declared beneficiaries on a claimed mountain, is the printing press''s cost-structure effect genuinely a natural/physical constraint, or does the ''mountain'' framing itself understate constructed choices (patent/guild regulation, type-founding capital requirements, licensing) that shaped who could operate presses and thus who benefited?',
    'Examine whether press operation was open to any capital-holder (supporting the natural-cost-curve reading) or gated by guild/licensing structures that constructed a narrower beneficiary class (supporting a constructed-constraint reading, which would favor reclassification per the false-summit-mountain signature).',
    'If press access was substantially gated by non-physical (legal/guild) barriers, the ''mountain'' claim partially conflates a physical cost-reduction fact with a constructed access-restriction fact, and the beneficiary structure would need decomposition into a separate constraint for the access-gating mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_beneficiary_structure, empirical, 'Required FSM-triggering omega: documents the natural-law-vs-constructed ambiguity created by declaring beneficiaries on a mountain claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_reformation_causality__technological_determinism_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t16, technology_reformation_causality__technological_determinism_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement_basis(tech_tr_t16, observed).
narrative_ontology:measurement(tech_tr_t32, technology_reformation_causality__technological_determinism_reading, theater_ratio, 32, 0.09).
narrative_ontology:measurement_basis(tech_tr_t32, observed).
narrative_ontology:measurement(tech_tr_t48, technology_reformation_causality__technological_determinism_reading, theater_ratio, 48, 0.1).
narrative_ontology:measurement_basis(tech_tr_t48, observed).
narrative_ontology:measurement(tech_tr_t64, technology_reformation_causality__technological_determinism_reading, theater_ratio, 64, 0.1).
narrative_ontology:measurement_basis(tech_tr_t64, observed).
narrative_ontology:measurement(tech_tr_t80, technology_reformation_causality__technological_determinism_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement_basis(tech_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t16, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 16, 0.05).
narrative_ontology:measurement_basis(tech_be_t16, observed).
narrative_ontology:measurement(tech_be_t32, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 32, 0.06).
narrative_ontology:measurement_basis(tech_be_t32, observed).
narrative_ontology:measurement(tech_be_t48, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 48, 0.07).
narrative_ontology:measurement_basis(tech_be_t48, observed).
narrative_ontology:measurement(tech_be_t64, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 64, 0.08).
narrative_ontology:measurement_basis(tech_be_t64, observed).
narrative_ontology:measurement(tech_be_t80, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 80, 0.08).
narrative_ontology:measurement_basis(tech_be_t80, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__technological_determinism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__technological_determinism_reading, 0.02).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the technology_reformation_causality kernel. technological_determinism_reading (this file) treats the press as a mountain-like fixed cost-structure fact; beneficiary_agency_reading treats the press as a tool strategically deployed by reformers/printers (likely rope or tangled_rope, with reformers as agenda_setters); co_constitution_reading treats press and social actors as mutually shaping (likely rope with distributed agency). All three must remain linked via affects_constraints; none averages ε across the others per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
