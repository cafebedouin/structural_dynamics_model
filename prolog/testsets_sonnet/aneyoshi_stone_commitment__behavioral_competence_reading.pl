% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone as Live Land-Use Rule (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/land_use
 *
 * SUMMARY:
 *   This story is the behavioral-competence reading of the Aneyoshi tsunami
 *   stone kernel: a marker erected after the 1933 Showa Sanriku tsunami
 *   instructing that homes not be built below its elevation. Under this
 *   reading, the stone functioned across 78 years (1933-2011) as an operative
 *   land-use constraint — households actually sited construction decisions
 *   relative to it, and in March 2011 the tsunami's runup stopped short of
 *   the marked line, with houses built above it surviving. The claim here is
 *   that the instruction retained live regulatory force through informal
 *   social transmission rather than decaying into mere commemoration. The
 *   sibling reading (commemorative_husk_reading, a separate constraint file)
 *   holds instead that the stone decayed into symbolic observance with no
 *   actual behavioral constraint on siting, and that the 2011 survival
 *   pattern reflects other causes (elevation determined by original
 *   settlement geography, fishing-harbor proximity constraints, coincidence).
 *   Both readings are consistent with the same surface facts about the
 *   stone's text and the 2011 outcome; they diverge on whether the causal
 *   arrow from stone-to-siting-decision is real. This file commits to the
 *   behavioral-competence reading only, per the ε-invariance principle — it
 *   does not hedge between readings or average their implied epsilon.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Tsunami Stone as Live Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/land_use").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, '0352e12d-053c-4021-8837-a008318e90d2').
narrative_ontology:cs_kernel_codification('0352e12d-053c-4021-8837-a008318e90d2', fixed_text).
narrative_ontology:cs_authority_grounding('0352e12d-053c-4021-8837-a008318e90d2', practice).
narrative_ontology:cs_interpretation_layer_present('0352e12d-053c-4021-8837-a008318e90d2').
narrative_ontology:cs_reading_relation('0352e12d-053c-4021-8837-a008318e90d2', aneyoshi_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('0352e12d-053c-4021-8837-a008318e90d2', foundational, inscribed_hazard_instruction_retains_behavioral_force_absent_formal_enforcement).
narrative_ontology:cs_axiom_status(inscribed_hazard_instruction_retains_behavioral_force_absent_formal_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('0352e12d-053c-4021-8837-a008318e90d2', inscribed_hazard_instruction_retains_behavioral_force_absent_formal_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('0352e12d-053c-4021-8837-a008318e90d2', secondary, informal_social_transmission_is_sufficient_causal_mechanism_for_multigenerational_compliance).
narrative_ontology:cs_axiom_status(informal_social_transmission_is_sufficient_causal_mechanism_for_multigenerational_compliance, holdable).
narrative_ontology:cs_axiom_grounding('0352e12d-053c-4021-8837-a008318e90d2', informal_social_transmission_is_sufficient_causal_mechanism_for_multigenerational_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('0352e12d-053c-4021-8837-a008318e90d2', post_1933_survivor_founding_intent).
narrative_ontology:cs_drift_state('0352e12d-053c-4021-8837-a008318e90d2', pre_2011_generational_transmission, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('0352e12d-053c-4021-8837-a008318e90d2', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents_below_stone_line_who_relocated).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, descendants_of_1933_tsunami_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__behavioral_competence_reading, prospective_household_builders).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_warning_transmission_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households in the hamlet whose ancestors, following the 1933 Showa Sanriku tsunami, carved and erected the boundary stone reading roughly 'do not build homes below this point,' and who as a matter of settled local practice have continued to site dwellings above the marked line for 78 years. In March 2011 the tsunami stopped a short distance below the stone; every house above the line survived undamaged. Their exit from the practice would mean rebuilding below the line, which almost none have done despite land pressure and convenience arguments favoring lower, flatter, more accessible lots.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents_below_stone_line_who_relocated, beneficiary,
    powerless, generational, constrained, local).

% The lineage that transmitted the stone's instruction across generations through repeated local retelling, informal social pressure on newcomers and returning family members about where to build, and periodic re-legitimation of the marker after near-miss events (the 1960 Chile tsunami, minor floods). They administer the norm informally — there is no zoning office, no permit gate, no fine — but they are the ones who would tell a relative building a house that the site is below the line.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, descendants_of_1933_tsunami_survivors, beneficiary,
    powerless, civilizational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, descendants_of_1933_tsunami_survivors, agenda_setter).

% A family deciding where to site a new or rebuilt house in Aneyoshi bears the real cost of compliance: land above the stone line is steeper, less convenient, sometimes more expensive per usable square meter, and farther from the harbor and fishing livelihood. Choosing to build below the line was always technically available — no legal prohibition existed — but the accumulated social and experiential weight of the stone made that choice rare enough that the 2011 survival pattern reflects near-universal compliance rather than random settlement.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, prospective_household_builders, payer,
    powerless, biographical, constrained, local).

% Prefectural and national disaster-reconstruction officials examined Aneyoshi's outcome after 2011 as an input to reformed tsunami hazard zoning, using the empirical correlation between siting-above-the-marker and structural survival as evidence for the causal claim that the stone functioned as an operative land-use constraint rather than a decorative marker. Their planning documents cite Aneyoshi specifically.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, post_2011_reconstruction_planners, observer,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, post_2011_reconstruction_planners, agenda_setter).

% Nearby Sanriku coast hamlets that erected similar post-1933 or post-1896 tsunami stones but where the markers were lost, forgotten, relocated, or simply not observationally reinforced by a near-miss event before 2011, and where housing was consequently built at lower elevations and suffered catastrophic losses. Their absence from the successful-transmission case is the comparison class that makes the Aneyoshi behavioral-competence claim testable, but they have no voice in how the Aneyoshi story is told or generalized.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, adjacent_communities_without_surviving_markers, excluded,
    powerless, generational, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine multi-generational coordination problem: how to transmit a hazard-avoidance instruction across a time horizon (decades to a century) that exceeds any living survivor's memory, without a formal cadastral or zoning authority, in a small hamlet with no independent land-use enforcement apparatus.
% TRANSFER_FUNCTION: Moves construction siting decisions upward in elevation, trading immediate convenience and lower construction cost (paid by each building household) for reduced expected loss from a low-probability, high-severity hazard (realized catastrophically in 2011). No monetary rent changes hands; the transfer is between a household's present-day siting convenience and its own future physical safety, mediated by inherited social pressure.
% ABSENT_VOICES: Adjacent Sanriku communities whose own tsunami markers failed to retain operational force are the natural comparison class and would complicate any simple narrative of stone-markers-as-reliable-mechanism — their outcomes are evidence that transmission fidelity, not stone-carving per se, is the operative variable, and that variable is not visible from inside the Aneyoshi case alone.
% DISAPPEARANCE_RATIONALE: If the stone and the social practice around it had not existed or had failed to retain force, the counterfactual land-use pattern would plausibly resemble neighboring hamlets: houses sited by convenience and cost near the harbor and flat ground, which the 2011 wave reached and destroyed. The stone's continued operational status is precisely why the built environment in Aneyoshi in 2011 differed from what unconstrained siting decisions would have produced.
% FOUNDING_PROBLEM: In the aftermath of the 1933 Showa Sanriku tsunami (which itself followed the 1896 Meiji Sanriku tsunami), survivors needed a way to prevent future generations — who would not have lived through the disaster and would face ordinary economic pressure to build on convenient low-lying land — from resettling the flood zone.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 field surveys and reconstruction-policy reviews by prefectural disaster-management researchers and outside anthropologists (not descendants of the stone's original authors, and with no stake in validating the marker's efficacy) document the correlation between siting above the marked line and 2011 structural survival, and cite the case in hazard-transmission literature as one of very few markers of this era that retained behavioral force rather than decaying into commemorative status alone.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored very low (0.06) and essentially flat across the 78-year interval because under this reading the constraint imposes only an opportunity cost (steeper, less convenient building sites) in exchange for a genuine risk-reduction benefit realized by the same households who bear the cost — there is no identifiable party extracting rent from another's compliance. Suppression is low (0.12): no legal enforcement, no penalty, purely normative and experiential reinforcement, with real (if rare) instances of departure from the norm. Theater ratio starts near zero and rises only slightly (0.02 to 0.08) reflecting the gradual generational dilution one would expect even in a mechanism that remains substantively operative — some of the later-era compliance may be increasingly ritualized restatement even while the underlying siting behavior continues. Accessibility collapse is moderate-high (0.62): once a household understands the marker and the local narrative around it, building below the line becomes a marked, socially visible choice rather than a neutral option, which is a real but not absolute narrowing of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   Under this reading, the payer seat (prospective builders inconvenienced by steep terrain) and the beneficiary seat (the same builders and their descendants, protected from tsunami) are not structurally opposed the way payer and agenda-setter diverge in an extractive constraint — the divergence one would expect to see instead is temporal: a builder deciding in, say, 1975, with no living memory of 1933 and no intervening confirming disaster, experiences the constraint as a costlier, less legible inherited custom, while the same builder's descendant in 2012 experiences it as vindicated life-saving wisdom. The engine's per-seat computation should reflect low-to-moderate divergence, not the sharp payer/beneficiary split characteristic of tangled ropes or snares.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries and payers substantially overlap here, which is the structural signature of a genuine coordination mechanism rather than an extractive one: the household that bears the inconvenience cost of building on a steeper site above the line is the same household protected from tsunami loss. Descendants who transmit the norm are simultaneously agenda-setters (informal norm enforcers) and beneficiaries (their own kin's safety). No victim group is declared under this reading because no party's compliance is captured by another party's benefit — this is the central structural fact distinguishing a Rope from a Tangled Rope or Snare here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored 'live' (not dead) precisely because 2011 re-confirmed the hazard the marker was built to address, which forecloses the standard mandatrophy pattern (constraint persisting after its founding problem resolved). This is the structural crux separating the two kernel readings: the commemorative_husk_reading would more plausibly authors founding_problem_status as contested or dead-with-symbolic-residue, since under that reading the actual land-use behavior had already decoupled from the founding purpose well before 2011 and the 2011 outcome would be attributed to other siting determinants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_mechanism_of_2011_correlation,
    'Did the stone''s inscribed instruction causally produce the above-line siting pattern that determined 2011 survival, or did an independent variable (original 1933 resettlement geography, harbor-proximity tradeoffs, terrain cost gradients) produce both the siting pattern and the survival outcome, with the stone as a correlated but non-causal marker?',
    'Oral-history interviews with households who built after 1933 asking explicitly whether the stone''s text or the associated social narrative factored into their siting decision, cross-referenced against land-cost and terrain records to see whether above-line siting is explicable by cost/terrain alone without invoking normative compliance.',
    'If the correlation is substantially explained by terrain/cost factors independent of normative compliance, this reading''s low-epsilon behavioral-competence claim collapses toward the commemorative_husk_reading, and the constraint''s claimed coordination function would need to be re-evaluated as largely coincidental rather than operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_mechanism_of_2011_correlation, empirical, 'Whether the stone caused compliance or merely correlates with independently-caused siting patterns.').

omega_variable(
    natural_vs_constructed_beneficiary_structure,
    'Are the declared beneficiaries (residents who relocated, descendants) benefiting from a self-imposed prudential norm they freely chose to maintain, or is there an element of constructed social coercion (shame, exclusion, family pressure) that makes the ''beneficiary'' framing understate the suppression actually operating on individual builders who might have preferred lower-cost sites?',
    'Interview accounts of any documented instances of builders who chose to build below the line, and the social consequences (if any) they faced, would reveal whether the mechanism operates purely through informed voluntary compliance or partly through informal social sanction.',
    'If meaningful social sanction existed for non-compliance, suppression should be revised upward from the currently low authored value (0.12), which would move the classification closer to a mild tangled_rope (coordination benefit plus a real, if soft, cost imposed on dissenters) rather than a clean rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_beneficiary_structure, conceptual, 'Whether the low-suppression rope framing understates informal social coercion against non-compliant builders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 13, 0.03).
narrative_ontology:measurement(aney_tr_t27, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 27, 0.04).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 39, 0.05).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 52, 0.06).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 65, 0.07).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 78, 0.08).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 13, 0.04).
narrative_ontology:measurement(aney_be_t27, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 27, 0.04).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 39, 0.05).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 52, 0.05).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 65, 0.06).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 78, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint and aneyoshi_stone_commitment__commemorative_husk_reading are the two declared readings of a single kernel (the Aneyoshi tsunami stone and its inscribed instruction). They share the same physical artifact, the same 78-year interval, and the same surface 2011 outcome, but diverge on the causal-mechanism question: this file claims the stone retained active regulatory force on siting decisions (very low epsilon, rope classification); the sibling claims the stone decayed to symbolic/commemorative status with no operative constraint on land use (a structurally different, higher-epsilon or piton-flavored claim, authored separately). Per the ε-invariance principle, these are two constraints, not one constraint measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
