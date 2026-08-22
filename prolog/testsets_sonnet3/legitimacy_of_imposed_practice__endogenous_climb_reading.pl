% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: State-Decreed Practice Displacement Without Endogenous Adoption Pathway (Endogenous Climb Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A modernizing state decrees displacement of a lunar calendar and
 *   traditional dress by legal mandate, treating the decree's existence as
 *   sufficient cause for the underlying practice to change. This reading
 *   holds that practice displacement structurally requires internalization
 *   through bottom-up adoption pathways — consultation with local
 *   authorities, gradual generational uptake, integration with existing
 *   social and religious meaning-structures — and that decree without such a
 *   pathway produces only surface compliance that decays into permanent
 *   theater. Two structural outcomes evidence this: the calendar reform fails
 *   outright (lunar observance persists for decades in rural and religious
 *   use), while the dress reform achieves partial urban diffusion but
 *   coexists with widespread private retention of the older dress, signaling
 *   that even the 'successful' half of the reform never achieved true
 *   internalization. This is ONE of three readings of a contested kernel
 *   about the legitimacy and mechanism of imposed practice displacement; the
 *   sibling readings (exogenous_override_reading, hybrid_scaffolding_reading)
 *   are separate constraint files with their own ε and structural data, not
 *   alternative interpretations folded into this one.
 *
 * KEY AGENTS:
 *   - reform_state_authority: Primary agenda-setter (institutional/arbitrage) — decrees displacement, measures success by decree existence rather than outcome
 *   - reform_bureaucracy_symbolic_capital: Beneficiary (institutional/arbitrage) — collects career and status capital from administering the reform regardless of actual uptake
 *   - urban_compliance_class: Payer/secondary beneficiary (moderate/constrained) — bears dual-system cost of public performance and private retention
 *   - rural_lunar_calendar_users: Primary payer (powerless/trapped) — bears ongoing legal and administrative friction from failed displacement with no adoption pathway ever offered
 *   - communities_preserving_autonomy: Beneficiary (organized/constrained) — preserves genuine cultural continuity as a byproduct of the reform's structural failure
 *   - state_modernization_timeline: Non-agent payer — absorbs the cost of a developmental program built on an assumption of completed displacement that never occurred
 *   - historians_of_the_reform: Analytical observer — documents the gap between reported compliance and actual internalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.71).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "State-Decreed Practice Displacement Without Endogenous Adoption Pathway (Endogenous Climb Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '886d5c3a-3868-4997-9261-80320acc7d6c').
narrative_ontology:cs_kernel_codification('886d5c3a-3868-4997-9261-80320acc7d6c', distributed).
narrative_ontology:cs_authority_grounding('886d5c3a-3868-4997-9261-80320acc7d6c', distributed).
narrative_ontology:cs_reading_relation('886d5c3a-3868-4997-9261-80320acc7d6c', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('886d5c3a-3868-4997-9261-80320acc7d6c', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('886d5c3a-3868-4997-9261-80320acc7d6c', foundational, displacement_requires_internalization).
narrative_ontology:cs_axiom_status(displacement_requires_internalization, holdable).
narrative_ontology:cs_axiom_grounding('886d5c3a-3868-4997-9261-80320acc7d6c', displacement_requires_internalization, empirically_contingent).
narrative_ontology:cs_axiom('886d5c3a-3868-4997-9261-80320acc7d6c', secondary, decree_without_adoption_pathway_produces_theater_not_change).
narrative_ontology:cs_axiom_status(decree_without_adoption_pathway_produces_theater_not_change, holdable).
narrative_ontology:cs_axiom_grounding('886d5c3a-3868-4997-9261-80320acc7d6c', decree_without_adoption_pathway_produces_theater_not_change, empirically_contingent).
narrative_ontology:cs_reference_frame('886d5c3a-3868-4997-9261-80320acc7d6c', pre_reform_customary_practice_baseline).
narrative_ontology:cs_drift_state('886d5c3a-3868-4997-9261-80320acc7d6c', post_reform_multigenerational_assessment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('886d5c3a-3868-4997-9261-80320acc7d6c', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, reform_bureaucracy_symbolic_capital).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliance_class).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, rural_lunar_calendar_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliance_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees the new calendar and dress code by legal mandate, treating the decree itself as sufficient to produce displacement of the prior practice. Measures its own success by the existence of the law and the visibility of compliance in the capital, not by whether the underlying calendrical or sartorial commitments have actually shifted in the population's daily reckoning.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, reform_state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Civil servants and officials whose careers and prestige are built on administering and certifying the reform's rollout — inspection tours, compliance statistics, ceremonial launches. They collect status and promotion from the decree's existence regardless of whether internalization occurs; their incentive is to report success, not to build the bottom-up pathway that would make success real.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, reform_bureaucracy_symbolic_capital, beneficiary,
    institutional, biographical, arbitrage, national).

% Urban professionals and civil servants who adopt the new dress and calendar publicly to retain employment, access to institutions, and legal standing, while privately retaining the older garments and reckoning the lunar calendar at home. They bear the cost of maintaining two parallel systems — the performative public one and the internalized private one — without the state ever crediting or resourcing that dual burden.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliance_class, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliance_class, beneficiary).

% Agricultural and rural communities whose planting cycles, religious observance, and market timing remain organized around the lunar calendar decades after the decree. They face legal exposure, exclusion from state services, and administrative friction (mismatched contracts, tax dates, school terms) for continuing a practice they never had a pathway to abandon on terms that served them, but they lack the exit or leverage to force accommodation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, rural_lunar_calendar_users, payer,
    powerless, generational, trapped, regional).

% Villages, guilds, and religious communities that successfully resist displacement by maintaining the prior practice underground or in parallel, preserving a degree of self-governance and continuity of custom that the decree tried to erase. Their persistence is a genuine gain for cultural continuity, purchased at the price of permanent friction with the state and exclusion from some formal benefits.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy, beneficiary,
    organized, generational, constrained, regional).

% The state's own developmental program — the schedule by which it claimed it would achieve a modernized, uniform administrative and cultural base — absorbs the cost of the failed displacement. Decades pass with the calendar unreformed in practice and dress only superficially adopted, so the timeline itself is the casualty: plans built on the assumption of completed displacement (unified scheduling, legal uniformity, administrative simplification) never materialize.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline, payer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).

% Scholars examining the decree's actual uptake decades later, comparing official compliance statistics against ethnographic and demographic evidence of persistent lunar-calendar use and private dress retention. They document the gap between decreed and internalized practice that the state's own reporting apparatus was structurally motivated to obscure.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, historians_of_the_reform, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle the decree tries to solve a genuine coordination problem — a single national calendar and dress standard would reduce transaction costs in administration, commerce, and cross-regional contact. That coordination gain is real in the abstract but was never realized because the mechanism chosen (legal fiat without a bottom-up adoption pathway) cannot actually produce the coordinated behavior it names.
% TRANSFER_FUNCTION: Moves compliance costs and legal exposure from the state (which bears none of the cost of building genuine adoption pathways) onto rural lunar-calendar communities and the urban compliance class, who must absorb the burden of either resisting, faking compliance, or maintaining dual systems. Symbolic capital and career credit flow to the reform bureaucracy regardless of actual outcomes.
% ABSENT_VOICES: Rural communities whose planting, market, and ritual calendars were never consulted in designing the transition; local religious and customary authorities who could have served as internalization vectors were excluded from the reform's design, and their absence is a primary reason the decree could not climb from the bottom up.
% DISAPPEARANCE_RATIONALE: If the decree and its enforcement apparatus vanished, the urban compliance class would drop the costly performative public adoption immediately (many already do so privately), lunar-calendar communities would face far less legal friction, and the state's reporting apparatus would lose its primary justification — but the underlying calendar and dress practices would not meaningfully change further, since they were never actually displaced. The world that rearranges is mostly the enforcement and reporting layer, not the practice itself.
% FOUNDING_PROBLEM: The state sought to modernize national administration and project a unified, forward-facing cultural identity by displacing a calendar and dress code seen as backward or foreign-associated, believing legal decree could accomplish what social internalization would otherwise take generations to achieve.
% FOUNDING_PROBLEM_CORROBORATION: The reform bureaucracy attests the problem is solved, citing formal legal compliance and urban visibility. Independent historians and ethnographers working outside the state apparatus, along with rural community elders and demographic survey data collected decades after the decree, attest that the underlying practice was never displaced — the calendar persisted in agricultural and religious use, and private dress retention among the urban compliance class was widespread and enduring.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.58 at interval end) because the constraint's persistence — continued legal exposure for lunar-calendar users, continued dual-burden costs for the urban compliance class — extracts real costs from powerless and moderate-power agents without producing the coordination benefit (a genuinely displaced, uniformly adopted practice) that would justify it. Theater ratio is authored high and rising (0.40 to 0.62) because an increasing share of the reform's visible activity — inspection tours, compliance statistics, ceremonial displays — substitutes for actual internalization, which per this reading's core claim never occurs without a bottom-up pathway that was never built. Suppression is authored high (0.71) because maintaining even the surface compliance requires active legal and administrative enforcement against a population whose underlying practice never changed. Accessibility collapse is authored low-moderate (0.35) precisely because this reading's claim is that alternatives to the decreed practice never actually collapsed — lunar observance persisted as a live, practiced alternative for decades, which is the central evidentiary fact this reading rests on.
 *
 * PERSPECTIVAL GAP:
 *   From the reform_state_authority's seat, the decree looks like successful coordination — a uniform standard proclaimed and administratively enforced. From the rural_lunar_calendar_users' seat, the same structure is unresolved extraction: legal exposure imposed on a practice that was never actually displaced, with no accommodation offered. The engine computing these as different types from the same structural data is exactly the point of this reading — the state's own compliance statistics are compatible with 'success' only if internalization is not measured, and this reading insists it must be.
 *
 * DIRECTIONALITY LOGIC:
 *   The reform_state_authority and reform_bureaucracy sit at the beneficiary end of directionality — they collect legitimacy, career capital, and administrative uniformity credit from the decree's existence, largely independent of whether displacement actually occurred. Rural lunar-calendar users sit at the full-target end: trapped exit, no adoption pathway was ever built for them, and they bear ongoing legal friction for practicing what they never had a route to abandon on workable terms. The urban compliance class occupies an intermediate, dual position — beneficiaries of continued institutional access purchased at the cost of maintaining a costly parallel private practice. Communities preserving autonomy are a genuine beneficiary class under this reading, but their benefit (continuity of custom) is a byproduct of the reform's failure, not something the state intended to provide.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a unified, modernized calendar and dress standard) may remain genuinely live as a state objective, but the specific decree-only mechanism has failed to solve it for decades — this is captured by founding_problem_status: contested, since the bureaucracy claims success while ethnographic and demographic evidence says otherwise. Reading the constraint as tangled_rope rather than pure snare acknowledges the abstract coordination value the reform aims at is real, while the asymmetric extraction (rural and urban payers bearing costs with no return) is also real and requires active enforcement to sustain — exactly the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_threshold_ambiguity,
    'What observable threshold distinguishes ''partial urban adoption with private retention'' (incomplete internalization, per this reading) from ''successful gradual displacement in progress'' (which the hybrid_scaffolding_reading might claim is simply slower than expected)?',
    'Multi-generational tracking of private dress and calendar use within the urban compliance class: if private retention persists or is transmitted to children rather than fading, this reading''s failure verdict holds; if it fades within one to two generations, the hybrid reading''s partial-success account gains support.',
    'If private retention fades naturally over time, the tangled_rope classification here may overstate extraction — the arrangement would be better read as slow-but-real coordination (closer to scaffold) rather than persistent extraction. If retention persists indefinitely, the tangled_rope reading is confirmed and may even understate the eventual snare-like entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_threshold_ambiguity, empirical, 'Whether observed partial dress adoption represents failure or slow success.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three kernel readings (endogenous_climb, exogenous_override, hybrid_scaffolding) locate their disagreement — is it about the causal mechanism of displacement (what actually makes practice change), or about the normative standard for judging the reform''s legitimacy (whether decree-only imposition is ever justified regardless of outcome)?',
    'Compare the three readings'' treatment of the same evidentiary record (calendar failure, dress partial-adoption): if all three agree on the facts but differ only on evaluative framing, the disagreement is normative; if they dispute the facts themselves (e.g., whether dress adoption counts as internalized), the disagreement is causal/empirical.',
    'If the disagreement is purely normative, all three readings could in principle share one ε and differ only in evaluative gloss — which would violate ε-invariance and require re-examining whether these are genuinely three constraints or one constraint with three commentaries. If causal, the three-constraint decomposition is well-founded, as authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locating whether kernel readings differ on facts or on evaluation.').

omega_variable(
    communities_preserving_autonomy_benefit_reality,
    'Is the benefit accruing to communities_preserving_autonomy a genuine net gain (successful resistance preserving valuable self-governance) or a compensating cost (isolation and exclusion from state services that happens to look like autonomy from outside)?',
    'Compare economic and social outcomes (access to services, market integration, legal protection) for autonomy-preserving communities against comparable communities that fully complied, controlling for other confounds.',
    'If autonomy-preservation correlates with genuine wellbeing gains, the beneficiary classification is well-founded. If it correlates with material deprivation from exclusion, the ''beneficiary'' framing should be reconsidered as another victim class experiencing costs differently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communities_preserving_autonomy_benefit_reality, empirical, 'Whether preserved autonomy is a real benefit or a disguised cost of exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 16, 0.53).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 32, 0.61).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimacy_of_imposed_practice kernel, decomposed per the ε-invariance principle because the three readings assign structurally different extraction profiles to the same nominal event (a state's calendar and dress reform). This reading (endogenous_climb) authors moderate-high extraction (0.58) driven by failed displacement and rising theater; the exogenous_override_reading is expected to author lower extraction if decree compliance alone is taken as sufficient success; the hybrid_scaffolding_reading is expected to author an intermediate profile reflecting partial success through ideological reinforcement. All three share the same underlying historical record but diverge on which observable (decree existence, actual internalization, or scaffolded partial uptake) counts as the referent for extraction — resolved here by writing three separate files rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
