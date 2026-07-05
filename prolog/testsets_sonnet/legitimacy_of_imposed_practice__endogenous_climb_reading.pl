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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Endogenous-Climb Reading of Imposed Practice Legitimacy (Calendar/Dress Reform)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A modernizing state decrees a new solar calendar and Western-style dress
 *   to project sovereignty and administrative unity. Under the
 *   endogenous-climb reading, the outcome divergence between the two reforms
 *   is read as evidence for the internalization thesis: the calendar reform,
 *   imposed with minimal grassroots buy-in, produced only nominal compliance
 *   while lunar observance persisted in practice for decades; the dress
 *   reform achieved more durable partial adoption specifically where it
 *   diffused organically through urban social networks rather than purely
 *   through fines and inspection, though even there private retention at home
 *   shows incomplete internalization. The constraint names a genuine
 *   coordination function (administrative and diplomatic legibility) riding
 *   alongside asymmetric extraction (enforcement costs and dual-practice
 *   burden borne by rural and urban-compliance populations) sustained only by
 *   active enforcement — hence tangled_rope, not mountain or rope.
 *
 * KEY AGENTS:
 *   - reforming_state_apparatus: agenda-setter and primary claimed beneficiary of legibility, institutional power, arbitrage-grade exit
 *   - rural_lunar_calendar_communities: structural beneficiary of the reform's failure, powerless, trapped exit
 *   - urban_compliance_class: dual payer/beneficiary bearing the cost of parallel public/private practice
 *   - state_modernization_timeline: the non-agent entity that actually absorbs the cost of failed internalization
 *   - local_enforcement_officials: embedded intermediaries who both sustain and quietly undermine enforcement
 *   - historians_of_state_formation: analytical observers documenting the displacement/retention divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.42).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.61).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Endogenous-Climb Reading of Imposed Practice Legitimacy (Calendar/Dress Reform)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '97263316-4db3-4865-84bf-c3a823147ade').
narrative_ontology:cs_kernel_codification('97263316-4db3-4865-84bf-c3a823147ade', distributed).
narrative_ontology:cs_authority_grounding('97263316-4db3-4865-84bf-c3a823147ade', distributed).
narrative_ontology:cs_reading_relation('97263316-4db3-4865-84bf-c3a823147ade', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('97263316-4db3-4865-84bf-c3a823147ade', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('97263316-4db3-4865-84bf-c3a823147ade', foundational, internalization_is_necessary_for_durable_displacement).
narrative_ontology:cs_axiom_status(internalization_is_necessary_for_durable_displacement, holdable).
narrative_ontology:cs_axiom_grounding('97263316-4db3-4865-84bf-c3a823147ade', internalization_is_necessary_for_durable_displacement, empirically_contingent).
narrative_ontology:cs_axiom('97263316-4db3-4865-84bf-c3a823147ade', secondary, decree_absent_grassroots_pathway_produces_only_nominal_compliance).
narrative_ontology:cs_axiom_status(decree_absent_grassroots_pathway_produces_only_nominal_compliance, holdable).
narrative_ontology:cs_axiom_grounding('97263316-4db3-4865-84bf-c3a823147ade', decree_absent_grassroots_pathway_produces_only_nominal_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('97263316-4db3-4865-84bf-c3a823147ade', pre_reform_customary_practice_baseline).
narrative_ontology:cs_drift_state('97263316-4db3-4865-84bf-c3a823147ade', post_decree_generational_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97263316-4db3-4865-84bf-c3a823147ade', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, informal_local_authorities).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliance_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, rural_lunar_calendar_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliance_class).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, local_enforcement_officials).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, rural_lunar_calendar_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues decrees mandating the new calendar and dress code as markers of modernization and sovereignty, and deploys inspectors, fines, and administrative penalties to enforce compliance. Measures its own success by the visible compliance rate in cities, not by whether the practices are internalized. Retains the option to escalate enforcement or quietly tolerate informal noncompliance depending on political cost.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, reforming_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Continue observing lunar dates for planting, festivals, and religious obligations regardless of the decreed solar calendar, using the old calendar privately while nominally complying on official paperwork. They bear occasional fines or social penalty for visible noncompliance but preserve the practice that actually organizes their agricultural and ritual life, and in that sense benefit from the state's failure to displace it.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, rural_lunar_calendar_communities, beneficiary,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, rural_lunar_calendar_communities, payer).

% Adopts the new dress code publicly to access state employment, schooling, and urban social standing, while often reverting to traditional dress at home or in private gatherings. Pays the cost of maintaining two parallel practices — public performance and private retention — and absorbs the social friction between them without ever fully internalizing the state's intended meaning of the reform.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliance_class, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_compliance_class, beneficiary).

% The state's own projected schedule for achieving a modernized, unified national culture is the thing that pays the cost of failed internalization: each decade the lunar calendar persists or private dress reversion continues is a decade the timeline slips, forcing repeated re-legislation, renewed enforcement campaigns, and eventual quiet abandonment of full displacement as a policy goal.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline, payer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).

% Administer fines and inspections on behalf of the state but live embedded in the same communities they police. Frequently exercise discretion to under-enforce, accept token compliance, or negotiate informal exemptions, which lets them maintain local standing while formally satisfying reporting requirements upward — a position that both sustains and quietly undermines the reform.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, local_enforcement_officials, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, local_enforcement_officials, beneficiary).

% Study the divergence between decreed practice and lived practice across comparable modernization campaigns, documenting where displacement succeeded, stalled, or reversed. Their analysis is the primary evidence base for whether internalization pathways determine reform durability.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, historians_of_state_formation, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reform genuinely tries to solve a real coordination problem — synchronizing a fragmented population onto a single calendar and dress standard to enable unified administration, trade scheduling, and international legibility. Where internalization occurs, this coordination benefit is real.
% TRANSFER_FUNCTION: The arrangement transfers administrative simplicity and international legitimacy toward the state apparatus and urban elites who can perform compliance, while transferring enforcement costs, social friction, and the burden of maintaining dual practices onto rural communities and the urban compliance class.
% ABSENT_VOICES: Rural communities' own accounts of why the lunar calendar continues to organize their lives are rarely solicited by the state, which measures compliance through inspection reports rather than lived practice; their perspective would argue the old practice was never actually displaced, only nominally overwritten.
% DISAPPEARANCE_RATIONALE: If state enforcement of the calendar and dress decrees vanished overnight, urban public life would likely revert quickly toward whatever practices are already privately retained, suggesting the visible arrangement rearranges; but for rural communities where lunar observance never actually stopped, nothing would change at all — the verdict depends on which population's world is being asked about.
% FOUNDING_PROBLEM: The state needed to demonstrate sovereign modernity to international observers and unify administrative timekeeping and civic presentation across a fragmented population inheriting diverse regional and religious practices.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus attests the problem is solved wherever visible compliance is achieved. Independent historians of state formation and ethnographic fieldwork in rural communities attest that the underlying practices persisted essentially unchanged beneath compliant surfaces, and that the founding problem — genuine cultural displacement — remains substantially unsolved decades on; this corroboration comes from outside the state's own reporting apparatus.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).
:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-declining 0.55→0.42 because under this reading the state's extraction from the population (enforcement costs, social sorting, penalty exposure) diminishes over the interval as enforcement is quietly relaxed in the face of persistent noncompliance — the state cannot sustain high-cost enforcement against a practice that never internalized. Theater ratio rises 0.25→0.48 because an increasing share of the state's compliance apparatus becomes performative: inspection reports document formal adherence while everyone involved, including local officials, understands the underlying practice has not changed. Suppression starts high (0.78, active fines/inspections at rollout) and declines to 0.61 as enforcement capacity attrits — this is a raw structural property tracked independently of the extraction trend, per the framework's rule that suppression is not scaled by scope or power in authored data.
 *
 * PERSPECTIVAL GAP:
 *   From the reforming state's seat, the calendar and dress decrees look like an ongoing, if imperfect, coordination success — compliance statistics trend the right direction on paper. From the rural community's seat, and from the historian's analytical seat, the same arrangement looks like theater layered over an unchanged underlying practice, with the state's own enforcement apparatus effectively becoming the primary payer once its diminishing returns are counted. The engine should compute these as structurally different seat-level types from the same authored data, not as competing opinions about one type.
 *
 * DIRECTIONALITY LOGIC:
 *   Rural lunar-calendar communities are coded as beneficiaries because, under this reading, the state's failure to displace their practice preserves their functional autonomy — the constraint's operation (or non-operation) subsidizes their continued self-governance of time and ritual, even though they bear intermittent formal penalties. The state_modernization_timeline is coded as a non-agent payer: it is the abstract institutional schedule, not a person, that structurally absorbs the cost of slipped displacement, kept out of directionality computation as a real beneficiary/victim actor per the agent:false convention. The urban compliance class occupies a genuinely dual position — visible beneficiaries of access to state employment and status, but payers of the ongoing cost of maintaining incompatible public and private selves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereign modernization legibility) is authored as contested rather than flatly dead or live: the state apparatus insists the problem remains live and current compliance rates justify continued enforcement, while historians and rural testimony corroborate that the substantive problem — actual cultural displacement — was never solved and the visible mandate now functions mostly as institutional face-saving. This mismatch (status=contested, verdict=contested) is exactly the ambiguous case the R5 genealogy interview is built to surface rather than resolve by assertion; the framework routes it to omega variables rather than forcing a premature verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the calendar/dress reform outcome best explained by the endogenous-climb mechanism (internalization is necessary and its absence explains failure), or does the exogenous-override or hybrid-scaffolding reading better fit the same historical record?',
    'Comparative case analysis across multiple state-modernization episodes: if displacement success correlates strongly with the presence of organic grassroots adoption pathways independent of enforcement intensity, the endogenous-climb reading is favored; if displacement success correlates instead with enforcement duration/intensity alone, the exogenous-override reading is favored; if success correlates with the combination of decree plus sustained ideological messaging campaigns, the hybrid-scaffolding reading is favored.',
    'This story''s classification and metrics are authored specifically FOR the endogenous-climb reading. If the historical evidence better fits a sibling reading, that is not a refutation of this constraint but a signal that a different constraint (a sibling story) more accurately describes the same events — per the ε-invariance principle, this is a decomposition question, not a parameter to average over.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the internalization-necessity claim, rather than a sibling reading, is the correct structural account of this reform''s trajectory.').

omega_variable(
    calendar_vs_dress_delta_significance,
    'Is the observed difference between calendar failure and dress partial-success genuinely explained by differential internalization pathways (dress diffused organically via urban social networks; calendar did not), or by confounds such as differential enforcement cost, differential everyday salience, or differential entanglement with religious practice?',
    'Micro-historical tracing of adoption pathways for each practice: document whether dress adoption preceded, followed, or was independent of state enforcement campaigns in specific urban centers, and whether calendar rejection tracks religious/agricultural entanglement more than absence of a diffusion pathway.',
    'If confounds explain the delta as well as internalization does, the endogenous-climb reading''s evidentiary basis in this specific case weakens relative to the hybrid-scaffolding reading, which would attribute dress''s partial success to ideological messaging rather than pure organic diffusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(calendar_vs_dress_delta_significance, empirical, 'Whether the calendar/dress outcome divergence is genuine evidence for the internalization thesis or explainable by other factors.').

omega_variable(
    state_modernization_timeline_agency_status,
    'Is state_modernization_timeline correctly treated as a non-agent abstraction bearing costs, or does it functionally operate as a proxy for identifiable reformist factions within the state who have personal careers and legitimacy staked on the timeline?',
    'Prosopographical study of which officials'' careers were tied to reform completion targets; if a concentrated faction bears the cost personally, the abstraction should be decomposed into a named agent stakeholder.',
    'If the timeline cost is actually concentrated on an identifiable reformist faction, this constraint edges toward snare (concentrated victim) rather than tangled_rope, and that faction should be added as a named agent-payer stakeholder.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_modernization_timeline_agency_status, empirical, 'Whether the abstract institutional-timeline payer should be decomposed into named agent stakeholders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 32, 0.46).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 32, 0.43).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 8, 0.74).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimacy_of_imposed_practice kernel, each authored as a separate ε-invariant constraint per the decomposition principle: endogenous_climb_reading (this story) claims displacement requires internalization and reads the calendar failure/dress partial-success divergence as confirming evidence; exogenous_override_reading claims decree authority alone suffices and would read the same historical record through compliance-rate metrics without reference to internalization; hybrid_scaffolding_reading claims decree-plus-ideological-messaging achieves partial displacement and would attribute dress's partial success to messaging campaigns rather than organic diffusion. All three share the same underlying historical events but diverge in claimed mechanism, beneficiary/victim structure, and classification — they are linked here rather than merged because their ε values and structural claims are genuinely distinct, not measurement variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
