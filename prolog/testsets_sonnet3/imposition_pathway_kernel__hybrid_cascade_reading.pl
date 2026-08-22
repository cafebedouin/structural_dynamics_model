% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: Meiji Calendar Reform as State-Manufactured Fringe Cascading to Organic Adoption
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This story instantiates the hybrid_cascade_reading of the
 *   imposition_pathway_kernel: a contested question in historical sociology
 *   about how commitment-system displacement (here, calendar and associated
 *   administrative-practice reform in Meiji Japan) actually propagates
 *   through a society. The hybrid reading holds that the 1873 Meiji decree
 *   mandating the Gregorian calendar for government and military personnel is
 *   best modeled as a two-phase mechanism — a coercive, top-down override
 *   that MANUFACTURES an artificial fringe population (conscripted soldiers,
 *   provincial clerks, state functionaries with no exit) which then serves as
 *   the seed population for a subsequent, genuinely organic climb through the
 *   wider population over the following decades. The override does the fast,
 *   coercive work of creating the initial fringe; the climb does the slow,
 *   voluntary work of expanding adoption outward from that fringe. Extraction
 *   and suppression are therefore front-loaded at the decree moment and decay
 *   as the climb phase takes over — a signature this reading treats as
 *   diagnostic and the sibling readings would not predict in the same shape.
 *
 * KEY AGENTS:
 *   - meiji_state_administrators: agenda_setter (institutional/analytical) — issues and administers the decree
 *   - conscripted_military_personnel: primary target (powerless/trapped) — the manufactured fringe bearing the compressed transition cost
 *   - provincial_government_clerks: secondary target-turned-beneficiary (powerless/constrained) — pays short-term, gains long-term credentialing advantage
 *   - modernizing_elite_reformers: primary beneficiary (powerful/arbitrage) — captures reputational and diplomatic gains
 *   - rural_agricultural_population: excluded — not consulted, absorbs climb pressure indirectly decades later
 *   - historical_sociologists: analytical observer — tests which mechanism class explains the adoption curve
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.42).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.61).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "Meiji Calendar Reform as State-Manufactured Fringe Cascading to Organic Adoption").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, '292a43f3-de1a-4367-8161-5591401f4654').
narrative_ontology:cs_kernel_codification('292a43f3-de1a-4367-8161-5591401f4654', formalized).
narrative_ontology:cs_authority_grounding('292a43f3-de1a-4367-8161-5591401f4654', extraction).
narrative_ontology:cs_interpretation_layer_present('292a43f3-de1a-4367-8161-5591401f4654').
narrative_ontology:cs_reading_relation('292a43f3-de1a-4367-8161-5591401f4654', imposition_pathway_kernel__endogenous_climb_reading, influences).
narrative_ontology:cs_reading_relation('292a43f3-de1a-4367-8161-5591401f4654', imposition_pathway_kernel__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('292a43f3-de1a-4367-8161-5591401f4654', foundational, override_and_climb_are_sequential_not_alternative_mechanisms).
narrative_ontology:cs_axiom_status(override_and_climb_are_sequential_not_alternative_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('292a43f3-de1a-4367-8161-5591401f4654', override_and_climb_are_sequential_not_alternative_mechanisms, empirically_contingent).
narrative_ontology:cs_axiom('292a43f3-de1a-4367-8161-5591401f4654', secondary, state_manufactured_fringe_is_a_distinct_seeding_class).
narrative_ontology:cs_axiom_status(state_manufactured_fringe_is_a_distinct_seeding_class, holdable).
narrative_ontology:cs_axiom_grounding('292a43f3-de1a-4367-8161-5591401f4654', state_manufactured_fringe_is_a_distinct_seeding_class, empirically_contingent).
narrative_ontology:cs_reference_frame('292a43f3-de1a-4367-8161-5591401f4654', pre_meiji_lunisolar_administrative_order).
narrative_ontology:cs_drift_state('292a43f3-de1a-4367-8161-5591401f4654', post_decree_compliance_deadline, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('292a43f3-de1a-4367-8161-5591401f4654', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, meiji_state_administrators).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, modernizing_elite_reformers).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, international_trade_partners).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_military_personnel).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, provincial_government_clerks).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, traditional_calendar_ritual_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, provincial_government_clerks).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, state_capacity_as_climb_accelerant).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, compressed_climb_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue the decree mandating the Gregorian calendar (and associated commitment practices) for government employees and military personnel as of a fixed date. They administer the transition, absorb the diplomatic and fiscal costs of the switch (notably eliminating a 13th lunar month in a budget year), and use the mandated fringe as a demonstration population for the rest of society.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, meiji_state_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Ordered to adopt the new calendar and associated administrative practices immediately upon decree, with no meaningful say in the timing or method. They bear the disorientation costs of the compressed transition — pay schedules, leave calculations, and ritual observances all disrupted overnight — and function, whether they intend to or not, as the visible early-adopter population the state uses to seed wider social climb.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_military_personnel, payer,
    powerless, immediate, trapped, national).

% Required by administrative order to convert records, tax schedules, and correspondence to the new system. They pay the immediate retraining and error-correction cost of enforced compliance, but over the following decade some gain professional advantage as early fluency in the new system becomes a marketable skill for provincial administrative careers — the same fringe imposition that burdens them in year one becomes a credential by year ten.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, provincial_government_clerks, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, provincial_government_clerks, beneficiary).

% Their professional and ritual authority rested on expertise in the lunisolar calendar's festival and auspicious-day calculations. State adoption of the new system for official purposes does not outlaw their practice but strips it of governmental sanction, shrinking their client base to those who continue to value traditional reckoning outside state functions.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, traditional_calendar_ritual_specialists, payer,
    moderate, biographical, constrained, national).

% Advocate for the reform as part of a broader modernization project aimed at parity with Western powers. They benefit reputationally and diplomatically from rapid, visible institutional Westernization and can move between government, business, and academic roles regardless of how the transition unfolds on the ground.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, modernizing_elite_reformers, beneficiary,
    powerful, civilizational, arbitrage, national).

% Benefit from Japan's calendar and administrative synchronization with Western commercial and diplomatic conventions, which reduces friction in trade scheduling, treaty administration, and shipping coordination. They neither administer nor bear the domestic transition cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, international_trade_partners, beneficiary,
    institutional, generational, analytical, global).

% Not directly targeted by the decree and largely continue lunisolar practice for agricultural and festival purposes for decades. They have no seat in the decision and are not consulted, yet eventually experience climb pressure indirectly as state, school, and market calendars normalize around them.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, rural_agricultural_population, excluded,
    powerless, generational, constrained, regional).

% Study the Meiji transition as a case in commitment-system displacement, comparing it against purely organic climb cases and purely coercive override cases to test which mechanism class better explains the observed adoption curve.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, diffuse).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizing state administrative time-reckoning with the international (Western) commercial and diplomatic calendar solves a genuine coordination problem for treaty administration, trade scheduling, and government payroll — a single official standard removes the need for constant bilateral date translation.
% TRANSFER_FUNCTION: Moves the cost of transition (retraining, disorientation, loss of professional standing) from the state onto the mandated fringe population — military and government personnel — while moving diplomatic and commercial legibility benefits to reformist elites and foreign trade partners; the fringe population's forced early adoption is then repurposed as a demonstration effect that lowers the adoption cost for the wider population.
% ABSENT_VOICES: Rural agricultural communities and traditional ritual specialists were not consulted on the decree's timing or scope; they would have objected to the elimination of the intercalary month (a direct fiscal loss for salaried employees paid monthly) and to the delegitimization of lunisolar ritual calculation, but neither group held a seat in the Meiji state's reform councils.
% DISAPPEARANCE_RATIONALE: Had the decree and its enforced fringe never occurred, calendar reform in Japan would likely have proceeded (if at all) through slower diffusion via merchants, missionaries, and returning students — a genuinely organic climb with a much longer timeline and a different, less state-centered set of early adopters; government payroll, military logistics, and treaty administration would have remained on dual or lunisolar reckoning far longer, materially altering the pace of administrative modernization.
% FOUNDING_PROBLEM: The Meiji state needed rapid, legible synchronization with Western administrative and commercial time-reckoning to support treaty revision, foreign trade, and international military coordination, and needed to demonstrate modernization capacity domestically and internationally.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic historians studying Meiji-era administrative reform corroborate that the coordination problem (calendar mismatch with treaty partners) was resolved within the first years of the decree; the ongoing social climb of calendar adoption among the general population that followed for decades afterward was not required by the original coordination problem and is attested by demographic and folklore-survival studies as a distinct, slower-moving process outside the founding administrative rationale.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).
:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42, falling from 0.55 at the decree moment to a floor near 0.40 as suppression requirement decays sharply (0.85 to 0.25) over the interval — capturing the hybrid model's prediction that active coercion is needed to seed the fringe but drops away once climb dynamics take over. Theater ratio rises modestly (0.15 to ~0.30) as the state increasingly performs 'organic modernization' rhetoric to narrate what was in fact a manufactured fringe, then plateaus once the climb becomes self-sustaining and the performative narrative is less needed. Suppression as an authored scalar (0.61) reflects the story's overall interval-average enforcement intensity, distinct from the temporal suppression_requirement series which tracks its decay.
 *
 * PERSPECTIVAL GAP:
 *   From the meiji_state_administrators' seat, the reform looks like coordination that solved a real synchronization problem and is now largely complete — successful modernization. From the conscripted_military_personnel and provincial_clerks' seats at the moment of imposition, it looks like coercive override with zero consultation and immediate, involuntary cost. The hybrid_cascade_reading's structural claim is that BOTH seats are describing the same constraint honestly at different phases: the override phase and the climb phase are sequential, not competing, descriptions.
 *
 * DIRECTIONALITY LOGIC:
 *   Meiji state administrators sit at the beneficiary end: institutional power, analytical exit (they are not themselves subject to the decree's disruption in the way conscripts are), and they control the decree's timing and scope. Conscripted military personnel sit at the full-target end: powerless, trapped, immediate time horizon — they absorb the compressed disorientation cost with zero say. Provincial clerks are declared dual-role (payer + beneficiary) because the SAME imposition that burdens them in year one becomes a career credential by year ten — the directionality is genuinely time-varying within a single agent, which the schema captures via secondary_role rather than an override. Modernizing elites and international trade partners are beneficiaries with high exit (arbitrage/analytical) — they are structurally insulated from the transition costs they advocate for.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (calendar synchronization for treaty and trade legibility) was resolved within the first several years of the decree — status is authored as 'dead.' But the mandated-fringe mechanism this reading isolates did not stop operating once that problem was solved: it continued generating climb pressure on the rural population for decades afterward, well past the point where the original coordination rationale applied. Classifying this as tangled_rope (not scaffold) is deliberate: there was no declared sunset, and the enforcement apparatus that manufactured the fringe was never formally retired — it simply decayed in suppression_requirement as climb dynamics took over the work, which is a different thing from a planned transition ending on schedule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phase_boundary_identifiability,
    'Can the override phase and the climb phase be empirically distinguished in the historical record, or is the ''two-phase'' structure this reading proposes an artifact of choosing where to draw the boundary?',
    'Fine-grained adoption-rate data by social group and region, cross-referenced against decree-compliance deadlines: a genuine two-phase structure should show a sharp compliance spike among mandated fringe groups at the deadline followed by a distinct, slower-sloped diffusion curve among non-mandated groups. A single smooth curve across both populations would favor the endogenous_climb_reading; a curve showing no diffusion beyond the mandated groups within the observed window would favor the exogenous_override_reading.',
    'If no clean phase boundary is identifiable, the hybrid reading''s central structural claim collapses into whichever sibling reading better fits the single observed curve, and this constraint should be understood as a redescription of one sibling rather than an independent structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phase_boundary_identifiability, empirical, 'Whether the override and climb phases are separable in the adoption data or an artifact of periodization choice.').

omega_variable(
    fringe_manufacture_intentionality,
    'Did the Meiji state deliberately design the military/government mandate AS a seeding mechanism for wider social climb, or is the cascade-to-climb effect an unintended byproduct of a narrower administrative-synchronization decision?',
    'Archival review of Meiji cabinet deliberations and contemporaneous reformist writing for explicit discussion of the mandate''s expected demonstration effects on the broader population, versus purely administrative/fiscal justifications.',
    'If intentional, the tangled_rope classification is strongly supported (a coordination cover story masking deliberate extraction-for-diffusion design). If unintentional, the case is closer to a rope with an unplanned extractive side-effect, which would weaken the requires_active_enforcement framing for the later climb phase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fringe_manufacture_intentionality, conceptual, 'Whether the fringe-to-climb cascade was designed or emergent.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the imposition_pathway_kernel''s three-way split (endogenous/exogenous/hybrid) itself well-founded, or does the Meiji case admit a fourth framing — e.g., multiple independent override events across different domains (calendar, dress, military organization) each with their own separable climb dynamics — that none of the three declared readings captures?',
    'Comparative case analysis across the several distinct Meiji-era reform decrees (calendar, conscription, dress codes, education) to test whether a single M-set cell type fits all of them or whether domain-specific hybrid patterns diverge enough to warrant further kernel decomposition.',
    'If a fourth framing is warranted, this reading (and its siblings) may need further decomposition into domain-specific constraints rather than being treated as exhaustive of the imposition_pathway_kernel''s structural space.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three declared kernel readings exhaust the plausible structural framings of Meiji-era top-down reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(impo_tr_t4, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(impo_be_t4, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(impo_su_t4, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 40, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__hybrid_cascade_reading, 0.1).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of imposition_pathway_kernel, each instantiating a different structural claim about how the same Meiji-era calendar reform propagated. endogenous_climb_reading denies any distinct override mechanism, treating the whole episode as compressed organic climb. exogenous_override_reading denies any climb-mechanism contribution, treating the whole episode as pure state-capacity-driven override. This hybrid_cascade_reading claims both mechanisms operate sequentially — override creates the fringe, climb completes the diffusion — and is authored with its own distinct ε (0.42, moderate, decaying) reflecting front-loaded coercion cost that tapers as climb dynamics take over. Do not average or blend ε across the three readings; each is a separate constraint with a separate, ε-invariant claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
