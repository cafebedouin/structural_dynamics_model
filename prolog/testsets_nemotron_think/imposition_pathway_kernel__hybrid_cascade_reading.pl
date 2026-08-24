% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Hybrid Cascade Pathway: State-Manufactured Fringe as Organic Climb Vector
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The hybrid_cascade_reading instantiates one reading of the
 *   imposition_pathway_kernel: top-down imposition creates an artificial
 *   fringe (state employees, military) which then climbs organically; the
 *   override initiates, the climb completes. The Meiji decree (1871-1873)
 *   mandated adoption of Western dress, haircuts, and behavioral codes by
 *   government officials and conscripted soldiers. This state-manufactured
 *   fringe became the vector for organic diffusion through society —
 *   teachers, police, village heads, merchants adopted the new codes because
 *   the fringe made them visible and legitimate. The M-set framework codes
 *   this as a compressed climb with state-manufactured fringe. The constraint
 *   is claimed as a mountain (universal law of state formation) but declares
 *   beneficiaries (state_elites, regime_institutions), triggering False
 *   Summit Mountain evaluation. The extraction is high initially (coercive
 *   imposition), declines as the climb naturalizes, then rises again in the
 *   analytical era as the framework itself becomes a tool for engineering
 *   commitment displacement in development contexts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.62).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.68).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, mountain).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "Hybrid Cascade Pathway: State-Manufactured Fringe as Organic Climb Vector").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).
domain_priors:emerges_naturally(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, 'd61f4414-ccc2-4318-bb97-b52e3cf7d5bb').
narrative_ontology:cs_kernel_codification('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', distributed).
narrative_ontology:cs_authority_grounding('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', expertise).
narrative_ontology:cs_interpretation_layer_present('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb').
narrative_ontology:cs_reading_relation('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', imposition_pathway_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', imposition_pathway_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_axiom('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', foundational, state_manufactured_fringe_enables_organic_climb).
narrative_ontology:cs_axiom_status(state_manufactured_fringe_enables_organic_climb, holdable).
narrative_ontology:cs_axiom_grounding('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', state_manufactured_fringe_enables_organic_climb, empirically_contingent).
narrative_ontology:cs_axiom('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', secondary, imposition_and_climb_are_inseparable_phases).
narrative_ontology:cs_axiom_status(imposition_and_climb_are_inseparable_phases, holdable).
narrative_ontology:cs_axiom_grounding('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', imposition_and_climb_are_inseparable_phases, empirically_contingent).
narrative_ontology:cs_reference_frame('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', meiji_cascade_paradigm).
narrative_ontology:cs_drift_state('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', contemporary_mset_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d61f4414-ccc2-4318-bb97-b52e3cf7d5bb', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, state_elites).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, regime_institutions).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, imperial_bureaucracy).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, subject_populations).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, civil_society_actors).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, traditional_status_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_military).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, state_employees).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_military).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, state_employees).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, hybrid_cascade_pathway).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, state_manufactured_fringe_mechanism).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, compressed_climb_with_artificial_fringe).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue top-down decrees (e.g., Meiji 1871-1873 edicts) mandating adoption of new status markers, dress, and behavioral codes by government officials and military personnel. They control the legislative and coercive apparatus that creates the artificial fringe and benefit from the resulting commitment displacement that legitimizes the new order.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% The bureaucracy, military, and educational apparatus that administer the imposed codes. Their institutional identity fuses with the new commitments; they cannot exit without dissolving the organization they embody. They gain structural coherence and resource access from the cascade but are bound to maintain it.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, regime_institutions, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, regime_institutions, agenda_setter).

% Mid-level officials who implement the decrees and police compliance. They receive career advancement and status from enforcing the new codes. Exit means leaving the only professional world they know; their livelihood and professional identity are locked to the cascade.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, imperial_bureaucracy, beneficiary,
    organized, biographical, constrained, national).

% Soldiers and officers required to adopt Western uniforms, drill, and rank structures by decree. They bear the immediate cost of abandoning traditional warrior identity (samurai status, topknots, swords). The military becomes the vector: their visible compliance signals the new order to society, and some gain status in the new hierarchy, fusing their identity to the cascade.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_military, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_military, beneficiary).

% Civil servants mandated to cut topknots, wear Western dress, and adopt new behavioral codes. They lose traditional status markers and face social sanction from their communities. Over time, the new codes become their professional identity; the cost of exit becomes existential (loss of position, pension, social recognition).
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, state_employees, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, state_employees, beneficiary).

% The general populace who encounter the cascade through the now-converted fringe: teachers, police, tax collectors, village heads. They did not choose the initial imposition but face a social world where the new commitments are now 'organic' and ubiquitous. Exit means emigration or total withdrawal from public life — effectively impossible.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, subject_populations, payer,
    powerless, biographical, trapped, national).

% Merchants, religious figures, local notables, and journalists who must navigate the new commitment landscape. Some adapt strategically (constrained exit), gaining access to new networks; others resist and are marginalized. The cascade remakes the field of legitimate action, extracting compliance from those who never consented to the original decree.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, civil_society_actors, payer,
    moderate, biographical, constrained, national).

% Samurai, court nobility, Buddhist clergy, and village elders whose authority derives from the old commitment order. They are not consulted on the decree; their objection is structurally irrelevant to the cascade's initiation. Some are co-opted into the new bureaucracy (becoming state_employees), others are suppressed. Their exclusion is what makes the artificial fringe 'artificial' — it bypasses the endogenous fringe adoption the endogenous_reading posits.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, traditional_status_groups, excluded,
    moderate, generational, trapped, regional).

% Scholars analyzing the Meiji case and the broader imposition pathway kernel. They debate whether the hybrid cascade is a universal mechanism, a Meiji-specific artifact, or an analytical construct. Their exit is analytical — they can change frameworks — but their professional reputation may be invested in a particular reading.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% Scholars committed to the endogenous_climb_reading who argue all commitment displacement is fringe-driven; top-down cases are compressed climbs with invisible fringe stages. They would object to the hybrid reading's claim that state manufacture of fringe is a distinct mechanism. Their professional identity is fused to the endogenous framework; conceding the hybrid case threatens their research program.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, endogenous_reading_proponents, excluded,
    organized, generational, identity_locked, universal).

% Scholars committed to the exogenous_override_reading who argue state capacity enables displacement without fringe adoption. They would object to the hybrid reading's claim that the override WORKS THROUGH creating an artificial fringe that then climbs. Their framework requires a clean separation between imposition and climb; the hybrid reading blurs it.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, exogenous_reading_proponents, excluded,
    organized, generational, identity_locked, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of rapid commitment displacement at scale: how a state can replace a deep, widely-held commitment order (Confucian status hierarchy, samurai identity) with a new one (national citizenship, bureaucratic meritocracy) without waiting for endogenous fringe adoption, which would take generations or fail. The artificial fringe provides immediate critical mass; the organic climb then diffuses the new commitments through existing social networks.
% TRANSFER_FUNCTION: Moves legitimacy, status, and resource access from traditional status groups (samurai, court nobility, religious authorities) to regime institutions and their personnel. The state extracts compliance from subject_populations and civil_society_actors by manufacturing a converted fringe (state_employees, conscripted_military) that makes the new commitments appear organic and inevitable. The transfer is enforced by decree initially, then sustained by the climb's social pressure.
% ABSENT_VOICES: Traditional status groups (samurai, court nobility, Buddhist clergy) were excluded from the decree process; their objection was structurally irrelevant. Peasant communities and urban commoners had no representation in the Meiji oligarchy. In the academic debate, voices from non-state-centered historical traditions (subaltern studies, anarchist anthropology) are largely absent from the M-set framework's development.
% DISAPPEARANCE_RATIONALE: If the hybrid cascade mechanism vanished overnight, the Meiji state would lack its primary engine for rapid commitment displacement. The new order would either fail to consolidate (reversion to Tokugawa patterns) or require a different, likely more violent, displacement mechanism. The cascade is not a passive description — it is the operational logic that made the Meiji transition possible at the speed and scale it occurred. Remove it, and the historical trajectory rearranges.
% FOUNDING_PROBLEM: The Meiji oligarchy faced a founding problem: how to transform a feudal, status-bound society into a modern nation-state capable of resisting colonial predation, within a timeframe that endogenous cultural evolution could not provide. The Tokugawa commitment order (Confucian hierarchy, samurai privilege, domain loyalties) was too entrenched for gradual fringe adoption. They needed a mechanism to instantiate the new commitments (national citizenship, bureaucratic service, industrial discipline) at scale, immediately.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (colonial threat + feudal entrenchment requiring rapid modernization) is historically corroborated by Meiji leaders' own writings (Iwakura Mission reports, Ōkubo Toshimichi memoranda) and by non-beneficiary sources: British diplomatic dispatches (Parkes, Satow) documenting the regime's existential anxiety, and peasant uprising records (Chichibu, Akizuki) showing the old order's resilience. No serious historian disputes the founding problem's reality or its resolution by 1890. The dispute is whether the hybrid cascade was the ONLY solution or a contingent choice among alternatives.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, ExtMetricName, E),
    domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(imposition_pathway_kernel__hybrid_cascade_reading),
    narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaks at initiation (1868: 0.85) when the decree extracts compliance at sword-point. It declines through the Meiji constitution (1889) and Taishō democracy (1912) as the climb naturalizes and the new commitments become 'common sense.' A suppression spike at 1945 reflects wartime totalization of the cascade (kokutai ideology). Postwar decline reflects democratic reorientation. The 2024 rise (0.62) reflects the analytical constraint's redeployment: the hybrid cascade framework is now used by development agencies and authoritarian modernizers to engineer commitment displacement, extracting compliance from new subject populations. Theater ratio rises as the mechanism's performative justification ('organic diffusion,' 'cultural fit') increasingly covers its engineered origin. Suppression requirement tracks the active enforcement needed to maintain the cascade's credibility — high at initiation, lower during naturalized climb, spiking when the framework is weaponized analytically.
 *
 * PERSPECTIVAL GAP:
 *   From the state_elites' seat, the cascade is a mountain — a necessary, inevitable law of modernization. From subject_populations' seat, it is a snare — coercive extraction with no exit. From conscripted_military and state_employees' seats, it begins as a snare (forced adoption) and becomes a tangled_rope (identity_locked participation in a coordination structure they now depend on). From the analytical seat, the mountain claim is false: the cascade is a contingent, engineered mechanism with identifiable beneficiaries. The engine computes this divergence from the structural data; the authored claim (mountain) does not adjudicate it. The False Summit Mountain signature will evaluate whether the mountain claim survives beneficiary presence.
 *
 * DIRECTIONALITY LOGIC:
 *   State_elites and regime_institutions are structural beneficiaries (d ≈ 0.1): they initiate the cascade, control its parameters, and collect legitimacy/resources. Imperial_bureaucracy and conscripted_military are dual-positioned: initially payers (forced adoption, loss of traditional identity), becoming beneficiaries as their identity fuses to the new order (identity_locked exit). State_employees similarly transition from payer to beneficiary via identity_lock. Subject_populations are trapped payers (d ≈ 0.95): they never consented, cannot exit, bear the cascade's full weight. Civil_society_actors are constrained payers (d ≈ 0.7): some strategic adaptation possible but the field is structured by the cascade. Traditional_status_groups are excluded (trapped): their objection is structurally irrelevant. The analytical observers (historical_sociologists) sit at d=0.5. The excluded reading proponents are identity_locked to their frameworks — conceding the hybrid case threatens their research identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rapid modernization under colonial threat) is dead — achieved by 1890, corroborated by external sources. Yet the cascade persists as an analytical framework and policy tool. The Meiji state's mandate has atrophied; the constraint now operates as a piton in the M-set framework (theatrical maintenance of a pathway cell) and as a snare when redeployed by development agencies. The mandatrophy is resolved in the historical sense but unresolved in the analytical sense: the framework continues to extract compliance from new cases by presenting the hybrid cascade as a natural law. The corridor_residue trap: the M-set's hybrid_cascade cell persists because no alternative cell fully explains Meiji-speed displacement, yet the cell's parameters are tuned to the Meiji case, making it a false universal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_mechanism,
    'Is the hybrid cascade a genuine natural law of state formation (mountain), or a constructed analytical category that benefits state-centric narratives by naturalizing engineered commitment displacement?',
    'Comparative historical analysis of non-Meiji cases: if the hybrid cascade appears only where state capacity matches Meiji''s, it is a contingent mechanism, not a natural law. If it appears universally across state formation episodes regardless of capacity, the mountain claim gains support.',
    'If constructed, the mountain claim fails and FSM reclassifies to tangled_rope (coordination of analytical framework + extraction via policy deployment). The beneficiaries (state_elites, regime_institutions, development_agencies) would be exposed as extracting via a false natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_mechanism, empirical, 'Whether the hybrid cascade is a universal law or a Meiji-specific mechanism generalized by beneficiaries.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three readings of the imposition_pathway_kernel disagree structurally?',
    'Map each reading''s M-set cell claims: endogenous claims single cell (fringe_climb); exogenous claims separate cell (state_override); hybrid claims coupled cell (manufactured_fringe → climb). The disagreement is whether the M-set needs one, two, or three cells for imposition pathways.',
    'If the cells are empirically distinguishable (different parameter ranges, different historical instances), all three readings coexist as valid specializations. If the hybrid cell subsumes the others (endogenous = hybrid with zero manufacture; exogenous = hybrid with zero climb), the kernel collapses to one reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural location of disagreement among endogenous_climb, exogenous_override, and hybrid_cascade readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state coercion, legal penalties) or internalized (subject populations believe the new commitments are legitimate/natural)?',
    'Post-exit suppression trajectory: in post-1945 Japan, the cascade''s commitments persisted despite occupation-imposed constitutional change. If suppression persists after the extractive mechanism (state enforcement) is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would raise the extraction ceiling for the analytical redeployment phase (post-1970).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the cascade''s climb phase and analytical afterlife.').

omega_variable(
    cs_framing_underdetermination,
    'Does the hybrid cascade reading ground its authority in the M-set framework''s formalized kernel (formalized+expertise), or in the historical practice of state formation (distributed+practice)?',
    'Trace citation networks: if the reading cites M-set formalisms as authority, it is formalized+expertise. If it cites Meiji historical record as self-authenticating practice, it is distributed+practice. The two framings yield different cs_pattern classifications.',
    'If formalized+expertise, the reading has an interpretation_layer_present=true and drift_state matters. If distributed+practice, no designated interpreter exists and drift_state is incoherent. This changes whether the reading can be foreclosed by axiom_overriding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framings of the hybrid cascade reading''s authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_tr_t1868, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_tr_t1889, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1889, 0.28).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_tr_t1912, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1912, 0.35).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_tr_t1945, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1945, 0.42).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_tr_t1970, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1970, 0.38).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_tr_t2000, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_tr_t2024, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_be_t1868, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1868, 0.85).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_be_t1889, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1889, 0.72).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_be_t1912, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1912, 0.65).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_be_t1945, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1945, 0.58).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_be_t1970, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_be_t2000, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_be_t2024, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_su_t1868, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1868, 0.9).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_su_t1889, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1889, 0.75).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_su_t1912, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1912, 0.68).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_su_t1945, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1945, 0.72).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_su_t1970, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_su_t2000, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(imposition_pathway_hybrid_cascade_su_t2024, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__hybrid_cascade_reading, 0.08).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, mset_framework_cascade_cell).

% DUAL FORMULATION NOTE:
% This constraint is one member of the imposition_pathway_kernel family. The endogenous_climb_reading claims all displacement is fringe-driven climb (single M-set cell). The exogenous_override_reading claims state override is a distinct mechanism without fringe (separate M-set cell). This hybrid_cascade_reading claims a coupled cell: state manufacture of fringe enables climb. The three readings share the kernel (imposition pathway) but instantiate different constraint structures with different ε values. Endogenous: ε≈0.15 (coordination only). Exogenous: ε≈0.75 (pure extraction). Hybrid: ε≈0.62 (coordination + extraction). The ε-invariance principle requires separate stories; they are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, institutional, 0.12).
constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, organized, 0.25).
constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, moderate, 0.68).
constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, powerless, 0.95).
constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
