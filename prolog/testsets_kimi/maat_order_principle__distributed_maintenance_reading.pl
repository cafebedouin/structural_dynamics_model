% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at Distributed Maintenance Reading
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   The distributed maintenance reading of the Ma'at kernel holds that cosmic
 *   order is sustained not by Pharaoh's inherent divinity alone but by the
 *   proper conduct of all actors in their assigned stations. This reading
 *   admits multiple legitimate interpreters and grounds authority in
 *   demonstrated maintenance rather than status. It is the least extractive
 *   reading of the Ma'at kernel, yet it still operates within a rigid station
 *   system where temple and administrative elites interpret 'proper conduct'
 *   while peasant, artisan, and subordinate populations bear its material and
 *   social costs. The constraint coordinates genuine collective-action
 *   problems (agricultural calendars, irrigation, labor pooling) while
 *   asymmetrically concentrating interpretive power and surplus extraction.
 *
 * KEY AGENTS:
 *   - pharaonic_administration: agenda_setter (institutional/constrained) â enforces station obligations and coordinates surplus extraction
 *   - temple_priesthood: agenda_setter/beneficiary (institutional/constrained) â interprets and legitimates Ma'at, collects offerings and land
 *   - peasant_cultivators: primary payer (powerless/trapped) â bears labor and surplus extraction
 *   - artisan_workers: payer (moderate/constrained) â constrained guild labor with limited occupational mobility
 *   - women_in_subordinate_stations: payer (powerless/trapped) â reproductive and domestic station obligations with fused identity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.28).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.4).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at Distributed Maintenance Reading").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__distributed_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '4cbe45bd-16be-4fd8-98f2-070111bfa5b9').
narrative_ontology:cs_kernel_codification('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', distributed).
narrative_ontology:cs_authority_grounding('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', practice).
narrative_ontology:cs_interpretation_layer_present('4cbe45bd-16be-4fd8-98f2-070111bfa5b9').
narrative_ontology:cs_reading_relation('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', foundational, maat_maintained_by_conduct_not_status).
narrative_ontology:cs_axiom_status(maat_maintained_by_conduct_not_status, holdable).
narrative_ontology:cs_axiom_grounding('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', maat_maintained_by_conduct_not_status, theological).
narrative_ontology:cs_axiom('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', foundational, every_station_carries_cosmic_responsibility).
narrative_ontology:cs_axiom_status(every_station_carries_cosmic_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', every_station_carries_cosmic_responsibility, conventional).
narrative_ontology:cs_reference_frame('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', distributed_cosmic_maintenance).
narrative_ontology:cs_drift_state('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', new_kingdom_temple_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4cbe45bd-16be-4fd8-98f2-070111bfa5b9', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaonic_administration).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, peasant_cultivators).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, artisan_workers).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, women_in_subordinate_stations).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, cosmic_order_legitimacy).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, station_based_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the assignment and enforcement of social stations, coordinates agricultural and labor obligations, and legitimates rule through participation in Ma'at maintenance. Bound by the same cosmic order it enforces; deviation risks cosmological chaos and political collapse.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaonic_administration, agenda_setter,
    institutional, generational, constrained, national).

% Interprets proper conduct and ritual observance required by Ma'at across stations. Derives material support and social authority from its role as legitimate interpreter. Cannot renounce interpretive role without losing institutional purpose and cosmological standing.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, temple_priesthood, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, temple_priesthood, beneficiary).

% Perform agricultural labor and corvÃ©e obligations as proper conduct in their station. Their surplus sustains temple and administrative institutions. Exit from the station system is physically and cosmologically unthinkable; landlessness or flight means chaos.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, peasant_cultivators, payer,
    powerless, biographical, trapped, local).

% Execute specialized labor within guild-like stations, contributing craft and tax obligations to the redistributive economy. Enjoy limited occupational mobility within the artisan class but cannot transcend the station framework without social death.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, artisan_workers, payer,
    moderate, biographical, constrained, regional).

% Maintain household and reproductive labor as cosmic duty within their station. Structural subordination is framed as necessary contribution to Ma'at. Identity is fused with familial station; independent exit routes do not exist.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, women_in_subordinate_stations, payer,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, diffuse).
narrative_ontology:fixing_cost_class(maat_order_principle__distributed_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes cosmic maintenance responsibility across all social stations, ensuring interdependent agricultural, craft, and ritual production without requiring centralized direction of every productive act.
% TRANSFER_FUNCTION: Moves agricultural surplus, labor service, and craft output from peasant, artisan, and subordinate stations to temple and administrative institutions; moves cosmological legitimacy and social stability downward through ritual and judicial affirmation.
% ABSENT_VOICES: Foreign populations and conquered peoples excluded from the Ma'at system; heretic movements rejecting station theology; subordinate women whose interpretation of proper conduct differs from priestly orthodoxy.
% DISAPPEARANCE_RATIONALE: The ideological architecture of station-based obligation would dissolve; labor allocation, temple economy, and political legitimacy would require alternative coercive or remunerative frameworks.
% FOUNDING_PROBLEM: Cosmic chaos (isft) and social fragmentation threaten agricultural productivity, riverine cooperation, and imperial coherence; unpredictable behavior undermines the interdependence required by the Nile environment.
% FOUNDING_PROBLEM_CORROBORATION: No independent corroboration exists outside the temple and administrative beneficiaries; the founding problem is attested only by the Ma'at-interpreting institutions themselves.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) because the distributed reading genuinely diffuses responsibility and limits the Pharaoh-as-divine-monopoly framing; however, station-based society still extracts labor and autonomy from subordinate groups. Suppression (0.40) reflects social, religious, and legal enforcement of station boundaries rather than pure physical coercion. Theater ratio (0.22) acknowledges that ritual performance of Ma'at is partially constitutive of the order, but a substantial portion of distributed maintenance is functional coordination. Accessibility collapse (0.45) is moderate: alternatives like isft (chaos) exist in mythology but are culturally uninhabitable. Resistance (0.25) is low because the cosmological framing internalizes compliance. The measurement series run on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The administrative and priestly seats experience the constraint as necessary coordination they maintain; the peasant and subordinate seats experience it as cosmologically legitimated extraction. The engine computes this divergence from shared structural data â identical enforcement rules produce opposite directionalities depending on exit options and power.
 *
 * DIRECTIONALITY LOGIC:
 *   Temple priesthood and pharaonic administration sit near the beneficiary end (low d): they define proper conduct, collect surplus, and derive authority from the system. Peasant cultivators and subordinate women sit near the full-target end (high d): they pay labor and autonomy, are trapped by identity-fusion with station, and cannot exit. Artisan workers occupy a middle position: some skill-based mobility but still station-bound.
 *
 * MANDATROPHY ANALYSIS:
 *   The distributed reading resists mandatrophy mislabeling because its coordination function (distributed cosmic maintenance) is structurally inseparable from its extraction (station-bound labor). The founding problem â chaos and fragmentation â remains contested because no external party attests it independently. If the problem were dead, the constraint would drift toward piton; here, the ongoing need for agricultural and ritual coordination keeps the coordination function live, preserving tangled_rope classification against snare drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maat_cosmic_or_social_construction,
    'Is Ma''at a discoverable cosmic principle or a socially constructed legitimation of station-based extraction?',
    'Comparative analysis of other ancient Near Eastern cosmologies; archaeological evidence of social mobility versus station rigidity.',
    'If purely constructed, the constraint''s claimed coordination function is cover for extraction and extraction metrics should rise; if genuinely cosmic-believed, the coordination function is sincere and the classification edges toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maat_cosmic_or_social_construction, conceptual, 'Cosmic natural law versus social construction of Ma''at').

omega_variable(
    distributed_authority_vs_temple_consolidation,
    'Did the distributed maintenance reading ever operate with genuinely distributed interpretive authority, or did temple and court institutions monopolize Ma''at interpretation despite the distributed rhetoric?',
    'Textual analysis of non-royal, non-priestly Ma''at claims in Instruction literature, tomb biographies, and rebel narratives.',
    'If interpretation was always centralized, the distributed reading is a false summit or ideological cover; if genuinely distributed, the reading''s low extraction claim is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_authority_vs_temple_consolidation, empirical, 'Whether Ma''at interpretation was truly distributed or monopolized').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of station deviation structural (legal punishment, economic sanction) or internalized (cosmological fear of chaos, identity-fusion with station)?',
    'Post-exit behavior analysis impossible historically; proxy via evidence of social mobility attempts and their cultural treatment.',
    'If internalized suppression dominates, effective extraction exceeds structural measures; if structural, the constraint operates more like standard enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression in station enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(maat_tr_t12, maat_order_principle__distributed_maintenance_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(maat_tr_t24, maat_order_principle__distributed_maintenance_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(maat_tr_t36, maat_order_principle__distributed_maintenance_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement(maat_tr_t48, maat_order_principle__distributed_maintenance_reading, theater_ratio, 48, 0.2).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__distributed_maintenance_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(maat_be_t12, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(maat_be_t24, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 24, 0.22).
narrative_ontology:measurement(maat_be_t36, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 36, 0.24).
narrative_ontology:measurement(maat_be_t48, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 48, 0.26).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 60, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(maat_su_t12, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(maat_su_t24, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(maat_su_t36, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 36, 0.36).
narrative_ontology:measurement(maat_su_t48, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 48, 0.38).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 60, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% The maat_order_principle kernel decomposes into three structurally distinct constraints: distributed_maintenance_reading (low extraction, distributed authority), divine_mandate_reading (high extraction, concentrated inherent authority), and reciprocity_reading (mutual obligation framing). Each reading has distinct beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
