% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Jerusalem Reading of Human Transcendence Pathway
 *   domain: religious/social/political
 *
 * SUMMARY:
 *   This constraint instantiates the Jerusalem reading of the human
 *   transcendence pathway kernel: authentic human community is rebuilt not
 *   through technological optimization or unified human power, but through
 *   patient, participatory labor under divine blessing. Integrating plurality
 *   into communion rather than uniformity, it structures social relations in
 *   Catholic Social Doctrine and allied communitarian traditions. The model
 *   centers marginalized returning exiles as beneficiaries of inclusion,
 *   requiring symmetric sacrifice of efficiency from all members for the sake
 *   of solidarity.
 *
 * KEY AGENTS:
 *   - Returning exiles (marginalized): primary beneficiaries (powerless/constrained) â receive inclusion and dignity through participatory labor
 *   - Hosting community members: primary beneficiaries (moderate/constrained) â share responsibility and surrender efficiency
 *   - Ecclesial magisterium: agenda-setter (institutional/analytical) â promulgates and interprets the doctrinal framework
 *   - Technocratic efficiency advocates: excluded (powerful/mobile) â would object to the sacrifice of optimization
 *   - Secular communitarian observers: observer (organized/analytical) â corroborate the social problem from outside the theology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.3).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.2).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Reading of Human Transcendence Pathway").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "religious/social/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '64695588-929e-484f-849e-95e020bf61c4').
narrative_ontology:cs_kernel_codification('64695588-929e-484f-849e-95e020bf61c4', formalized).
narrative_ontology:cs_authority_grounding('64695588-929e-484f-849e-95e020bf61c4', lineage).
narrative_ontology:cs_interpretation_layer_present('64695588-929e-484f-849e-95e020bf61c4').
narrative_ontology:cs_reading_relation('64695588-929e-484f-849e-95e020bf61c4', human_transcendence_pathway__babel_reading, forecloses).
narrative_ontology:cs_reading_relation('64695588-929e-484f-849e-95e020bf61c4', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('64695588-929e-484f-849e-95e020bf61c4', foundational, communion_requires_divine_blessing).
narrative_ontology:cs_axiom_status(communion_requires_divine_blessing, holdable).
narrative_ontology:cs_axiom_grounding('64695588-929e-484f-849e-95e020bf61c4', communion_requires_divine_blessing, theological).
narrative_ontology:cs_axiom('64695588-929e-484f-849e-95e020bf61c4', foundational, diversity_preserved_as_resource).
narrative_ontology:cs_axiom_status(diversity_preserved_as_resource, holdable).
narrative_ontology:cs_axiom_grounding('64695588-929e-484f-849e-95e020bf61c4', diversity_preserved_as_resource, deontological).
narrative_ontology:cs_reference_frame('64695588-929e-484f-849e-95e020bf61c4', restored_communion_in_plurality).
narrative_ontology:cs_drift_state('64695588-929e-484f-849e-95e020bf61c4', contemporary_technocratic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('64695588-929e-484f-849e-95e020bf61c4', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles_marginalized).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, hosting_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Those on the margins of communityâeconomically displaced, spiritually exiled, socially fragmentedâwho are explicitly welcomed into the participatory rebuilding process. They receive inclusion and dignity but must invest labor and patience into slow communal restoration rather than receiving immediate material optimization.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles_marginalized, beneficiary,
    powerless, biographical, constrained, local).

% Established members who share responsibility for rebuilding, surrendering individual efficiency and control to integrate returning exiles. They contribute labor and patience, receiving strengthened communion and shared identity in return.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, hosting_community_members, beneficiary,
    moderate, generational, constrained, regional).

% The teaching authority that promulgates the Jerusalem model through encyclicals, catechesis, and pastoral guidance. It administers the doctrinal framework, interpreting divine blessing and communion norms without directly extracting material benefit from those coordinated.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, ecclesial_magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Actors in broader society who prioritize speed, optimization, and top-down technological control in social organization. They would object to the sacrifice of efficiency and the rejection of technological self-sufficiency but are not present in the communal deliberation.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, technocratic_efficiency_advocates, excluded,
    powerful, biographical, mobile, global).

% Sociologists and civic republicans who observe the Jerusalem model's communitarian outcomes without subscribing to its theological premises. They corroborate the social value of participatory integration but remain outside the doctrinal commitment.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, secular_communitarian_observers, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rebuilding authentic human community after fragmentation by coordinating diverse members into participatory, shared labor that produces communion without requiring uniformity.
% TRANSFER_FUNCTION: Moves time, patience, and labor from individual optimization toward collective solidarity; moves social standing from marginalization toward dignified inclusion through shared responsibility.
% ABSENT_VOICES: Technocratic planners who prioritize speed and top-down control; assimilationists who would erase diversity for uniformity; secular actors who reject transcendent framing but might share communitarian goals.
% DISAPPEARANCE_RATIONALE: If the Jerusalem model vanished, communities formed around it would lose their organizing principle of patient participatory integration; exiles would lose their pathway from marginalization to communion, and the social fabric would shift toward either atomized individualism or forced uniformity.
% FOUNDING_PROBLEM: The fragmentation of human community through sin, exile, and technocratic atomization, which destroys the conditions for authentic communion and reduces persons to units of production or consumption.
% FOUNDING_PROBLEM_CORROBORATION: Secular sociologists and communitarian critics corroborate the problem of social fragmentation and atomization from outside the theological framework, though they do not attest the specific divine-blessing solution; within the tradition, the problem is self-attested through Scripture and magisterial teaching.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.3, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.30) because the coordination is participatory and persuasive rather than coercive; costs are symmetrically distributed rather than extracted from one party by another. Suppression is low (0.20) consistent with formation and persuasion. Theater ratio is low (0.18) because the labor is genuinely participatory, though some ritual performance of community may occur. Accessibility collapse is moderate (0.45): secular alternatives remain available, so the constraint does not monopolize social imagination. Resistance is low-moderate (0.25): internal friction exists around efficiency sacrifice, and the technocratic alternative exerts cultural pull. The temporal series shows slight drift upward as modernity pressures the model, but the constraint remains fundamentally coordinative.
 *
 * PERSPECTIVAL GAP:
 *   The marginalized and established community members experience the constraint as genuine solidarity and inclusion (low directionality), while technocratic actors outside the framework experience it as an irrational rejection of optimization (high directionality relative to their interests, though they are excluded rather than governed). The magisterium experiences it as a custodial duty with minimal material extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to returning exiles and hosting community members, both of whom receive communion and inclusion. No victim group is declared because the sacrifice of efficiency is structurally symmetric. The magisterium is an agenda-setter without material beneficiary status. Directionality is therefore low for all governed parties, producing low effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope prevents mislabeling the Jerusalem model as either a naive natural law (it is constructed and doctrinally mediated) or as extraction (the efficiency sacrifice is symmetric and consented to). If the efficiency sacrifice were shown to fall disproportionately on the marginalized, the constraint would shift toward tangled_rope; the omega variable documents this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_sacrifice_symmetry,
    'Is the required sacrifice of efficiency for solidarity borne symmetrically by all community members, or does it fall disproportionately on the marginalized?',
    'Comparative ethnographic study of labor and time burdens across power levels within Jerusalem-model communities.',
    'If asymmetric, the constraint shifts toward tangled_rope; if symmetric, it remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_sacrifice_symmetry, empirical, 'Whether efficiency costs are symmetric or extractive').

omega_variable(
    divine_blessing_necessity,
    'Is the ''divine blessing'' component a necessary causal factor for the coordination function, or can the same communitarian outcomes be achieved through secular participatory mechanisms?',
    'Comparative study of religious versus secular intentional communities on integration of marginalized members and preservation of diversity.',
    'If secular mechanisms suffice, the constraint''s theological scaffolding is performative and theater_ratio rises toward piton; if divine blessing is causally necessary, the coordination is genuinely dependent on the transcendent frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_blessing_necessity, conceptual, 'Theological necessity versus performative framing').

omega_variable(
    formation_vs_coercion,
    'Does the ''patient participatory labor'' operate through genuine persuasion and free consent, or does it rely on internalized obligation and identity-lock that suppresses exit?',
    'Exit interviews and longitudinal study of members who leave Jerusalem-model communities, measuring whether departure is freely chosen or fraught with shame and ostracism.',
    'If exit is genuinely free, suppression remains low and the constraint stays rope; if internalized suppression is high, the constraint becomes tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formation_vs_coercion, empirical, 'Persuasion versus internalized coercion mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(htp_jer_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(htp_jer_tr_t16, human_transcendence_pathway__jerusalem_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(htp_jer_tr_t32, human_transcendence_pathway__jerusalem_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(htp_jer_tr_t48, human_transcendence_pathway__jerusalem_reading, theater_ratio, 48, 0.16).
narrative_ontology:measurement(htp_jer_tr_t64, human_transcendence_pathway__jerusalem_reading, theater_ratio, 64, 0.18).
narrative_ontology:measurement(htp_jer_tr_t80, human_transcendence_pathway__jerusalem_reading, theater_ratio, 80, 0.2).

% Extraction over time
narrative_ontology:measurement(htp_jer_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(htp_jer_be_t16, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(htp_jer_be_t32, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 32, 0.26).
narrative_ontology:measurement(htp_jer_be_t48, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 48, 0.28).
narrative_ontology:measurement(htp_jer_be_t64, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 64, 0.29).
narrative_ontology:measurement(htp_jer_be_t80, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 80, 0.3).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(human_transcendence_pathway__jerusalem_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% The human_transcendence_pathway kernel decomposes into three structurally distinct readings: the Babel reading (technocratic self-sufficiency), the technocratic-vs-incarnational contest reading, and the Jerusalem reading (divine-blessed participatory communion). Each has distinct epsilon, beneficiary structure, and authority grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
