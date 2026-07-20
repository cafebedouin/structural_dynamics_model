% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Scripture-Tradition-Magisterium Authority Structure
 *   domain: theology/religious_studies
 *
 * SUMMARY:
 *   This constraint instantiates the tradition_scripture_reading of the
 *   biblical_authority kernel: the claim that Scripture requires apostolic
 *   tradition and a living magisterium for authoritative interpretation, and
 *   that the magisterium guards the deposit of faith against doctrinal
 *   fragmentation. It is structurally distinct from the sola_scriptura
 *   reading (self-interpreting Scripture) and the conciliar reading
 *   (patristic/council consensus without monarchical magisterium). The
 *   constraint coordinates Christian communities around a unified doctrinal
 *   center while extracting interpretive agency from lay believers and
 *   concentrating sacramental and doctrinal power in the clerical hierarchy.
 *
 * KEY AGENTS:
 *   - clerical_hierarchy: Primary agenda-setter and beneficiary (institutional/civilizational) â administers interpretive monopoly and collects status and material support.
 *   - lay_interpretive_agents: Primary payer (powerless/identity-locked) â bear the loss of direct scriptural authority and depend on clerical mediation.
 *   - academic_theologian: Analytical observer (analytical/global) â external structural analysis without vested interest.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.78).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.75).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Scripture-Tradition-Magisterium Authority Structure").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious_studies").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, 'fbdc9a50-a687-4c3c-94a1-0984962e86cd').
narrative_ontology:cs_kernel_codification('fbdc9a50-a687-4c3c-94a1-0984962e86cd', fixed_text).
narrative_ontology:cs_authority_grounding('fbdc9a50-a687-4c3c-94a1-0984962e86cd', lineage).
narrative_ontology:cs_interpretation_layer_present('fbdc9a50-a687-4c3c-94a1-0984962e86cd').
narrative_ontology:cs_reading_relation('fbdc9a50-a687-4c3c-94a1-0984962e86cd', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('fbdc9a50-a687-4c3c-94a1-0984962e86cd', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('fbdc9a50-a687-4c3c-94a1-0984962e86cd', foundational, scripture_requires_tradition_for_authoritative_interpretation).
narrative_ontology:cs_axiom_status(scripture_requires_tradition_for_authoritative_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('fbdc9a50-a687-4c3c-94a1-0984962e86cd', scripture_requires_tradition_for_authoritative_interpretation, theological).
narrative_ontology:cs_axiom('fbdc9a50-a687-4c3c-94a1-0984962e86cd', foundational, magisterium_guards_deposit_of_faith).
narrative_ontology:cs_axiom_status(magisterium_guards_deposit_of_faith, holdable).
narrative_ontology:cs_axiom_grounding('fbdc9a50-a687-4c3c-94a1-0984962e86cd', magisterium_guards_deposit_of_faith, theological).
narrative_ontology:cs_reference_frame('fbdc9a50-a687-4c3c-94a1-0984962e86cd', apostolic_tradition_continuous).
narrative_ontology:cs_drift_state('fbdc9a50-a687-4c3c-94a1-0984962e86cd', contemporary_laic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fbdc9a50-a687-4c3c-94a1-0984962e86cd', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, clerical_hierarchy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_interpretive_agents).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, apostolic_succession).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, sacramental_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims apostolic succession and magisterial authority to interpret Scripture authentically through sacred tradition. Administers sacraments, defines orthodoxy, and excludes competing interpretations. Receives status, material support, and existential purpose from the monopoly on sacramental mediation and doctrinal adjudication.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, clerical_hierarchy, agenda_setter,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, clerical_hierarchy, beneficiary).

% Receive religious instruction and sacraments exclusively through clerical mediation. Denied authoritative capacity to interpret Scripture independently; their spiritual formation and communal belonging depend on the hierarchy's interpretive monopoly. Exit means excommunication or abandonment of a community fused with family and cultural identity.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_interpretive_agents, payer,
    powerless, biographical, identity_locked, universal).

% Studies the historical development of magisterial authority and its effects on textual interpretation. Neither collects rents from nor pays costs to the constraint; provides external structural analysis of its coordination and extraction functions.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, academic_theologian, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, clerical_hierarchy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents doctrinal fragmentation by providing a centralized, continuous interpretive authority that adjudicates contested readings of Scripture across generations and maintains sacramental continuity.
% TRANSFER_FUNCTION: Transfers interpretive authority and sacramental mediation from lay believers to the ordained hierarchy; moves material and status resources to the institutional Church in exchange for regulated spiritual access.
% ABSENT_VOICES: Dissenting reformers, Protestant exegetes, and lay theologians who would claim direct interpretive access are structurally excluded from authoritative discourse; their readings are classified as private opinion rather than magisterial teaching.
% DISAPPEARANCE_RATIONALE: If the magisterial monopoly on interpretation vanished, the Catholic Church would lose its centralized doctrinal continuity mechanism; sacramental theology would require reconfiguration, lay interpretive agency would expand dramatically, and competing readings would proliferate.
% FOUNDING_PROBLEM: Preventing doctrinal chaos and heresy in a dispersed community with divergent scriptural readings; preserving apostolic continuity after the death of the eyewitness generation.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchy attests the problem is still live, citing modernity's interpretive pluralism. Secular historians and Protestant scholars attest the founding problem was solved differently by other means (canon closure, creedal formulation) and that the magisterial solution now perpetuates itself as institutional extraction. Academic historians outside the beneficiary set corroborate the second-century context but dispute the necessity of a continuing magisterial monopoly.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the clerical hierarchy monopolizes sacramental efficacy and doctrinal adjudication, extracting deference and material support from laity. Suppression is high (0.75) because the constraint's persistence requires actively excluding alternative interpretations (heresy trials, excommunication, censorship). Theater_ratio is moderate (0.42): much magisterial activity is functionally genuine (doctrinal continuity, pastoral care), but a substantial share is performative maintenance of authority (ritual, dress, rhetoric). Accessibility_collapse is high (0.72) because once inside the framework, lay alternatives (personal interpretation) collapse doctrinally; resistance (0.60) reflects historical Reformation, modern dissent, and secularization pressures.
 *
 * PERSPECTIVAL GAP:
 *   The clerical hierarchy seat experiences the constraint as a necessary burden of guardianship and genuine coordination of dispersed believers; the lay_interpretive_agents seat experiences the same structure as the suppression of their direct relationship to Scripture. The engine computes this divergence from structural data â same constraint, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The clerical_hierarchy is declared beneficiary and agenda_setter (low d): the constraint subsidizes their status, material base, and existential role. Lay_interpretive_agents are declared victims (high d): they pay the cost of foregone agency and dependence. The academic observer sits at analytical exit with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) because it genuinely solves a coordination problem (doctrinal fragmentation) and provides sacramental continuity; it prevents mislabeling as pure coordination (rope) because the coordination is inseparable from asymmetric clerical extraction and active suppression of lay agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function of doctrinal continuity be preserved without the asymmetric extraction of a clerical interpretive monopoly?',
    'Comparative historical analysis of Christian communities maintaining doctrinal stability with lower extraction (e.g., Eastern Orthodox conciliarity, early creedal Christianity).',
    'If separable, the constraint is snare-flavored extraction with coordination cover; if inseparable, the extraction is the necessary price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether doctrinal unity and clerical monopoly are structurally separable.').

omega_variable(
    lay_agency_suppression_mechanism,
    'Is the suppression of lay interpretive agency structural (enforced by excommunication, censorship, institutional exclusion) or internalized (lay believers voluntarily cede authority due to identity fusion)?',
    'Post-exit trajectory analysis: if former Catholics regain interpretive agency quickly after exit, suppression was structural; if they persistently defer to clerical authority even outside the Church, it was internalized.',
    'Internalized suppression raises effective extraction above structural measures; the constraint travels with the agent after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_agency_suppression_mechanism, empirical, 'Structural vs internalized suppression of lay interpretive agency.').

omega_variable(
    magisterial_necessity_or_contingency,
    'Is the magisterial monopoly on interpretation a necessary condition for apostolic continuity, or a contingent historical development that concentrated power?',
    'Historical analysis of pre-medieval Christian governance and comparative ecclesiology across traditions without monarchical magisteria.',
    'If contingent, the high extractiveness is not the price of coordination but a historical accident ossified into doctrine; if necessary, the extraction is structurally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_necessity_or_contingency, conceptual, 'Whether the magisterial structure is historically contingent or theologically necessary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bibl_tr_t400, biblical_authority__tradition_scripture_reading, theater_ratio, 400, 0.3).
narrative_ontology:measurement(bibl_tr_t800, biblical_authority__tradition_scripture_reading, theater_ratio, 800, 0.35).
narrative_ontology:measurement(bibl_tr_t1200, biblical_authority__tradition_scripture_reading, theater_ratio, 1200, 0.45).
narrative_ontology:measurement(bibl_tr_t1600, biblical_authority__tradition_scripture_reading, theater_ratio, 1600, 0.55).
narrative_ontology:measurement(bibl_tr_t2000, biblical_authority__tradition_scripture_reading, theater_ratio, 2000, 0.42).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bibl_be_t400, biblical_authority__tradition_scripture_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(bibl_be_t800, biblical_authority__tradition_scripture_reading, base_extractiveness, 800, 0.65).
narrative_ontology:measurement(bibl_be_t1200, biblical_authority__tradition_scripture_reading, base_extractiveness, 1200, 0.75).
narrative_ontology:measurement(bibl_be_t1600, biblical_authority__tradition_scripture_reading, base_extractiveness, 1600, 0.82).
narrative_ontology:measurement(bibl_be_t2000, biblical_authority__tradition_scripture_reading, base_extractiveness, 2000, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bibl_su_t400, biblical_authority__tradition_scripture_reading, suppression_requirement, 400, 0.55).
narrative_ontology:measurement(bibl_su_t800, biblical_authority__tradition_scripture_reading, suppression_requirement, 800, 0.7).
narrative_ontology:measurement(bibl_su_t1200, biblical_authority__tradition_scripture_reading, suppression_requirement, 1200, 0.82).
narrative_ontology:measurement(bibl_su_t1600, biblical_authority__tradition_scripture_reading, suppression_requirement, 1600, 0.88).
narrative_ontology:measurement(bibl_su_t2000, biblical_authority__tradition_scripture_reading, suppression_requirement, 2000, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the biblical_authority kernel, decomposed per the Îµ-invariance principle because the natural-language label 'biblical authority' conflates structurally distinct claims: tradition-magisterial, sola scriptura, and conciliar-patristic readings instantiate different constraints with different beneficiary/victim structures and Îµ values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
