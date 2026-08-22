% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment as Militia-Conditioned Individual Right (Civic Right Reading)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the civic-right reading of the Second Amendment
 *   kernel: the right to keep and bear arms is constitutionally anchored in,
 *   and conditioned upon, participation in an organized, state-regulated
 *   militia. This is distinct from the individual-right reading (right exists
 *   independent of militia service — see sibling constraint
 *   individual_right_reading) and the collective-right reading (no individual
 *   right at all, only state authority to maintain militias — see sibling
 *   constraint collective_right_reading). The theater_ratio and
 *   suppression_requirement trajectories track the growing gap between the
 *   reading's textual anchor (an organized militia) and the near-total
 *   institutional supplanting of that militia by professional standing forces
 *   after the Militia Act of 1903 and the National Defense Act of 1916 — the
 *   coordination story becomes harder to sustain in practice even as courts
 *   and advocates continue invoking it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.42).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.38).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment as Militia-Conditioned Individual Right (Civic Right Reading)").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '53716591-1a09-401d-b96e-8c990177969e').
narrative_ontology:cs_kernel_codification('53716591-1a09-401d-b96e-8c990177969e', fixed_text).
narrative_ontology:cs_authority_grounding('53716591-1a09-401d-b96e-8c990177969e', lineage).
narrative_ontology:cs_interpretation_layer_present('53716591-1a09-401d-b96e-8c990177969e').
narrative_ontology:cs_reading_relation('53716591-1a09-401d-b96e-8c990177969e', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('53716591-1a09-401d-b96e-8c990177969e', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('53716591-1a09-401d-b96e-8c990177969e', foundational, militia_clause_is_operative_condition).
narrative_ontology:cs_axiom_status(militia_clause_is_operative_condition, holdable).
narrative_ontology:cs_axiom_grounding('53716591-1a09-401d-b96e-8c990177969e', militia_clause_is_operative_condition, conventional).
narrative_ontology:cs_axiom('53716591-1a09-401d-b96e-8c990177969e', secondary, civic_virtue_grounds_arms_right_not_personal_autonomy).
narrative_ontology:cs_axiom_status(civic_virtue_grounds_arms_right_not_personal_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('53716591-1a09-401d-b96e-8c990177969e', civic_virtue_grounds_arms_right_not_personal_autonomy, deontological).
narrative_ontology:cs_reference_frame('53716591-1a09-401d-b96e-8c990177969e', founding_era_civic_militia_republicanism).
narrative_ontology:cs_drift_state('53716591-1a09-401d-b96e-8c990177969e', post_national_guard_absorption, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('53716591-1a09-401d-b96e-8c990177969e', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, organized_state_militias).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, civic_republican_legal_tradition).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_militia_gun_owners).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, urban_residents_without_militia_access).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, gun_control_advocates_seeking_categorical_regulation).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republicanism_as_constitutional_baseline).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, well_regulated_militia_clause_operative_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who participate or could participate in organized, state-regulated militia structures (historically the state-organized militia, today closer to National Guard service or state defense force membership). Under this reading, their right to keep and bear arms is textually and historically the clearest case — the right exists because civic militia service is a live institution they participate in.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, civilizational, constrained, national).

% State-chartered militia bodies (historically the militia system,今 substantially absorbed into the National Guard) whose organizational legitimacy and resourcing this reading treats as the constitutional anchor. They administer training, standards, and call-up, and their continued relevance is what keeps the reading's coordination story alive.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, organized_state_militias, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, organized_state_militias, agenda_setter).

% Not an actor but a jurisprudential tradition (linking arms-bearing to civic virtue and collective self-governance) whose doctrinal standing is vindicated whenever courts read the prefatory militia clause as operative rather than merely explanatory.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, civic_republican_legal_tradition, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_scope__civic_right_reading, civic_republican_legal_tradition).

% The large majority of contemporary firearm owners who have no connection to any organized militia body and never will, since standing militias in the historical sense barely exist. Under this reading their claim to a constitutionally protected individual right is weaker or absent, subjecting their ownership to whatever regulatory latitude the civic-service gate allows — they bear the cost of a reading that ties the right to a mostly defunct institution.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, non_militia_gun_owners, payer,
    moderate, biographical, constrained, national).

% Residents (often in jurisdictions with strict local gun laws) who have no practical path into an organized militia and therefore, on this reading, no clear constitutional foothold against restrictive local ordinances. They cannot exit the jurisdiction easily nor manufacture militia eligibility, and bear the downstream regulatory consequences of a reading premised on a civic institution largely unavailable to them.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, urban_residents_without_militia_access, payer,
    powerless, biographical, trapped, regional).

% Advocacy organizations who would prefer the collective-right reading (no individual right at all, only state militia-maintenance authority) find this civic-right reading only partially useful: it permits regulation of individual ownership outside militia service but still concedes a real individual right within militia contexts, foreclosing the cleanest deregulatory ground they seek.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, gun_control_advocates_seeking_categorical_regulation, payer,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, gun_control_advocates_seeking_categorical_regulation, observer).

% Legal historians and doctrinal scholars who examine ratification-era militia statutes, English common-law antecedents, and founding-era rhetoric to assess whether the civic-right reading accurately recovers original public meaning, without a direct stake in the outcome's practical regulatory effect.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, second_amendment_originalist_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ties the constitutional right to keep and bear arms to participation in an organized, state-regulated militia system, coordinating individual arms-bearing with collective defense capacity and civic obligation — solving the founding-era problem of maintaining trained, armed citizen-soldiers without a standing professional army.
% TRANSFER_FUNCTION: Moves constitutional protection preferentially toward militia-connected individuals and state militia institutions, and correspondingly withholds strong constitutional protection from firearm ownership disconnected from militia service — shifting regulatory latitude toward legislatures for the non-militia-connected majority of gun owners.
% ABSENT_VOICES: Contemporary gun owners with no militia connection and no realistic path to acquire one are not treated as central rights-holders under this reading even though they are the overwhelming majority of those the amendment is invoked to protect in modern litigation; they are present in the debate but structurally marginal to this reading's core logic.
% DISAPPEARANCE_RATIONALE: If courts abandoned the civic-right reading overnight in favor of the individual-right reading (as occurred substantially post-2008), the practical world for most gun owners changes little in the short term since the individual-right reading already dominates; but institutionally the National Guard/militia-tradition's claim to special constitutional salience would erode, and jurisdictions relying on militia-connection tests for regulatory latitude would lose their doctrinal footing. Whether this constitutes rearrangement or continuity is itself the subject of live jurisprudential dispute.
% FOUNDING_PROBLEM: The founding generation distrusted standing professional armies as tools of tyranny and needed to preserve the capacity of ordinary citizens, organized into state militias, to bear arms for collective defense and to resist federal military overreach.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the militia system and several originalist scholars outside the civic-right advocacy camp (including scholars who ultimately favor the individual-right reading) attest that organized militias as a live institution have been almost entirely supplanted by professional standing military and National Guard structures since the early 20th century (Militia Act of 1903 and after), making the founding problem largely dead in practice even though the civic-right reading treats it as doctrinally still-operative.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).
:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate: this reading does perform real coordination work (linking arms-bearing to a genuine civic-defense institution) but it also withholds strong protection from the majority of contemporary gun owners who have no militia connection, which functions as a structural cost imposed on that majority in service of a reading whose institutional anchor has substantially atrophied. Suppression (0.38) reflects the active doctrinal and legislative work required to sustain a militia-connection test given the near-disappearance of organized militias as lived institutions. Accessibility collapse is moderate (0.4) — non-militia owners are not fully foreclosed from all protection (they retain political and some judicial recourse) but the civic-right test does meaningfully narrow their path.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of an organized militia member or the National Guard, this reading looks like principled continuity with founding-era civic republicanism. From the seat of the ordinary urban gun owner with no militia connection, the same reading looks like an arbitrary gate that happens to exclude them from the amendment's strongest protection based on an institutional affiliation that is practically unavailable. The engine's per-seat computation should reflect that asymmetry rather than averaging it away.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible citizens and organized state militia institutions are the clear beneficiaries — the reading's protection concentrates on them by design. Non-militia gun owners, urban residents without militia access, and would-be categorical regulators all pay costs of different kinds: the former two lose strong individual protection; the latter loses a cleaner deregulatory hook because the reading still concedes a real (if conditioned) individual right. This asymmetry — real coordination benefit for a narrowing beneficiary class, real cost for a majority class — is the tangled-rope signature.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fear of standing armies, need for citizen-soldier capacity) is substantially dead as a live institutional matter — organized militias in the founding-era sense do not meaningfully exist. The civic-right reading persists as a doctrinal framework anyway, which risks classic mandatrophy: an arrangement whose founding function has atrophied but which continues to allocate rights and burdens as if it were still operative. Naming this explicitly (rather than assuming the reading's continued invocation proves continued function) is what keeps the analysis honest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_institution_vitality,
    'Does an ''organized militia'' in any constitutionally meaningful sense still exist, such that the civic-right reading''s coordination function is live rather than vestigial?',
    'Historical and institutional analysis of whether the National Guard, state defense forces, or any other body functions as the constitutional ''militia'' the amendment contemplates, versus having been fully absorbed into the professional federal military structure.',
    'If no live militia institution exists in the relevant sense, the civic-right reading''s coordination story collapses into pure gatekeeping — the reading would then function closer to a snare (excluding the majority from strong protection) dressed in coordination language, rather than a genuine tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_institution_vitality, empirical, 'Whether the militia institution the reading depends on is still functionally alive.').

omega_variable(
    kernel_reading_choice_ambiguity,
    'Is the civic-right reading a distinct, coherent third position, or is it better understood as an unstable midpoint between the individual-right and collective-right readings that collapses toward one or the other under sustained doctrinal pressure?',
    'Track whether courts applying a militia-connection test converge in practice toward either near-total deference (collapsing into collective_right_reading) or near-total individual protection with a formalistic militia nod (collapsing into individual_right_reading).',
    'If the civic-right reading is unstable, its moderate epsilon and tangled-rope classification may be a snapshot of a transitional state rather than a stable equilibrium reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_choice_ambiguity, conceptual, 'Whether the civic-right reading is a stable third position or a transitional midpoint between the other two kernel readings.').

omega_variable(
    founding_problem_status_ambiguity,
    'Is the founding problem (need for citizen-soldier capacity against standing-army tyranny) dead, or has it been transformed rather than eliminated (e.g., into concerns about federal overreach more generally)?',
    'Comparative analysis of contemporary invocations of the amendment''s purpose against 18th-century anti-Federalist arguments to see whether the underlying anxiety persists in transformed form.',
    'If transformed rather than dead, the mandatrophy verdict softens — the arrangement may still be doing analogous work even without literal militias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_ambiguity, conceptual, 'Whether the founding problem is dead or has migrated into a different but related contemporary concern.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 1791, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__civic_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1903, second_amendment_scope__civic_right_reading, theater_ratio, 1903, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1903, observed).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_scope__civic_right_reading, theater_ratio, 1939, 0.25).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_scope__civic_right_reading, theater_ratio, 1980, 0.32).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__civic_right_reading, theater_ratio, 2008, 0.28).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_scope__civic_right_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement_basis(seco_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__civic_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1903, second_amendment_scope__civic_right_reading, base_extractiveness, 1903, 0.25).
narrative_ontology:measurement_basis(seco_be_t1903, observed).
narrative_ontology:measurement(seco_be_t1939, second_amendment_scope__civic_right_reading, base_extractiveness, 1939, 0.32).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_scope__civic_right_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__civic_right_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2025, second_amendment_scope__civic_right_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(seco_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__civic_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1903, second_amendment_scope__civic_right_reading, suppression_requirement, 1903, 0.18).
narrative_ontology:measurement_basis(seco_su_t1903, observed).
narrative_ontology:measurement(seco_su_t1939, second_amendment_scope__civic_right_reading, suppression_requirement, 1939, 0.24).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t1980, second_amendment_scope__civic_right_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement_basis(seco_su_t1980, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__civic_right_reading, suppression_requirement, 2008, 0.35).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2025, second_amendment_scope__civic_right_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(seco_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__civic_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, collective_right_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the second_amendment_scope kernel. individual_right_reading authors a low-epsilon, low-gating individual right unconnected to militia service (beneficiary set: all gun owners broadly). collective_right_reading authors a reading with no individual beneficiary class at all, only state militia-maintenance authority (beneficiary set: state governments). This civic_right_reading occupies the structural middle: it names militia-eligible individuals and organized militias as beneficiaries with moderate epsilon and service-based gating, producing a tangled_rope classification distinct from the other two readings' likely classifications. Each reading has its own stable epsilon; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
