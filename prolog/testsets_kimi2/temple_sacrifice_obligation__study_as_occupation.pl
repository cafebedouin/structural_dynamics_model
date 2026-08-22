% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Legitimate Occupation of Obligation in Temple's Absence
 *   domain: religious/halakhic/commitment_systems
 *
 * SUMMARY:
 *   In the absence of the Jerusalem Temple, this reading of the sacrificial
 *   commandment holds that intensive study of the relevant Torah and Talmudic
 *   texts constitutes a legitimate 'occupation' with the
 *   obligationâfulfilling it rather than suspending or merely archiving it.
 *   The kernel is the biblical sacrificial legislation; the reading is
 *   mediated through a rabbinic interpretive layer that treats cognitive
 *   labor as functionally equivalent to cultic action. The constraint
 *   coordinates communal continuity without identifiable victims, while the
 *   authority structure absorbs the massive historical drift (destruction of
 *   the Temple) without surfacing a need to revise the kernel itself.
 *
 * KEY AGENTS:
 *   - Rabbinic authority (agenda_setter): institutional power, analytical exitâcontrols the interpretive substitution mechanism.
 *   - Torah study community (beneficiary): moderate power, constrained exitâinvests time and receives merit/continuity.
 *   - Temple restoration advocates (excluded): moderate power, constrained exitâreject substitution but are marginalized from halakhic discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.18).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.25).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.18).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Legitimate Occupation of Obligation in Temple's Absence").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, '807ee0e7-0859-4a43-9e42-5dc5fe155ed0').
narrative_ontology:cs_kernel_codification('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', fixed_text).
narrative_ontology:cs_authority_grounding('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', lineage).
narrative_ontology:cs_interpretation_layer_present('807ee0e7-0859-4a43-9e42-5dc5fe155ed0').
narrative_ontology:cs_reading_relation('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', temple_sacrifice_obligation__messianic_suspension, forecloses).
narrative_ontology:cs_reading_relation('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', temple_sacrifice_obligation__study_as_archiving, forecloses).
narrative_ontology:cs_axiom('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', foundational, study_occupies_sacrificial_obligation).
narrative_ontology:cs_axiom_status(study_occupies_sacrificial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', study_occupies_sacrificial_obligation, theological).
narrative_ontology:cs_axiom('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', foundational, obligation_persists_post_temple).
narrative_ontology:cs_axiom_status(obligation_persists_post_temple, holdable).
narrative_ontology:cs_axiom_grounding('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', obligation_persists_post_temple, deontological).
narrative_ontology:cs_reference_frame('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', active_divine_command_state).
narrative_ontology:cs_drift_state('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', post_temple_destruction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('807ee0e7-0859-4a43-9e42-5dc5fe155ed0', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, torah_study_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, oral_torah_authority).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, rabbinic_hermeneutic_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the interpretive tradition that substitutes textual study for sacrificial performance; determines curriculum, normative weight, and the legal mechanisms by which study occupies the obligation. Derives authority from a chain of transmission anchored in the fixed scriptural kernel.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, rabbinic_authority, agenda_setter,
    institutional, generational, analytical, global).

% Engages in daily study of sacrificial lawâtractates Tamid, Zevachim, Menachotâunder the framing that this cognitive labor fulfills the biblical commandment in the absence of the Temple. Receives spiritual merit, communal status, and ontological continuity with the covenantal past.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, torah_study_community, beneficiary,
    moderate, biographical, constrained, global).

% Advocates for immediate rebuilding of the Temple and restoration of actual sacrificial cult; rejects the premise that textual study can substitute for physical performance. Marginalized within mainstream halakhic discourse, their position is treated as eschatologically premature rather than legally live.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, temple_restoration_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains collective continuity of a covenantal obligation across a diasporic community after the physical site and priestly infrastructure for performance have been destroyed; coordinates memory, legal competence, and communal identity through a shared, daily textual practice.
% TRANSFER_FUNCTION: Moves cognitive labor and time from individual community members into the study of sacrificial texts, while consolidating interpretive authority in the rabbinic line that manages the substitution framework.
% ABSENT_VOICES: Temple restoration movements who reject textual substitution as insufficient; secular Jewish communities who view the arrangement as ritual nostalgia rather than obligation fulfillment; non-Jewish biblical critics who read the kernel as historically contingent rather than eternally binding.
% DISAPPEARANCE_RATIONALE: If the obligation to study sacrifice law vanished, the rabbinic curriculum would reorganize around performable commandments, diasporic Jewish practice would lose a major daily anchor for study cycles, and the theological mechanism preserving covenantal continuity in exile would collapseâalternative frameworks (suspension, messianic waiting) would gain normative ground.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the physical and priestly infrastructure for the biblical sacrificial commandment, threatening covenantal rupture and communal dissolution in the absence of a central cult.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of religion corroborate the Temple destruction as a historical rupture requiring adaptive response. The specific 'study as occupation' solution is attested primarily within the rabbinic literary tradition (Mishnah, Talmud Bavli); competing internal readings (suspension, archiving) are also attested, corroborating the persistence of the problem but disputing the solution.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the 'cost' of study is framed as spiritual merit and communal participation rather than extraction; suppression is low (0.25) because participation is normatively guided rather than coerced; theater ratio is low (0.2) because the study function is substantively continuous with the community's identity and legal practice. Accessibility collapse is moderate-high (0.65): once inside the halakhic framework, the logic of substitution is tight, but outside frames (secular, academic, restorationist) remain intellectually available. Resistance is low (0.15) because this reading is mainstream within Orthodox Judaism, with only marginal opposition from restoration movements.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, the constraint is legitimate adaptation that preserves covenantal continuity under impossible conditions. From the excluded restorationist seat, the same structure appears as an evasion of actual divine command that consolidates interpretive power while deferring performance indefinitely. The engine computes this divergence from the structural dataâdirectionality, exit options, and role declarationsâwithout requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority sits near the beneficiary end (low d): it controls the interpretive apparatus and is structurally subsidized by the arrangement's legitimacy. The Torah study community sits slightly above symmetric (moderate d): they benefit from meaning and status but pay in labor and constrained exit. Temple restoration advocates sit near the target end (high d): they bear the cost of marginalization and normative exclusion. No directionality overrides are needed because the structural derivation captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâTemple destruction and consequent inability to perform sacrificesâremains live two millennia later. The constraint is therefore not a piton: its function has not atrophied, and its maintenance is not theatrical. The classification as rope is supported by the continued live need for covenantal continuity mechanisms in diaspora and the absence of a victim set extracting from the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_vs_performance_ontological_status,
    'Does study of sacrifice law genuinely fulfill the divine obligation in an ontological sense, or is it a pragmatic communal survival mechanism retroactively elevated to normative status?',
    'Historical-philological analysis of early rabbinic literature versus Second Temple texts; detection of retroactive elevation versus original intent.',
    'If retroactive, the constraint''s extraction (cognitive labor) is higher than claimed because it compensates for an unresolvable loss; if original, the coordination function is genuinely continuous with the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_vs_performance_ontological_status, conceptual, 'Whether study-fulfillment is ontologically real or retroactive justification').

omega_variable(
    authority_extraction_ambiguity,
    'Does the rabbinic authority structure benefit from maintaining the ''study as occupation'' reading beyond the coordination need, by consolidating interpretive control over a non-performable commandment?',
    'Comparative analysis of rabbinic resource allocation and status attribution across commandments that are currently performable versus non-performable.',
    'If authority extracts status or legitimacy disproportionately, the constraint drifts toward tangled_rope; if not, it remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_extraction_ambiguity, conceptual, 'Whether interpretive authority over non-performable commandments concentrates unilateral benefit').

omega_variable(
    kernel_reading_incommensurability,
    'Can the three sibling readings of this kernel be adjudicated within a single halakhic framework, or do they represent mutually exclusive foundational commitments?',
    'Survey of halakhic adjudication practicesâwhether a single decisor can hold all three as legitimate options or must choose one to the exclusion of others.',
    'If mutually exclusive, the forecloses relations declared in this story are structurally accurate; if pluralistically co-holdable, the relations should be coexists_with, altering the kernel dynamics from zero-sum to polyvocal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether sibling readings are mutually exclusive or co-holdable within one framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 500, 0.12).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 2000, 0.2).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 500, 0.14).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1000, 0.15).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1500, 0.16).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 2000, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__study_as_occupation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'temple_sacrifice_obligation'. The kernel decomposes into structurally distinct claims because the epsilon values and victim/beneficiary structures differ across readings: this reading claims low extraction and no victims; siblings claim different functional relationships to the obligation. See sibling constraint files for their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
