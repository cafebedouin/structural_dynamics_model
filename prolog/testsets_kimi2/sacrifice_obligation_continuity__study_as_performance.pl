% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Study of Sacrifice Law as Ritual Fulfillment
 *   domain: religious_law/ritual_studies
 *
 * SUMMARY:
 *   This constraint instantiates the study_as_performance reading of the
 *   sacrifice_obligation_continuity kernel. Within this reading, the biblical
 *   commandment to offer sacrifice remains fully binding and is actively
 *   fulfilled through cognitive engagement with sacrificial texts. The
 *   physical impossibility of Temple worship is addressed by a
 *   substitutionary hermeneutic rather than by suspension or archival
 *   preservation. Sibling readingsâperformance_only, messianic_suspension,
 *   and archival_preservationâare live positions held by other parties but
 *   are not described inside this constraint story per the Îµ-invariance
 *   rule. The authored metrics reflect low extraction and low suppression
 *   because the fulfillment mechanism is accessible, non-monetary, and does
 *   not actively suppress alternatives; the engine will compute any
 *   divergence from the rope claim.
 *
 * KEY AGENTS:
 *   - students_of_sacrifice_law (moderate/constrained) â primary beneficiaries who experience the constraint as accessible fulfillment
 *   - rabbinic_authorities (institutional/arbitrage) â agenda-setters who maintain the interpretive tradition
 *   - diaspora_communities (organized/constrained) â beneficiaries who gain normative continuity without Temple access
 *   - temple_restoration_advocates (moderate/constrained) â excluded voices marginalized by the sufficiency claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.18).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.15).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Study of Sacrifice Law as Ritual Fulfillment").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86').
narrative_ontology:cs_kernel_codification('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', fixed_text).
narrative_ontology:cs_authority_grounding('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', lineage).
narrative_ontology:cs_interpretation_layer_present('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86').
narrative_ontology:cs_reading_relation('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_axiom('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', foundational, textual_study_fulfills_sacrifice_commandment).
narrative_ontology:cs_axiom_status(textual_study_fulfills_sacrifice_commandment, holdable).
narrative_ontology:cs_axiom_grounding('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', textual_study_fulfills_sacrifice_commandment, theological).
narrative_ontology:cs_axiom('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', foundational, covenant_obligation_unbroken).
narrative_ontology:cs_axiom_status(covenant_obligation_unbroken, holdable).
narrative_ontology:cs_axiom_grounding('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', covenant_obligation_unbroken, theological).
narrative_ontology:cs_reference_frame('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', normative_torah_framework).
narrative_ontology:cs_drift_state('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('92b4e15d-952e-48f4-8ffb-6cf6ad6a7f86', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, students_of_sacrifice_law).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, diaspora_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in daily study of Temple-sacrifice tractates as a form of worship. They understand this cognitive engagement to be ritually equivalent to actual performance, generating spiritual merit and satisfying the biblical commandment without physical or financial prerequisites beyond literacy and time.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, students_of_sacrifice_law, beneficiary,
    moderate, biographical, constrained, global).

% Maintain the interpretive tradition that equates textual study with sacrificial worship. They teach the curriculum, publish commentaries, and decide halakhic questions. Their authority depends on the continuity of the textual chain and the legitimacy of substitutionary fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, rabbinic_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Communities scattered across the diaspora who lack access to the Temple and priesthood. The study-as-performance reading allows them to maintain covenantal continuity and normative satisfaction without geographical or institutional access to the Jerusalem cult.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% Advocate for rebuilding the Temple and restoring physical sacrificial worship. They regard study as inadequate and await messianic restoration. They are not structurally victimized by the study-as-performance constraint itself but are marginalized in rabbinic discourse that treats study as already sufficient.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, temple_restoration_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains covenantal continuity and communal normative structure for a people separated from its central cultic site, by substituting accessible cognitive engagement for geographically and politically impossible physical ritual.
% TRANSFER_FUNCTION: Moves religious obligation from the physical domain of animal sacrifice in Jerusalem to the cognitive and textual domain of sacrificial-law study, distributing fulfillment capacity across all literate members regardless of location or priestly status.
% ABSENT_VOICES: Temple restoration advocates and holders of the messianic-suspension reading are present in broader discourse but structurally backgrounded by a tradition that treats study as already sufficient; secular archival-preservation voices sit outside the normative framework entirely and would reframe the texts as cultural memory rather than binding command.
% DISAPPEARANCE_RATIONALE: If the principle vanished, diaspora communities would lose their primary mechanism for sacrificial-commandment fulfillment; the normative structure would tilt toward messianic suspension or performance-only restorationism, and the daily study curriculum would lose its ritual equivalence.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the loss of access to the Jerusalem cult, creating a crisis of covenantal continuity and commandment-fulfillment for a religion centered on sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: Historians and archaeologists outside the rabbinic beneficiary set attest to the Temple's destruction and the sociological crisis it produced; the Roman-Jewish war record corroborates the founding trauma from a non-rabbinic seat.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint transfers obligation into an accessible cognitive domain rather than extracting material rents. Suppression is low (0.15) because alternatives (messianic suspension, performance-only hope) are not actively persecuted and coexist as live theological options. Theater ratio is low (0.12) because the study practice is functionally integral to communal religious life rather than performative maintenance. Accessibility collapse is low (0.20) because other readings remain thinkable and are openly discussed. Resistance is low (0.10) because the reading is broadly accepted within its target communities.
 *
 * PERSPECTIVAL GAP:
 *   Divergence between seats is minimal: the agenda-setter sees a delicate hermeneutic achievement preserving covenantal law, while the beneficiary sees meritorious and satisfying practice. Both experience the constraint as coordination rather than extraction. The engine should compute a rope classification across all seated perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   All named agents are either beneficiaries (students, diaspora communities) or agenda-setters (rabbinic authorities). There is no victim set. Because every situated agent is a net beneficiary of the arrangement, directionality derives toward the subsidy end (low d) for all seats. The rabbinic authorities' d is slightly higher than the students' because they bear the maintenance burden of the interpretive tradition, but both sit well below the symmetric threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâloss of the Templeâis still live because the Temple remains absent and the political conditions for restoration do not exist. The arrangement persists because it continues to solve the problem it was built for, not because it has atrophied into zombie maintenance. Consequently mandatrophy_resolved is not declared; the R5 interview records founding_problem_status as live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_fulfillment_vs_preparation_framing,
    'Is textual study structurally capable of fulfilling a commandment originally directed at physical performance, or is the study-as-performance reading a post-hoc rabbinic construction to manage post-Temple discontinuity?',
    'Theological and philosophical analysis of ritual substitution in halakhic thought; historical philology tracing the emergence of the ''study fulfills'' principle in Talmudic literature.',
    'If study is genuinely divine fulfillment, the constraint remains a rope. If it is a post-hoc construction masking abandonment, the constraint trends toward scaffold or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_fulfillment_vs_preparation_framing, conceptual, 'Whether textual substitution is intrinsic or constructed.').

omega_variable(
    rabbinic_agency_in_fulfillment_claim,
    'Does the study-as-performance reading concentrate religious authority in the textual-interpreter class, creating extraction disguised as coordination?',
    'Sociological analysis of power and status distribution in communities that organize around Talmud study, measuring whether interpretive authority translates into material or status rents.',
    'If authority concentration is high, effective extraction rises and the constraint may compute as tangled_rope rather than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rabbinic_agency_in_fulfillment_claim, empirical, 'Whether the reading functions as covert authority concentration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sacr_tr_t4, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 4, 0.09).
narrative_ontology:measurement(sacr_tr_t8, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 8, 0.1).
narrative_ontology:measurement(sacr_tr_t12, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 12, 0.1).
narrative_ontology:measurement(sacr_tr_t16, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 16, 0.11).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 20, 0.12).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sacr_be_t4, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 4, 0.16).
narrative_ontology:measurement(sacr_be_t8, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(sacr_be_t12, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 12, 0.17).
narrative_ontology:measurement(sacr_be_t16, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% The natural-language concept 'sacrifice obligation continuity' decomposes into four structurally distinct constraint stories. Each reading has a different epsilon, beneficiary structure, and normative force. This story (study_as_performance) is the low-extraction, beneficiary-only reading. The siblings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
