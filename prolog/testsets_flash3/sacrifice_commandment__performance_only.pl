% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment: Performance-Only Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'performance-only' reading of the
 *   sacrifice commandment within Halakha, which asserts that the commandment
 *   requires physical execution and is therefore suspended in the absence of
 *   the Temple. This reading drives significant scholarly labor towards the
 *   theoretical study of unperformable laws, diverting intellectual and
 *   communal resources from other areas. The high extractiveness reflects the
 *   opportunity cost of this sustained, unfulfillable intellectual commitment
 *   over nearly two millennia. The claimed type is 'snare' because the
 *   coordination story (preserving knowledge) serves as cover for the
 *   extraction of scholarly attention and resources, maintained by
 *   institutional authority and identity-lock mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.85).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.9).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, snare).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment: Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious_studies/halakhic_theory/commitment_system_analysis").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '5cdd6436-799f-4a15-9213-f63427db1d33').
narrative_ontology:cs_kernel_codification('5cdd6436-799f-4a15-9213-f63427db1d33', fixed_text).
narrative_ontology:cs_authority_grounding('5cdd6436-799f-4a15-9213-f63427db1d33', lineage).
narrative_ontology:cs_interpretation_layer_present('5cdd6436-799f-4a15-9213-f63427db1d33').
narrative_ontology:cs_reading_relation('5cdd6436-799f-4a15-9213-f63427db1d33', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('5cdd6436-799f-4a15-9213-f63427db1d33', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('5cdd6436-799f-4a15-9213-f63427db1d33', foundational, physical_performance_is_sine_qua_non).
narrative_ontology:cs_axiom_status(physical_performance_is_sine_qua_non, holdable).
narrative_ontology:cs_axiom_grounding('5cdd6436-799f-4a15-9213-f63427db1d33', physical_performance_is_sine_qua_non, deontological).
narrative_ontology:cs_axiom('5cdd6436-799f-4a15-9213-f63427db1d33', foundational, commandment_suspended_not_fulfilled_without_temple).
narrative_ontology:cs_axiom_status(commandment_suspended_not_fulfilled_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('5cdd6436-799f-4a15-9213-f63427db1d33', commandment_suspended_not_fulfilled_without_temple, conventional).
narrative_ontology:cs_reference_frame('5cdd6436-799f-4a15-9213-f63427db1d33', halakhic_continuity_post_temple_destruction).
narrative_ontology:cs_drift_state('5cdd6436-799f-4a15-9213-f63427db1d33', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5cdd6436-799f-4a15-9213-f63427db1d33', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, yeshiva_institutions).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, scholarly_attention).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, community_resources).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and transmitters of Halakha, who maintain the doctrine that Temple sacrifices require physical performance. Their careers and intellectual identity are deeply intertwined with the study and preservation of these laws, even in their unperformable state. They benefit from the intellectual labor directed at this complex, suspended body of law.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Educational institutions that dedicate significant curriculum and resources to the study of sacrificial laws. They benefit from the sustained intellectual activity and the prestige associated with mastering this intricate, ancient legal system, attracting students and funding based on this commitment.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% The collective intellectual effort and focus of the rabbinic community, which is directed towards the detailed study of unperformable sacrificial laws. This attention is 'paid' in the sense that it is diverted from other areas of Halakha or contemporary ethical/social issues, representing a significant opportunity cost.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, scholarly_attention, payer,
    powerless, biographical, trapped, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_commandment__performance_only, scholarly_attention).

% Financial and human resources within the Jewish community that are allocated to support institutions and scholars engaged in the study of sacrificial laws. These resources are 'paid' in that they could otherwise be directed to other communal needs, social welfare, or more immediately applicable religious practices.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, community_resources, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(sacrifice_commandment__performance_only, community_resources).

% Individuals who adhere to the rabbinic tradition and internalize the belief that sacrifices are suspended. They bear the 'cost' of a religious practice that cannot be fully observed, leading to a sense of incompleteness or deferred fulfillment, and contribute to the communal resources that fund its study.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, lay_adherents, payer,
    moderate, biographical, identity_locked, local).

% Groups who actively seek the rebuilding of the Temple and the resumption of sacrifices. While they agree on the performance-only aspect, they are excluded from mainstream rabbinic discourse regarding the *timing* and *means* of restoration, often viewed as premature or overly zealous. Their efforts are suppressed by the dominant rabbinic consensus on suspension.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_activists, excluded,
    organized, immediate, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a consistent understanding of the sacrifice commandment across generations, ensuring that the detailed laws are preserved and transmitted, even in the absence of the Temple, preventing fragmentation of halakhic knowledge.
% TRANSFER_FUNCTION: Transfers scholarly attention and communal resources towards the theoretical study of unperformable sacrificial laws, from the broader community to rabbinic institutions and scholars.
% ABSENT_VOICES: Messianic activists who advocate for immediate Temple rebuilding and resumption of sacrifices are largely excluded from mainstream halakhic decision-making, as are those who might argue for a more symbolic or spiritual interpretation of the commandment in the absence of the Temple.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished, the vast intellectual and institutional infrastructure dedicated to the study of unperformable sacrificial laws would collapse or reorient. Scholarly attention and communal resources would be redirected, potentially towards more immediately applicable halakhic areas or social action, fundamentally altering the landscape of Jewish religious life and scholarship.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) left the central commandment of sacrifice unperformable, creating a crisis of religious practice and continuity for the Jewish people.
% FOUNDING_PROBLEM_CORROBORATION: The problem of unperformable sacrifices is universally acknowledged across Jewish denominations and historical texts. While the *response* to the problem is contested (as seen in sibling readings), the problem itself is a foundational historical and theological reality, corroborated by centuries of rabbinic literature and historical accounts of the Temple's destruction.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) due to the immense, sustained intellectual and communal investment in a commandment that cannot be fulfilled. This represents a significant opportunity cost. Suppression (0.9) is also high, as the rabbinic authority actively maintains this interpretation, and the identity-locked nature of scholarly careers makes exit difficult. Theater ratio is low (0.1) because the study is genuinely rigorous and not merely performative; the 'snare' aspect comes from the *direction* of this genuine effort towards an unperformable act. The historical interval spans from the destruction of the Second Temple (70 CE) to the present, reflecting the continuous nature of this interpretive tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars, this is a 'rope' or even 'mountain' – a necessary and natural response to a divine commandment, ensuring its preservation. From the perspective of the extracted scholarly attention and community resources, it functions as a 'snare', diverting immense effort towards an unperformable task, with the coordination function (knowledge preservation) serving as a justification for the ongoing extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars and yeshiva institutions are beneficiaries, as they derive prestige, intellectual purpose, and institutional support from maintaining and teaching this complex body of law. Scholarly attention and community resources are the victims, as they are extracted and directed towards this suspended commandment. Lay adherents also bear a cost in the form of deferred religious fulfillment and resource contribution. Messianic activists are excluded, as their attempts to alter the 'suspended' status are actively resisted by the dominant interpretive authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving the sacrifice commandment) is technically 'live' in that the Temple has not been rebuilt. However, the *function* of the commandment (physical performance) is 'dead'. The 'snare' classification prevents mislabeling this as a 'rope' (genuine coordination) by highlighting the asymmetric extraction of resources and attention towards an unfulfillable mandate, maintained by identity-lock and institutional authority. The persistence is due to the deep identity fusion of scholars with this tradition, rather than a genuine, active coordination problem that benefits all parties symmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_fulfillment_ambiguity,
    'Is there a legitimate alternative mode of fulfilling the sacrifice commandment (e.g., prayer, study, ethical action) that this reading suppresses?',
    'Theological and halakhic re-evaluation by a broad consensus of rabbinic authorities, or the emergence of a widely accepted alternative interpretive tradition.',
    'If an alternative mode of fulfillment were recognized, the extractiveness of this reading would decrease significantly, as scholarly attention and resources could be redirected to performable acts. This would shift the classification towards a ''tangled_rope'' or ''rope'', depending on the degree of remaining extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fulfillment_ambiguity, conceptual, 'Whether the commandment''s essence is tied solely to physical performance or allows for other forms of fulfillment.').

omega_variable(
    identity_lock_vs_genuine_commitment,
    'To what extent is the continued dedication to studying unperformable sacrificial laws driven by genuine religious commitment versus professional identity-lock and institutional inertia?',
    'Sociological studies of rabbinic career paths and institutional funding models; counterfactual analysis of scholarly output if the Temple were rebuilt or an alternative fulfillment recognized.',
    'If identity-lock is the dominant driver, the ''snare'' classification is strongly reinforced, highlighting the self-perpetuating nature of the extraction. If genuine commitment is primary, the extractiveness might be re-evaluated as a ''cost of devotion'' rather than pure extraction, potentially shifting towards a ''tangled_rope'' or even ''rope'' from the scholar''s seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_genuine_commitment, empirical, 'Distinguishing between intrinsic religious motivation and structural professional incentives.').

omega_variable(
    kernel_reading_structural_delta,
    'Given the ''sacrifice_commandment'' kernel, how would the structural properties (extractiveness, suppression, beneficiaries, victims) change if a sibling reading (e.g., ''study_as_performance'' or ''archive_maintenance'') were adopted as dominant?',
    'Comparative analysis of historical periods or communities where sibling readings held greater sway, or counterfactual modeling of resource allocation under alternative interpretive regimes.',
    'The ''study_as_performance'' reading would likely reduce extractiveness and suppression, as study would be seen as fulfillment rather than deferred labor, shifting towards a ''rope''. The ''archive_maintenance'' reading would reframe the ''extraction'' as a necessary ''cost of preservation'', potentially reducing perceived extractiveness but maintaining a ''tangled_rope'' or ''snare'' if the cost is disproportionate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__performance_only, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__performance_only, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t1954, sacrifice_commandment__performance_only, theater_ratio, 1954, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__performance_only, base_extractiveness, 500, 0.75).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__performance_only, base_extractiveness, 1000, 0.8).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.83).
narrative_ontology:measurement(sacr_be_t1954, sacrifice_commandment__performance_only, base_extractiveness, 1954, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__performance_only, suppression_requirement, 500, 0.85).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__performance_only, suppression_requirement, 1000, 0.88).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.89).
narrative_ontology:measurement(sacr_su_t1954, sacrifice_commandment__performance_only, suppression_requirement, 1954, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_commandment' kernel. This 'performance_only' reading emphasizes physical execution, leading to the commandment's suspension and the redirection of scholarly labor. Sibling readings (study_as_performance, archive_maintenance) offer alternative interpretations of how the commandment is addressed in the absence of the Temple, with different implications for resource allocation and fulfillment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
