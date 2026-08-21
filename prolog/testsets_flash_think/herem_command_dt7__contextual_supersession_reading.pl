% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command: Contextual Supersession Reading
 *   domain: religious_ethics/biblical_hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the 'contextual supersession' reading of the
 *   biblical Herem command (Deuteronomy 7). This reading interprets Herem as
 *   a historically bounded directive specific to ancient Israel's settlement
 *   period, morally superseded by later prophetic universalism or the
 *   Christian covenant. Its primary function is to coordinate an ethical
 *   understanding of difficult ancient texts, allowing adherents to reconcile
 *   them with broader moral principles. The low extractiveness and
 *   suppression reflect the diminished moral force of the literal command
 *   within this interpretive framework, though it acknowledges residual harm
 *   from other, more literal readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.15).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.1).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command: Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious_ethics/biblical_hermeneutics").

narrative_ontology:has_sunset_clause(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, 'ebfce8e1-00e8-4eb3-8d69-c4474cde1cad').
narrative_ontology:cs_kernel_codification('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', fixed_text).
narrative_ontology:cs_authority_grounding('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', lineage).
narrative_ontology:cs_interpretation_layer_present('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad').
narrative_ontology:cs_reading_relation('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', foundational, divine_ethics_evolve_or_unfold).
narrative_ontology:cs_axiom_status(divine_ethics_evolve_or_unfold, holdable).
narrative_ontology:cs_axiom_grounding('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', divine_ethics_evolve_or_unfold, deontological).
narrative_ontology:cs_axiom('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', foundational, universal_moral_principles_transcend_particular_commands).
narrative_ontology:cs_axiom_status(universal_moral_principles_transcend_particular_commands, holdable).
narrative_ontology:cs_axiom_grounding('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', universal_moral_principles_transcend_particular_commands, deontological).
narrative_ontology:cs_reference_frame('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', prophetic_universalism_or_new_covenant).
narrative_ontology:cs_drift_state('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ebfce8e1-00e8-4eb3-8d69-c4474cde1cad', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, adherents_of_contextual_supersession).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, individuals_subject_to_literalist_enforcement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_literalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These adherents find moral clarity and ethical consistency by understanding Herem as a historically bounded directive, superseded by later universalist or covenantal ethics. They are freed from the moral burden of literal application.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, adherents_of_contextual_supersession, beneficiary,
    organized, generational, mobile, global).

% Theologians and scholars who advocate for this reading, emphasizing the moral trajectory of scripture towards universal inclusion and justice. They shape the interpretive framework.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, prophetic_universalists, agenda_setter,
    institutional, civilizational, analytical, global).

% Scholars and religious leaders within Christian traditions who interpret Herem as superseded by the New Covenant, emphasizing grace and universal love. They contribute to the interpretive framework.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, christian_theologians, agenda_setter,
    institutional, civilizational, analytical, global).

% Those who adhere to a literal, timeless interpretation of Herem. They bear the cost of intellectual and moral challenge from this reading, and may face social pressure or internal conflict if their communities are exposed to it. They may also be the source of residual enforcement.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_literalists, payer,
    organized, biographical, identity_locked, local).

% Individuals who are still subject to social exclusion, condemnation, or other forms of harm within communities that maintain a literal, timeless application of Herem (e.g., intermarriage bans, ethnic separation). This reading identifies their plight.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, individuals_subject_to_literalist_enforcement, payer,
    powerless, immediate, trapped, local).

% Academics and thinkers who analyze religious texts and their ethical implications from a non-theological perspective. They observe the internal theological debates and their real-world consequences.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, secular_ethicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__contextual_supersession_reading, adherents_of_contextual_supersession).
narrative_ontology:fixing_cost_class(herem_command_dt7__contextual_supersession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates an ethical understanding of ancient biblical texts, allowing adherents to reconcile historically specific and violent commands with universal moral principles and later theological developments.
% TRANSFER_FUNCTION: Transfers moral authority from the literal, historical application of Herem to a broader, universalist or covenantal ethical framework, thereby re-orienting moral obligations for contemporary believers.
% ABSENT_VOICES: Ancient Canaanite populations (historical victims of Herem) are absent from the interpretive discourse. Also, those who advocate for a purely secular, non-religious ethical framework are often excluded from internal theological debates.
% DISAPPEARANCE_RATIONALE: If this reading vanished, many adherents would struggle to reconcile ancient texts with modern ethics, potentially leading to moral relativism, abandonment of faith due to perceived ethical contradictions, or a return to literal, harmful interpretations of Herem, increasing the burden on individuals subject to such enforcement.
% FOUNDING_PROBLEM: Reconciling the violent, exclusionary commands of ancient texts (like Herem in Deuteronomy 7) with later prophetic teachings emphasizing justice, mercy, and universal inclusion, or with the Christian New Covenant's emphasis on love and grace.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream theological scholarship, interfaith dialogue initiatives, and ethical philosophy from outside specific religious traditions consistently highlight the ongoing tension between ancient texts and universal ethics, corroborating the problem's live status.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.1) reflect that this reading largely neutralizes the coercive and extractive potential of the literal Herem command for its adherents. The 'has_sunset_clause: true' flag directly maps to the 'historically-bounded' aspect. The 'rope' classification is chosen because it successfully coordinates a complex ethical problem for its community, providing a coherent moral framework. The decreasing extractiveness over time reflects the increasing consolidation and acceptance of this supersessionist view within theological discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents of this reading, the constraint is a beneficial coordination mechanism. However, from the perspective of fundamentalist literalists, this reading itself might be seen as an attack on divine authority, creating a different kind of 'extraction' (of their traditional understanding). The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Adherents of this reading are beneficiaries, gaining moral coherence and freedom from literalist burdens. Prophetic universalists and Christian theologians act as agenda-setters, shaping and propagating this interpretation. Fundamentalist literalists are payers, as their worldview is challenged, and they may face pressure to adapt. Individuals subject to literalist enforcement are also payers, as they bear the direct costs of other readings, which this reading seeks to delegitimize.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supersession_completeness_ambiguity,
    'Is the supersession of Herem truly complete within this reading, or does it implicitly retain elements of separation or exclusion, merely re-framing them?',
    'Detailed textual analysis of contemporary theological applications of this reading, examining whether any ''superseded'' elements are subtly re-introduced in new forms (e.g., spiritualized exclusion).',
    'If elements are implicitly retained, the effective extractiveness and suppression of this reading would be higher than currently assessed, potentially shifting its classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_completeness_ambiguity, conceptual, 'Ambiguity regarding the completeness of Herem''s supersession.').

omega_variable(
    acceptance_scope_ambiguity,
    'How widely is this ''contextual supersession'' reading accepted and applied within the broader religious communities that engage with the Herem text?',
    'Sociological and theological surveys of diverse religious communities, analyzing sermon content, theological publications, and educational curricula to gauge the prevalence and depth of this interpretation.',
    'If acceptance is narrow, the constraint''s effective reach in mitigating harm from literal interpretations is limited, and the ''victims'' group remains larger and more vulnerable than this reading''s internal logic suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptance_scope_ambiguity, empirical, 'The actual scope of acceptance for this interpretive reading.').

omega_variable(
    residual_enforcement_impact,
    'What is the measurable impact of residual fundamentalist enforcement of literal Herem interpretations on individuals, despite the existence of supersessionist readings?',
    'Ethnographic studies and qualitative interviews with individuals in communities where literal interpretations persist, documenting instances of social exclusion, discrimination, or other harms.',
    'If the impact is severe and widespread, the ''victims'' group''s powerlessness and trapped exit options are amplified, highlighting the ongoing struggle against literalist readings and potentially re-framing the overall constraint as more extractive due to the persistence of its ''shadow''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(residual_enforcement_impact, empirical, 'The real-world harm from persistent literal interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 100, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__contextual_supersession_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement(here_tr_t500, herem_command_dt7__contextual_supersession_reading, theater_ratio, 500, 0.07).
narrative_ontology:measurement(here_tr_t1000, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1000, 0.06).
narrative_ontology:measurement(here_tr_t1500, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(here_tr_t2000, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(here_be_t100, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 100, 0.3).
narrative_ontology:measurement(here_be_t500, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 500, 0.25).
narrative_ontology:measurement(here_be_t1000, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1000, 0.2).
narrative_ontology:measurement(here_be_t1500, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1500, 0.18).
narrative_ontology:measurement(here_be_t2000, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t100, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 100, 0.2).
narrative_ontology:measurement(here_su_t500, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 500, 0.15).
narrative_ontology:measurement(here_su_t1000, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1000, 0.12).
narrative_ontology:measurement(here_su_t1500, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1500, 0.11).
narrative_ontology:measurement(here_su_t2000, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
