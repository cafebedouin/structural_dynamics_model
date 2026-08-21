% ============================================================================
% CONSTRAINT STORY: human_dignity_ai_safeguarding__imago_dei_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_dignity_ai_safeguarding__imago_dei_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: human_dignity_ai_safeguarding__imago_dei_reading
 *   human_readable: Human Dignity as Imago Dei in AI Governance (Imago Dei Reading)
 *   domain: theological_ethics/technology_governance/philosophical_anthropology
 *
 * SUMMARY:
 *   This constraint instantiates the 'Imago Dei' reading of human dignity in
 *   the context of AI and biotechnology governance. It asserts that human
 *   dignity is an inviolable, divinely-given attribute, equal in all persons
 *   prior to any capability. This principle mandates that AI must remain a
 *   subordinate tool, and categorically rejects human enhancement or
 *   transhumanist agendas that would challenge this fixed definition of
 *   humanity. The constraint is presented as a foundational, immutable truth
 *   by its adherents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_dignity_ai_safeguarding__imago_dei_reading, 0.65).
domain_priors:suppression_score(human_dignity_ai_safeguarding__imago_dei_reading, 0.85).
domain_priors:theater_ratio(human_dignity_ai_safeguarding__imago_dei_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_dignity_ai_safeguarding__imago_dei_reading, mountain).
narrative_ontology:human_readable(human_dignity_ai_safeguarding__imago_dei_reading, "Human Dignity as Imago Dei in AI Governance (Imago Dei Reading)").
narrative_ontology:topic_domain(human_dignity_ai_safeguarding__imago_dei_reading, "theological_ethics/technology_governance/philosophical_anthropology").

domain_priors:requires_active_enforcement(human_dignity_ai_safeguarding__imago_dei_reading).
domain_priors:emerges_naturally(human_dignity_ai_safeguarding__imago_dei_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_dignity_ai_safeguarding__imago_dei_reading, 'ed6c529c-91bd-42f1-b455-53fba603761b').
narrative_ontology:cs_kernel_codification('ed6c529c-91bd-42f1-b455-53fba603761b', fixed_text).
narrative_ontology:cs_authority_grounding('ed6c529c-91bd-42f1-b455-53fba603761b', lineage).
narrative_ontology:cs_interpretation_layer_present('ed6c529c-91bd-42f1-b455-53fba603761b').
narrative_ontology:cs_reading_relation('ed6c529c-91bd-42f1-b455-53fba603761b', human_dignity_ai_safeguarding__autonomy_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed6c529c-91bd-42f1-b455-53fba603761b', human_dignity_ai_safeguarding__posthumanist_reading, forecloses).
narrative_ontology:cs_axiom('ed6c529c-91bd-42f1-b455-53fba603761b', foundational, human_dignity_divinely_endowed).
narrative_ontology:cs_axiom_status(human_dignity_divinely_endowed, holdable).
narrative_ontology:cs_axiom_grounding('ed6c529c-91bd-42f1-b455-53fba603761b', human_dignity_divinely_endowed, theological).
narrative_ontology:cs_axiom('ed6c529c-91bd-42f1-b455-53fba603761b', foundational, human_nature_immutable).
narrative_ontology:cs_axiom_status(human_nature_immutable, holdable).
narrative_ontology:cs_axiom_grounding('ed6c529c-91bd-42f1-b455-53fba603761b', human_nature_immutable, deontological).
narrative_ontology:cs_reference_frame('ed6c529c-91bd-42f1-b455-53fba603761b', classical_theological_anthropology).
narrative_ontology:cs_drift_state('ed6c529c-91bd-42f1-b455-53fba603761b', contemporary_ai_biotech_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ed6c529c-91bd-42f1-b455-53fba603761b', '').
narrative_ontology:cs_kernel_id(human_dignity_ai_safeguarding__imago_dei_reading, human_dignity_ai_safeguarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__imago_dei_reading, humanity_as_imago_dei).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__imago_dei_reading, theological_authorities).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__imago_dei_reading, transhumanists).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__imago_dei_reading, ai_enhancement_advocates).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__imago_dei_reading, secular_autonomy_ethicists).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__imago_dei_reading, ai_developers_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, interpret, and enforce the doctrine of Imago Dei as the foundation of human dignity, guiding ethical boundaries for AI and biotechnology. They benefit from the preservation of their moral authority and the stability of this foundational concept.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, theological_authorities, agenda_setter,
    institutional, civilizational, identity_locked, universal).

% Benefits from the protection of human dignity as divinely endowed and inviolable, ensuring AI remains a subordinate tool and preventing forms of enhancement or synthetic personhood that would challenge this definition. This group includes adherents to the theological framework.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, humanity_as_imago_dei, beneficiary,
    organized, generational, identity_locked, global).

% Bear the cost of suppressed research and development paths aimed at human enhancement or radical alteration, facing moral condemnation and regulatory barriers from this ethical framework.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, transhumanists, payer,
    organized, biographical, constrained, global).

% Face significant ethical and regulatory barriers to their work on AI-driven human enhancement, as such endeavors are seen as challenging the divinely-given and immutable nature of human dignity.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, ai_enhancement_advocates, payer,
    powerful, biographical, constrained, global).

% Their alternative ethical frameworks, which ground dignity in human autonomy, rationality, or rights rather than divine image, are marginalized or dismissed by the dominant theological view, limiting their influence on policy.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, secular_autonomy_ethicists, payer,
    organized, biographical, constrained, global).

% Must navigate the ethical boundaries set by this doctrine, which can limit certain lines of inquiry or application in AI development, particularly those touching on human-AI integration or synthetic intelligence with personhood claims.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, ai_developers_researchers, payer,
    moderate, biographical, constrained, global).

% Their fundamental premise of human mutability and the potential for dignity in non-biological or enhanced forms is categorically rejected by this reading, effectively excluding their voice from the core ethical debate within this framework.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__imago_dei_reading, posthumanist_philosophers, excluded,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_dignity_ai_safeguarding__imago_dei_reading, theological_authorities).
narrative_ontology:fixing_cost_class(human_dignity_ai_safeguarding__imago_dei_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, divinely-grounded ethical framework for humanity's relationship with AI and biotechnology, aiming to prevent existential risks and preserve human uniqueness as defined by the Imago Dei doctrine.
% TRANSFER_FUNCTION: Transfers moral authority and definitional power over 'human' from secular or evolving philosophical frameworks to theological doctrine. It also transfers the burden of ethical compliance onto AI developers and those pursuing enhancement, restricting their freedom of action.
% ABSENT_VOICES: Posthumanist philosophers, secular ethicists, and transhumanist advocates are structurally excluded. They would argue for human self-determination, evolving definitions of personhood, and the potential benefits of enhancement, but their premises are deemed incompatible with this framework.
% DISAPPEARANCE_RATIONALE: If this understanding of dignity vanished, the ethical guardrails for AI development would shift dramatically. The categorical rejection of enhancement and synthetic personhood would dissolve, potentially opening paths for radical human alteration and a re-evaluation of human-AI relationships without a fixed, divinely-ordained boundary. The moral authority of theological institutions in this domain would also diminish significantly.
% FOUNDING_PROBLEM: The perceived threat of rapidly advancing technology (especially AI and biotechnology) to human identity, uniqueness, and moral status, leading to a need for an immutable, transcendent ethical foundation to safeguard humanity.
% FOUNDING_PROBLEM_CORROBORATION: Theological scholars and religious institutions universally attest to the problem's live status, citing ongoing technological advancements. Independent philosophical anthropologists and some AI ethicists (from outside the direct beneficiary group) also acknowledge the profound challenge AI poses to human identity, though they may not agree on the Imago Dei solution.
narrative_ontology:disappearance_verdict(human_dignity_ai_safeguarding__imago_dei_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_dignity_ai_safeguarding__imago_dei_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_dignity_ai_safeguarding__imago_dei_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(human_dignity_ai_safeguarding__imago_dei_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_dignity_ai_safeguarding__imago_dei_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_dignity_ai_safeguarding__imago_dei_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_dignity_ai_safeguarding__imago_dei_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, ExtMetricName, E),
    domain_priors:suppression_score(human_dignity_ai_safeguarding__imago_dei_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(human_dignity_ai_safeguarding__imago_dei_reading),
    narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__imago_dei_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(human_dignity_ai_safeguarding__imago_dei_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because, from the perspective of its adherents, it represents an unchangeable, divinely-ordained truth. However, its operation involves substantial extractiveness (0.65) by limiting the freedom of those pursuing alternative technological or philosophical paths, and very high suppression (0.85) through doctrinal authority and active enforcement against perceived threats. The low theater ratio (0.1) reflects that its defense is seen as a genuine, core mission, not mere performance. The metrics show a slight increase over time as technological challenges intensify, requiring more active defense and suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of theological authorities and Imago Dei adherents, this constraint is a protective, non-extractive truth that safeguards humanity. From the perspective of transhumanists, AI enhancement advocates, and secular ethicists, the same structure operates as a highly extractive and suppressive force, limiting their research, philosophical inquiry, and freedom to define human flourishing differently. The engine's classification will measure this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Theological authorities and humanity as defined by Imago Dei are the primary beneficiaries, gaining moral authority and protection of their worldview. Transhumanists, AI enhancement advocates, secular autonomy ethicists, and AI developers are the targets, bearing the costs of restricted research, development, and philosophical discourse. Posthumanist philosophers are explicitly excluded, their fundamental premises rejected by this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of this constraint—to protect human dignity as Imago Dei—is considered eternally live and increasingly vital by its proponents in the face of technological advancement. This prevents any internal recognition of mandatrophy. The engine's classification will assess whether this 'live' status is structurally supported or if the constraint persists primarily through suppression of alternatives, despite a potentially atrophied coordination function for a broader society.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, universally applicable theological truth, or one specific reading of human dignity contested by alternative philosophical and ethical frameworks?',
    'Comparative analysis of ethical frameworks across diverse cultures and philosophical traditions, assessing the universality of the ''Imago Dei'' concept versus other dignity groundings (e.g., autonomy, sentience).',
    'If it is a specific reading, its ''Mountain'' claim would be reclassified to reflect its constructed and enforced nature, likely as a Tangled Rope or Snare, depending on the degree of extraction and suppression of alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between a universal truth and a specific, contested ethical framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily doctrinal/ideological (internalized by adherents) or institutional/regulatory (structural for non-adherents)?',
    'Analysis of compliance mechanisms: if adherence is primarily driven by internal conviction within the faith community, it''s internalized; if it relies on external regulatory barriers, funding restrictions, or social pressure on non-adherents, it''s structural.',
    'If primarily internalized, the constraint''s effective suppression is higher for adherents, as they carry the suppression with them. If primarily structural, the constraint''s persistence relies more heavily on active enforcement against external challengers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ethical adherence.').

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the ''Imago Dei'' concept a natural/divine law, or a constructed theological doctrine that serves to maintain a particular social and ethical order?',
    'Historical and sociological analysis of the concept''s development and application across different eras and cultures, examining its adaptability and the interests it served.',
    'If primarily a constructed doctrine, the ''Mountain'' classification would be challenged, potentially reclassifying it as a Tangled Rope or Snare, as its ''naturalness'' would be revealed as a legitimizing narrative for an enforced social order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Ambiguity of Imago Dei as natural law versus constructed doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_dignity_ai_safeguarding__imago_dei_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(huma_tr_t10, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(huma_tr_t20, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(huma_tr_t30, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(huma_tr_t40, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(huma_tr_t50, human_dignity_ai_safeguarding__imago_dei_reading, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(huma_be_t10, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(huma_be_t20, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(huma_be_t30, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(huma_be_t40, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(huma_be_t50, human_dignity_ai_safeguarding__imago_dei_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(huma_su_t10, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(huma_su_t20, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 20, 0.81).
narrative_ontology:measurement(huma_su_t30, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(huma_su_t40, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(huma_su_t50, human_dignity_ai_safeguarding__imago_dei_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_dignity_ai_safeguarding__imago_dei_reading, identity_coordination).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__imago_dei_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__imago_dei_reading, biotech_regulation).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__imago_dei_reading, human_dignity_ai_safeguarding__autonomy_rights_reading).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__imago_dei_reading, human_dignity_ai_safeguarding__posthumanist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'human_dignity_ai_safeguarding' kernel. Its ε value and structural properties differ significantly from the 'autonomy_rights_reading' and 'posthumanist_reading' due to its specific theological grounding and categorical rejections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
