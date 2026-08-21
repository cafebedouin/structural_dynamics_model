% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Progressive Abrogation of Gender-Specific Qur'anic Verses
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies
 *
 * SUMMARY:
 *   This constraint instantiates the 'progressive_abrogation' reading of the
 *   'quranic_gender_verses' kernel. This reading posits that later
 *   egalitarian principles in the Qur'an (e.g., 49:13 universal human
 *   dignity) supersede earlier gender-specific rules (e.g., 4:11, 2:282,
 *   4:34) via the principle of naskh (abrogation), leading to a complete
 *   normative reversal. Sibling readings include 'literal_hierarchical' and
 *   'contextual_egalitarian'. The high extractiveness and suppression reflect
 *   the profound challenge this reading poses to established traditional
 *   authority and the active effort required to enforce its new interpretive
 *   framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.9).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.85).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.9).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Progressive Abrogation of Gender-Specific Qur'anic Verses").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '8fafdf5b-be0f-426b-b952-434b11c25443').
narrative_ontology:cs_kernel_codification('8fafdf5b-be0f-426b-b952-434b11c25443', fixed_text).
narrative_ontology:cs_authority_grounding('8fafdf5b-be0f-426b-b952-434b11c25443', lineage).
narrative_ontology:cs_interpretation_layer_present('8fafdf5b-be0f-426b-b952-434b11c25443').
narrative_ontology:cs_reading_relation('8fafdf5b-be0f-426b-b952-434b11c25443', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('8fafdf5b-be0f-426b-b952-434b11c25443', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('8fafdf5b-be0f-426b-b952-434b11c25443', foundational, quranic_egalitarianism_supersedes_hierarchy).
narrative_ontology:cs_axiom_status(quranic_egalitarianism_supersedes_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('8fafdf5b-be0f-426b-b952-434b11c25443', quranic_egalitarianism_supersedes_hierarchy, deontological).
narrative_ontology:cs_axiom('8fafdf5b-be0f-426b-b952-434b11c25443', foundational, naskh_applies_to_gender_specific_verses).
narrative_ontology:cs_axiom_status(naskh_applies_to_gender_specific_verses, holdable).
narrative_ontology:cs_axiom_grounding('8fafdf5b-be0f-426b-b952-434b11c25443', naskh_applies_to_gender_specific_verses, conventional).
narrative_ontology:cs_reference_frame('8fafdf5b-be0f-426b-b952-434b11c25443', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('8fafdf5b-be0f-426b-b952-434b11c25443', contemporary_islamic_feminist_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8fafdf5b-be0f-426b-b952-434b11c25443', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, women_seeking_legal_parity).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_islamic_authorities).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, adherents_of_literal_interpretations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and develop the hermeneutical framework of progressive abrogation, seeking to establish a consistent egalitarian ethic in Islamic law. They benefit from the intellectual coherence and moral alignment this reading offers but face significant institutional resistance and potential marginalization within traditional structures.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Stand to gain full legal and social parity under this interpretation, as it directly challenges and supersedes traditional gender-specific rules. They are primary beneficiaries of the normative reversal this reading entails.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women_seeking_legal_parity, beneficiary,
    organized, biographical, constrained, global).

% Bear the cost of delegitimization and loss of authority, as their interpretations, often based on literal readings of gender-specific verses, are directly challenged and rendered obsolete by progressive abrogation. Their institutional power and intellectual lineage are undermined.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_islamic_authorities, payer,
    institutional, generational, identity_locked, global).

% Experience epistemic violence and cognitive dissonance as their deeply held beliefs about gender roles, derived from literal readings of the Qur'an, are declared superseded. They face pressure to abandon interpretations that are central to their identity and community norms.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, adherents_of_literal_interpretations, payer,
    moderate, biographical, identity_locked, local).

% Observe and often support the progressive abrogation reading as a means to achieve gender equality within Islamic frameworks, aligning with universal human rights principles. They provide external validation and pressure for such interpretations.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, secular_gender_equality_advocates, observer,
    organized, generational, analytical, global).

% Are often marginalized in the discourse, their voices dismissed as resistant to progress. Their identity and social structures are deeply intertwined with literal interpretations, making the progressive abrogation reading a profound threat to their way of life, with high costs for internalizing the new norm.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_reading, excluded,
    powerless, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a consistent, ethically coherent framework for Islamic law that aligns with universal human dignity, resolving perceived contradictions within the Qur'an by prioritizing later, universal principles over earlier, context-specific rules.
% TRANSFER_FUNCTION: Transfers normative authority from earlier, gender-specific Qur'anic verses to later, universal egalitarian principles, thereby shifting legal and social power from traditional male-dominated structures to women and progressive interpretations of Islamic law.
% ABSENT_VOICES: Communities whose identity and social structures are deeply bound to literal interpretations of gender-specific verses are often excluded from the discourse. They would object vehemently, viewing this reading as an attack on their faith and tradition, leading to profound social and theological disruption.
% DISAPPEARANCE_RATIONALE: If the principle of progressive abrogation for gender verses vanished, the legal and social landscape for women in many Muslim-majority contexts would revert to more traditional, hierarchical interpretations. This would significantly alter their rights, status, and opportunities, and reverse decades of progressive theological and legal work, reorganizing the entire discourse around gender in Islam.
% FOUNDING_PROBLEM: The perceived contradiction between early Qur'anic verses that appear to establish gender hierarchy (e.g., 4:11, 2:282, 4:34) and later verses emphasizing universal human dignity and equality (e.g., 49:13), creating an ethical and legal dilemma for modern Muslims seeking to reconcile faith with contemporary values.
% FOUNDING_PROBLEM_CORROBORATION: Progressive Islamic scholars, women's rights advocates within Muslim communities, and international human rights organizations attest to the ongoing ethical and legal tension. Academic studies in Islamic feminism and comparative law also corroborate the disparity between traditional interpretations and universal human rights norms.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.90) because this reading fundamentally reconfigures the normative landscape, stripping traditional interpretations of their legal force and delegitimizing the authority structures built upon them. Suppression (0.85) is also very high, as the establishment of this reading requires actively countering and marginalizing deeply entrenched literalist views and their proponents. Resistance is high (0.90) due to the direct challenge to established power and identity. The theater ratio is low (0.10) because this is a direct, impactful hermeneutical shift with real-world consequences, not a performative maintenance of an atrophied function. The increasing extractiveness and suppression over the interval reflect the growing intensity of the debate and the increasing pressure to adopt or resist this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of progressive scholars and women, this constraint is a necessary re-alignment of Islamic ethics with universal justice, a form of coordination towards a more equitable society. From the perspective of traditional authorities and literal adherents, it is a profound act of extraction, undermining their faith, authority, and social order. The engine's classification as a Tangled Rope captures this dual nature: a coordination function for a new ethical framework, but with significant extraction from those whose established norms are overturned.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive Islamic scholars and women seeking legal parity are clear beneficiaries, as the constraint directly empowers them and validates their ethical stance. Traditional Islamic authorities and adherents of literal interpretations are the primary targets/victims, as their established norms and power structures are directly undermined. Communities bound to literal readings are excluded, facing significant pressure to conform or be marginalized. Secular advocates act as observers, often lending support to the progressive reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutical_imposition_ambiguity,
    'Is the progressive abrogation reading a genuine internal hermeneutical development within Islamic jurisprudence, or an imposition of modern, secular egalitarian values onto the sacred text?',
    'Analysis of the historical development of naskh principles and their application in other contexts, as well as the internal consistency of the arguments with classical Islamic legal theory, independent of external ethical frameworks.',
    'If an imposition, its legitimacy within traditional Islamic discourse is weakened, increasing resistance and potentially reclassifying it as a Snare for those who perceive it as external coercion. If a genuine internal development, its persuasive power and potential for widespread adoption are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_imposition_ambiguity, conceptual, 'Ambiguity regarding the internal vs. external grounding of the progressive abrogation methodology.').

omega_variable(
    epistemic_violence_impact,
    'To what extent does the progressive abrogation reading constitute ''epistemic violence'' against communities whose identity is bound to literal interpretations, and what are the social and psychological costs?',
    'Sociological and anthropological studies of communities adhering to literal interpretations, documenting their experiences of marginalization, identity crisis, and social disruption in response to the ascendancy of progressive readings.',
    'If the epistemic violence is severe, the effective suppression and extractiveness for these communities are higher than structural measures suggest, potentially pushing the constraint closer to a Snare from their perspective, even if it is a Rope for beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_violence_impact, empirical, 'Measurement of the social and psychological costs of normative reversal on literalist communities.').

omega_variable(
    scholarly_exit_costs_quantification,
    'What are the quantifiable exit costs (career stagnation, loss of institutional affiliation, social ostracization) for scholars within traditional Islamic institutions who adopt and promote the progressive abrogation reading?',
    'Longitudinal studies tracking the career trajectories, institutional affiliations, and social standing of scholars who publicly endorse this reading versus those who maintain traditional interpretations.',
    'Higher quantifiable exit costs indicate stronger institutional suppression and identity-locking mechanisms, reinforcing the Tangled Rope classification and highlighting the coercive aspects of maintaining traditional authority structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scholarly_exit_costs_quantification, empirical, 'Quantification of professional and social costs for scholars adopting progressive interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1980, quranic_gender_verses__progressive_abrogation, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(qura_tr_t1990, quranic_gender_verses__progressive_abrogation, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(qura_tr_t2000, quranic_gender_verses__progressive_abrogation, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(qura_tr_t2010, quranic_gender_verses__progressive_abrogation, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(qura_tr_t2020, quranic_gender_verses__progressive_abrogation, theater_ratio, 2020, 0.09).
narrative_ontology:measurement(qura_tr_t2025, quranic_gender_verses__progressive_abrogation, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t1980, quranic_gender_verses__progressive_abrogation, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(qura_be_t1990, quranic_gender_verses__progressive_abrogation, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(qura_be_t2000, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(qura_be_t2010, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(qura_be_t2020, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2020, 0.88).
narrative_ontology:measurement(qura_be_t2025, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2025, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1980, quranic_gender_verses__progressive_abrogation, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(qura_su_t1990, quranic_gender_verses__progressive_abrogation, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(qura_su_t2000, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(qura_su_t2010, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(qura_su_t2020, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2020, 0.83).
narrative_ontology:measurement(qura_su_t2025, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
