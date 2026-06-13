% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Rationalist Reading)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   This constraint represents the theological position that the Qur'an is
 *   created divine speech (makhlūq), a view primarily associated with
 *   rationalist schools like the Mu'tazila. This reading emphasizes God's
 *   absolute transcendence, arguing that an eternal, uncreated Qur'an would
 *   imply a co-eternal entity with God, compromising monotheism. It allows
 *   for greater interpretive flexibility and the integration of reason into
 *   theological understanding. This is one reading of the
 *   'quran_ontological_status' kernel, distinct from the 'uncreated' and
 *   'state_enforced_creation' readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.25).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.15).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Rationalist Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "islamic_theology/philosophy_of_language/political_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, 'd6cddef7-35ae-4650-9613-2b0a8056af32').
narrative_ontology:cs_kernel_codification('d6cddef7-35ae-4650-9613-2b0a8056af32', formalized).
narrative_ontology:cs_authority_grounding('d6cddef7-35ae-4650-9613-2b0a8056af32', expertise).
narrative_ontology:cs_interpretation_layer_present('d6cddef7-35ae-4650-9613-2b0a8056af32').
narrative_ontology:cs_reading_relation('d6cddef7-35ae-4650-9613-2b0a8056af32', quran_ontological_status__uncreated_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6cddef7-35ae-4650-9613-2b0a8056af32', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('d6cddef7-35ae-4650-9613-2b0a8056af32', foundational, divine_transcendence_absolute).
narrative_ontology:cs_axiom_status(divine_transcendence_absolute, holdable).
narrative_ontology:cs_axiom_grounding('d6cddef7-35ae-4650-9613-2b0a8056af32', divine_transcendence_absolute, deontological).
narrative_ontology:cs_axiom('d6cddef7-35ae-4650-9613-2b0a8056af32', foundational, reason_as_interpretive_tool).
narrative_ontology:cs_axiom_status(reason_as_interpretive_tool, holdable).
narrative_ontology:cs_axiom_grounding('d6cddef7-35ae-4650-9613-2b0a8056af32', reason_as_interpretive_tool, conventional).
narrative_ontology:cs_reference_frame('d6cddef7-35ae-4650-9613-2b0a8056af32', rational_theological_inquiry).
narrative_ontology:cs_drift_state('d6cddef7-35ae-4650-9613-2b0a8056af32', contemporary_islamic_thought, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d6cddef7-35ae-4650-9613-2b0a8056af32', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain hermeneutic flexibility and intellectual authority by asserting the Qur'an is a created artifact, allowing for allegorical interpretation and reconciliation with reason. This position elevates their interpretive role.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    organized, generational, mobile, global).

% Benefit from the flexibility to re-interpret scripture in light of modern challenges, moving away from rigid literalism. This reading provides a theological basis for progressive social and political reforms.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    moderate, generational, mobile, global).

% Find a theological framework compatible with philosophical inquiry, as it removes the constraint of an eternal, uncreated text that might limit rational thought or scientific discovery. Their intellectual pursuits are less constrained.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, generational, mobile, global).

% Experience a challenge to their authority, which often derives from a literalist and fixed interpretation of an uncreated Qur'an. This reading diminishes the perceived immutability of their legal rulings and hermeneutic methods.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    powerful, generational, constrained, global).

% Their identity and worldview are deeply tied to the belief in the Qur'an as unmediated, eternal divine speech. The 'created' reading can be perceived as undermining the sacredness and direct authority of the text, causing existential discomfort and requiring a re-evaluation of core beliefs.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    moderate, biographical, identity_locked, local).

% Observe the theological debate, potentially leveraging the 'created' reading to support state authority over religious institutions or to promote a more flexible interpretation of religious law in civil society. They do not directly participate in the theological argument but can be influenced by its outcome.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, secular_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates theological discourse by establishing a framework where divine transcendence is preserved, and revelation can be interpreted through reason, allowing for intellectual flexibility and adaptation to new knowledge without compromising monotheistic principles.
% TRANSFER_FUNCTION: Transfers hermeneutic authority from rigid textual literalism to rational theological inquiry, enabling philosophical and reformist interpretations to gain legitimacy within Islamic thought.
% ABSENT_VOICES: Extremist literalist factions, who would reject any interpretation that diminishes the absolute, unmediated authority of the Qur'an, are largely excluded from mainstream theological discourse that engages with this reading. Their voices are suppressed by the intellectual consensus of rationalist schools.
% DISAPPEARANCE_RATIONALE: If the 'created' reading vanished, rationalist theology would lose a foundational premise, leading to a resurgence of literalism and potentially hindering reform efforts. The intellectual landscape of Islamic thought would become more rigid, and the relationship between faith and reason would be fundamentally altered.
% FOUNDING_PROBLEM: To reconcile the absolute transcendence of God with the temporal nature of revelation, and to provide a theological basis for rational inquiry and allegorical interpretation of scripture, avoiding anthropomorphism and rigid literalism.
% FOUNDING_PROBLEM_CORROBORATION: Philosophical schools and contemporary reform movements continue to attest to the live nature of this problem, seeking to integrate modern thought with Islamic tradition. Historians of Islamic thought also corroborate the historical necessity of this theological development to address philosophical challenges to divine unity and transcendence.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).
:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is relatively low because this reading primarily facilitates intellectual coordination and hermeneutic flexibility, rather than directly extracting material resources. Suppression (0.15) is also low, as this reading generally relies on intellectual persuasion and scholarly debate, not coercive enforcement, to gain adherence. Theater ratio is minimal (0.05) as the theological arguments are direct and functional. The metrics reflect a rope-like function, coordinating intellectual activity and providing a framework for rationalist thought.
 *
 * PERSPECTIVAL GAP:
 *   Rationalist theologians and reform movements perceive this as a liberating and intellectually coherent framework, enhancing their ability to engage with scripture and reason. Traditionalist jurists and literalist communities, however, experience it as a challenge to established authority and a threat to the perceived immutability of divine revelation, leading to a sense of loss of certainty and interpretive control.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians, reform movements, and philosophical schools are beneficiaries, as this reading provides a theological foundation for their intellectual and social agendas. Traditionalist jurists and literalist communities are victims, as their authority and identity are challenged by the interpretive flexibility this reading introduces. Secular authorities are observers, as they can leverage the intellectual outcomes without direct theological participation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as extraction by highlighting its primary function: to coordinate theological understanding in a way that preserves divine transcendence and allows for rational inquiry. While it challenges existing authorities (traditionalist jurists), this is a consequence of a genuine coordination problem (reconciling revelation with reason), not a primary extractive mechanism. The 'victims' are those whose authority is tied to an alternative, more rigid coordination mechanism, not those from whom resources are directly extracted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_enforcement_ambiguity,
    'To what extent did the ''created'' reading, at certain historical junctures, become a tool for state-enforced orthodoxy (e.g., the Mihna), thereby shifting its nature from a ''rope'' to a ''snare''?',
    'Historical analysis of state decrees, judicial records, and theological persecutions during periods like the Mihna, focusing on the coercive mechanisms employed and the beneficiaries of such enforcement.',
    'If state enforcement was a primary driver of its adoption, the constraint''s classification would shift towards ''tangled_rope'' or ''snare'' during those periods, indicating a hybrid coordination/extraction or pure extraction function, respectively. This would highlight the political instrumentalization of a theological position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_enforcement_ambiguity, empirical, 'Ambiguity regarding the historical enforcement of the ''created'' reading and its potential shift to a coercive constraint.').

omega_variable(
    interpretive_authority_concentration,
    'Does the interpretive flexibility afforded by the ''created'' reading lead to an undue concentration of hermeneutic authority in the hands of a select intellectual elite, effectively creating a new form of extraction?',
    'Sociological study of theological institutions and discourse, examining access to interpretive training, publication patterns, and the influence of specific schools of thought on broader religious understanding. Analyze whether the ''flexibility'' is genuinely distributed or controlled by a few.',
    'If interpretive authority becomes highly concentrated and used to marginalize dissenting voices or enforce specific social agendas, the constraint''s extractiveness would be higher, potentially pushing it towards a ''tangled_rope'' classification, as the coordination function (rational interpretation) would be coupled with asymmetric power dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_concentration, empirical, 'Whether interpretive flexibility leads to concentrated authority and new forms of extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 750, 1250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(qura_be_t750, quran_ontological_status__created_reading, base_extractiveness, 750, 0.2).
narrative_ontology:measurement(qura_be_t850, quran_ontological_status__created_reading, base_extractiveness, 850, 0.22).
narrative_ontology:measurement(qura_be_t950, quran_ontological_status__created_reading, base_extractiveness, 950, 0.25).
narrative_ontology:measurement(qura_be_t1050, quran_ontological_status__created_reading, base_extractiveness, 1050, 0.23).
narrative_ontology:measurement(qura_be_t1150, quran_ontological_status__created_reading, base_extractiveness, 1150, 0.24).
narrative_ontology:measurement(qura_be_t1250, quran_ontological_status__created_reading, base_extractiveness, 1250, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t750, quran_ontological_status__created_reading, suppression_requirement, 750, 0.1).
narrative_ontology:measurement(qura_su_t850, quran_ontological_status__created_reading, suppression_requirement, 850, 0.12).
narrative_ontology:measurement(qura_su_t950, quran_ontological_status__created_reading, suppression_requirement, 950, 0.15).
narrative_ontology:measurement(qura_su_t1050, quran_ontological_status__created_reading, suppression_requirement, 1050, 0.13).
narrative_ontology:measurement(qura_su_t1150, quran_ontological_status__created_reading, suppression_requirement, 1150, 0.14).
narrative_ontology:measurement(qura_su_t1250, quran_ontological_status__created_reading, suppression_requirement, 1250, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_ontological_status' kernel. It posits the Qur'an as created, contrasting with the 'uncreated_reading' which sees it as eternal, and the 'state_enforced_creation_reading' which adds state coercion to the created doctrine. The ε values differ significantly due to the presence of coercion and the degree of interpretive flexibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
