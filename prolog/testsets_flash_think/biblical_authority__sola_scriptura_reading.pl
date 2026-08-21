% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture as Sole and Self-Interpreting Authority
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint instantiates the 'Sola Scriptura' reading of biblical
 *   authority, a foundational principle of the Protestant Reformation. It
 *   asserts that the Bible alone is the sufficient and self-interpreting
 *   source of authority for Christian doctrine and practice, rejecting the
 *   need for an external interpretive tradition or magisterium. This reading
 *   emphasizes individual access to scripture and congregational autonomy,
 *   leading to low clerical extraction but also contributing to doctrinal
 *   fragmentation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.15).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.1).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture as Sole and Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies/history_of_christianity").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, 'da925c5c-92aa-48e4-84f5-3ab5f8df7d0a').
narrative_ontology:cs_kernel_codification('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', fixed_text).
narrative_ontology:cs_authority_grounding('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', self_enforcing).
narrative_ontology:cs_reading_relation('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', biblical_authority__conciliar_reading, forecloses).
narrative_ontology:cs_axiom('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', foundational, scripture_is_perspicuous).
narrative_ontology:cs_axiom_status(scripture_is_perspicuous, holdable).
narrative_ontology:cs_axiom_grounding('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', scripture_is_perspicuous, theological).
narrative_ontology:cs_axiom('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', foundational, scripture_is_sufficient).
narrative_ontology:cs_axiom_status(scripture_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', scripture_is_sufficient, theological).
narrative_ontology:cs_reference_frame('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', reformation_era_clarity).
narrative_ontology:cs_drift_state('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', contemporary_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da925c5c-92aa-48e4-84f5-3ab5f8df7d0a', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, individual_congregations).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, denominational_leaders).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, perspicuity_of_scripture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Empowered to read and interpret scripture directly without clerical mediation, fostering personal faith and autonomy. Benefits from direct access and low interpretive overhead.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, mobile, global).

% Maintain autonomy in doctrine and practice, deriving authority directly from their interpretation of scripture. Benefits from freedom from external hierarchical control.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, individual_congregations, beneficiary,
    organized, biographical, mobile, local).

% Provide interpretive tools, historical context, and systematic theology, influencing how scripture is understood. While not holding ultimate authority, their work shapes congregational and individual interpretation.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, theologians_scholars, agenda_setter,
    powerful, generational, constrained, global).

% Struggle to maintain doctrinal coherence and unity across diverse congregations, as the principle of Sola Scriptura often leads to varied interpretations and theological fragmentation. Bears the cost of managing internal disputes.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, denominational_leaders, payer,
    organized, biographical, constrained, national).

% Their claims of interpretive authority residing in tradition, magisterium, or ecumenical councils are rejected by this reading. They are structurally excluded from the interpretive framework.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, magisterial_churches, excluded,
    institutional, civilizational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__sola_scriptura_reading, lay_believers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, accessible textual foundation for Christian faith and practice, enabling individual and congregational autonomy by decentralizing interpretive authority.
% TRANSFER_FUNCTION: Transfers interpretive authority from clerical hierarchies to individual believers and local communities; transfers the burden of doctrinal coherence from a central authority to decentralized consensus.
% ABSENT_VOICES: Magisterial churches and their adherents are structurally excluded from the interpretive framework; they would argue for the necessity of an interpretive tradition or magisterium to maintain doctrinal unity and guard against error.
% DISAPPEARANCE_RATIONALE: If Sola Scriptura vanished, the landscape of Protestant Christianity would fundamentally change. Denominations would either collapse into pure congregationalism or seek new, extra-biblical sources of authority, leading to a major theological and institutional reorganization.
% FOUNDING_PROBLEM: The perceived corruption and unbiblical doctrines of the medieval church, and the desire for direct access to God's word without clerical mediation or the perceived distortions of tradition.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Reformation, independent theological scholars, and sociological studies of religious movements corroborate the historical problem and its ongoing relevance for many Protestant traditions, who continue to see extra-biblical authority as a threat.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the principle decentralizes interpretive authority, preventing a single entity from extracting rents through exclusive access or interpretation. Suppression is low (0.10) as it encourages individual interpretation and rejects coercive enforcement of external doctrinal decrees. Theater ratio is low (0.05) because the emphasis is on direct engagement with the text, minimizing performative rituals or elaborate interpretive structures. Accessibility collapse is moderate (0.40) as literacy and study are still required, but no clerical gate exists. Resistance is low (0.05) as this is a core, widely accepted principle within its own tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Sola Scriptura adherents, the constraint is a pure Rope, coordinating around a clear, accessible text and liberating believers. From the perspective of excluded magisterial churches, it is a destructive force leading to chaos and heresy. The engine's classification will reflect the internal structure of the Sola Scriptura reading, not the external critique.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and individual congregations are the primary beneficiaries, gaining direct access to religious authority and autonomy. Theologians and scholars act as agenda-setters, influencing interpretation without holding ultimate authority. Denominational leaders are 'payers' in the sense that they bear the cost of managing the doctrinal diversity and fragmentation that can arise from decentralized interpretation. Magisterial churches are excluded, as their foundational claims are rejected by this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The Sola Scriptura principle inherently guards against mandatrophy by preventing the accumulation of power and extraction by an interpretive elite. Its mandate is to keep authority decentralized and accessible. However, it introduces a different challenge: managing the consequences of radical decentralization, such as doctrinal fragmentation, which is a cost borne by those attempting to maintain broader coherence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_interpreting_ambiguity,
    'Is Scripture truly ''self-interpreting'' in practice, or does it inevitably require interpretive frameworks (e.g., historical context, theological presuppositions, community norms) that are not themselves Scripture?',
    'Empirical study of interpretive divergence across Sola Scriptura traditions: if significant, persistent divergence exists on core doctrines, it suggests the ''self-interpreting'' claim is aspirational rather than descriptive.',
    'If not truly self-interpreting, the constraint''s effective suppression might be higher than measured, as informal interpretive authorities (theologians, popular preachers) implicitly guide interpretation, creating a de facto interpretive layer. This would shift the classification closer to a Tangled Rope if such informal authority becomes extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_interpreting_ambiguity, empirical, 'Ambiguity of ''self-interpreting'' claim in practice.').

omega_variable(
    doctrinal_fragmentation_cost,
    'At what point does the doctrinal fragmentation resulting from decentralized interpretation become a net cost that outweighs the benefits of individual autonomy, from a systemic perspective?',
    'Preference-based: requires a normative judgment on the value of doctrinal unity versus individual interpretive freedom. This is a policy choice, not an empirical resolution.',
    'If fragmentation is judged a severe systemic cost, the ''payer'' role of denominational leaders would be amplified, and the overall classification might lean towards a Tangled Rope due to the unmanaged negative externality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_fragmentation_cost, preference, 'Normative evaluation of doctrinal fragmentation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 1517, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1517, biblical_authority__sola_scriptura_reading, theater_ratio, 1517, 0.03).
narrative_ontology:measurement(bibl_tr_t1650, biblical_authority__sola_scriptura_reading, theater_ratio, 1650, 0.04).
narrative_ontology:measurement(bibl_tr_t1800, biblical_authority__sola_scriptura_reading, theater_ratio, 1800, 0.04).
narrative_ontology:measurement(bibl_tr_t1900, biblical_authority__sola_scriptura_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(bibl_tr_t2024, biblical_authority__sola_scriptura_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1517, biblical_authority__sola_scriptura_reading, base_extractiveness, 1517, 0.1).
narrative_ontology:measurement(bibl_be_t1650, biblical_authority__sola_scriptura_reading, base_extractiveness, 1650, 0.12).
narrative_ontology:measurement(bibl_be_t1800, biblical_authority__sola_scriptura_reading, base_extractiveness, 1800, 0.13).
narrative_ontology:measurement(bibl_be_t1900, biblical_authority__sola_scriptura_reading, base_extractiveness, 1900, 0.14).
narrative_ontology:measurement(bibl_be_t2024, biblical_authority__sola_scriptura_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1517, biblical_authority__sola_scriptura_reading, suppression_requirement, 1517, 0.08).
narrative_ontology:measurement(bibl_su_t1650, biblical_authority__sola_scriptura_reading, suppression_requirement, 1650, 0.09).
narrative_ontology:measurement(bibl_su_t1800, biblical_authority__sola_scriptura_reading, suppression_requirement, 1800, 0.09).
narrative_ontology:measurement(bibl_su_t1900, biblical_authority__sola_scriptura_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(bibl_su_t2024, biblical_authority__sola_scriptura_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, information_standard).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, doctrinal_fragmentation_in_protestantism).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, congregational_autonomy_norms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
