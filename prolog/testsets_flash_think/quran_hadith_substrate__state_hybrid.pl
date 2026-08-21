% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State Hybrid Application of Islamic Law
 *   domain: islamic_jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   This constraint describes the 'state_hybrid' reading of the
 *   'quran_hadith_substrate' kernel, where a state selectively applies
 *   classical Islamic legal rulings in certain domains (e.g., family,
 *   criminal law) while adopting reformist or secular frameworks in others
 *   (e.g., commercial, administrative law). The state's legitimacy is
 *   grounded in political sovereignty and its ability to manage these
 *   tensions, rather than pure doctrinal fidelity. This approach functions as
 *   a tangled rope, providing a degree of coordination and stability for the
 *   state and some actors, but extracting costs from those whose
 *   comprehensive legal visions are truncated or suppressed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.35).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.65).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.35).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Hybrid Application of Islamic Law").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "islamic_jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'b92a4fbf-554a-4c04-9744-90b99c466471').
narrative_ontology:cs_kernel_codification('b92a4fbf-554a-4c04-9744-90b99c466471', formalized).
narrative_ontology:cs_authority_grounding('b92a4fbf-554a-4c04-9744-90b99c466471', extraction).
narrative_ontology:cs_interpretation_layer_present('b92a4fbf-554a-4c04-9744-90b99c466471').
narrative_ontology:cs_reading_relation('b92a4fbf-554a-4c04-9744-90b99c466471', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('b92a4fbf-554a-4c04-9744-90b99c466471', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('b92a4fbf-554a-4c04-9744-90b99c466471', foundational, state_sovereignty_over_legal_interpretation).
narrative_ontology:cs_axiom_status(state_sovereignty_over_legal_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('b92a4fbf-554a-4c04-9744-90b99c466471', state_sovereignty_over_legal_interpretation, conventional).
narrative_ontology:cs_axiom('b92a4fbf-554a-4c04-9744-90b99c466471', secondary, selective_application_for_public_interest).
narrative_ontology:cs_axiom_status(selective_application_for_public_interest, holdable).
narrative_ontology:cs_axiom_grounding('b92a4fbf-554a-4c04-9744-90b99c466471', selective_application_for_public_interest, instrumental).
narrative_ontology:cs_reference_frame('b92a4fbf-554a-4c04-9744-90b99c466471', political_sovereignty_over_doctrine).
narrative_ontology:cs_drift_state('b92a4fbf-554a-4c04-9744-90b99c466471', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b92a4fbf-554a-4c04-9744-90b99c466471', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, secular_commercial_actors).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, general_populace).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors selectively adopt classical Islamic legal rulings in areas like family law and criminal codes to bolster religious legitimacy, while applying reformist or secular frameworks in commercial and administrative law to maintain economic flexibility and international integration. They benefit from a stable, adaptable legal system that serves their political and economic interests.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% These entities benefit from the state's application of secular or reformist frameworks in commercial law, which provides predictability, aligns with international standards, and facilitates economic growth. They are largely unburdened by traditional religious legal constraints in their operations.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, secular_commercial_actors, beneficiary,
    powerful, biographical, mobile, national).

% These scholars advocate for the comprehensive application of classical Islamic legal schools (madhhabs) and adherence to taqlid (emulation of established rulings). They bear the cost of the state's selective approach, which truncates their vision of a fully sharia-compliant society and marginalizes their interpretive authority.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    organized, generational, constrained, national).

% These activists seek to re-interpret Islamic law (ijtihad) in light of contemporary ethics, human rights, and public interest (maslaha), prioritizing the Quran's ethical trajectory. They are constrained by the state's selective adoption of classical rulings, which limits the scope for critical reform and can suppress their advocacy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_activists, payer,
    moderate, biographical, constrained, national).

% The general populace benefits from the perceived stability and religious legitimacy offered by the state's hybrid legal system, which often addresses personal status issues (family law) in a way that resonates with cultural and religious norms, while also providing a functional modern economy. They indirectly pay through limited legal alternatives.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, general_populace, beneficiary,
    moderate, biographical, constrained, national).

% These bodies observe and critique the state's legal practices, particularly where classical rulings (e.g., in criminal law) may conflict with international human rights standards. Their analysis influences international perception and diplomatic pressure, but they have no direct enforcement power over the state's internal legal system.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable legal framework that balances traditional religious legitimacy with the demands of modern state governance, economic development, and international relations, preventing internal legal fragmentation or external isolation.
% TRANSFER_FUNCTION: Transfers ultimate legal interpretive authority and policy flexibility from comprehensive doctrinal schools to the state, while extracting compliance and truncated legal visions from both traditionalist and reformist groups. It also transfers social stability and religious legitimation to the state from the populace.
% ABSENT_VOICES: Advocates for a fully comprehensive, traditionalist sharia and those pushing for a fully reformed, ethical sharia are structurally marginalized in the state's legal discourse. Their comprehensive visions are not fully integrated, and their critiques are often suppressed to maintain the state's chosen hybridity.
% DISAPPEARANCE_RATIONALE: If this state-led hybrid legal system vanished overnight, the state would face a profound legitimacy crisis, potentially leading to widespread legal uncertainty, social unrest, and a power struggle between competing traditionalist and reformist factions over the very foundation and application of law. The existing legal order would collapse.
% FOUNDING_PROBLEM: How to reconcile the historical and religious authority of Islamic legal heritage with the practical demands of modern statehood, economic development, and international legal norms, while maintaining political stability and legitimacy in a diverse society.
% FOUNDING_PROBLEM_CORROBORATION: State legal scholars, political analysts, and even critics from both traditionalist and reformist camps attest to the ongoing challenge of balancing these competing demands. Legislative debates, academic discourse, and public opinion surveys corroborate the persistence of this foundational tension, though they dispute the state's specific solutions.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) due to the state's instrumentalization of religious law for political and economic flexibility, which comes at the cost of doctrinal consistency and the full realization of either traditionalist or reformist legal ideals. Suppression is higher (0.65) because the state actively enforces its selective application, marginalizing alternative comprehensive legal interpretations. The theater ratio is moderate (0.4) as the state performs adherence to sharia to maintain legitimacy, even while its application is pragmatic and selective. Accessibility collapse is moderate (0.5) as alternative legal frameworks or comprehensive sharia applications are not entirely eliminated but are significantly constrained by state power. Resistance is moderate (0.45) from both traditionalist and reformist camps who challenge the state's hybridity.
 *
 * PERSPECTIVAL GAP:
 *   State elites perceive this constraint as a necessary and legitimate act of governance, balancing diverse needs and maintaining stability. Traditionalist and reformist scholars, however, experience it as an extractive truncation of their respective legal visions, where the state's political sovereignty overrides doctrinal fidelity or ethical reform. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites are clear beneficiaries, gaining legitimacy and flexibility. Secular commercial actors also benefit from predictable, modern commercial law. The general populace benefits from stability but is constrained in legal choice. Traditionalist and reformist scholars are victims, as their comprehensive legal frameworks are selectively applied or suppressed. International human rights bodies act as analytical observers, critiquing the system from an external perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumentalization_vs_adaptation,
    'To what extent is the state''s selective application of Islamic law a genuine adaptation to modern challenges, versus an instrumentalization of religious authority for political control and economic gain?',
    'Comparative legal analysis across different state contexts, examining the consistency of application, the transparency of legal reforms, and the degree of public participation in legal development, particularly in areas where religious and secular laws intersect.',
    'If primarily instrumentalization, the constraint''s extractiveness and suppression are higher, indicating a more snare-like function. If genuine adaptation, the coordination function is stronger, aligning more with a tangled rope or even a rope, with lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalization_vs_adaptation, conceptual, 'Distinguishing genuine legal adaptation from political instrumentalization of religious law.').

omega_variable(
    doctrinal_fidelity_impact,
    'How does the state''s hybrid legal approach impact the long-term doctrinal fidelity and internal coherence of Islamic jurisprudence within its borders?',
    'Longitudinal study of legal scholarship, fatwa issuance, and judicial rulings over several decades, assessing shifts in interpretive methodologies, the authority of classical texts, and the emergence of new legal schools or syntheses.',
    'If it leads to significant doctrinal fragmentation or erosion of traditional interpretive methods without a coherent alternative, the constraint''s long-term costs to religious intellectual heritage are higher. If it fosters a new, coherent, and widely accepted synthesis, the long-term coordination benefits are greater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_fidelity_impact, empirical, 'Assessing the long-term impact on Islamic doctrinal coherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.35).
narrative_ontology:measurement(qura_tr_t6, quran_hadith_substrate__state_hybrid, theater_ratio, 6, 0.37).
narrative_ontology:measurement(qura_tr_t12, quran_hadith_substrate__state_hybrid, theater_ratio, 12, 0.38).
narrative_ontology:measurement(qura_tr_t18, quran_hadith_substrate__state_hybrid, theater_ratio, 18, 0.39).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__state_hybrid, theater_ratio, 24, 0.4).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__state_hybrid, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qura_be_t6, quran_hadith_substrate__state_hybrid, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(qura_be_t12, quran_hadith_substrate__state_hybrid, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(qura_be_t18, quran_hadith_substrate__state_hybrid, base_extractiveness, 18, 0.34).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__state_hybrid, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__state_hybrid, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qura_su_t6, quran_hadith_substrate__state_hybrid, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(qura_su_t12, quran_hadith_substrate__state_hybrid, suppression_requirement, 12, 0.61).
narrative_ontology:measurement(qura_su_t18, quran_hadith_substrate__state_hybrid, suppression_requirement, 18, 0.63).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__state_hybrid, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__state_hybrid, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
