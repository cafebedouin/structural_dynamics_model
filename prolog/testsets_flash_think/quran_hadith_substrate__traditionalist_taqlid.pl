% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__traditionalist_taqlid, []).

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
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Traditionalist Taqlid in Islamic Jurisprudence
 *   domain: islamic_jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   This constraint describes the traditionalist reading of Islamic
 *   jurisprudence, where classical fiqh schools represent authoritative
 *   consensus (ijma) and contemporary Muslims are obligated to follow
 *   established madhhab rulings via taqlid (emulation). This reading is one
 *   instantiation of the 'quran_hadith_substrate' kernel, which is contested
 *   by reformist and state-hybrid interpretations. The constraint is claimed
 *   as a 'tangled_rope' because it genuinely coordinates doctrinal unity but
 *   also extracts significantly from those who seek alternative
 *   interpretations, enforced by institutional and social pressure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.75).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.8).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.75).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Traditionalist Taqlid in Islamic Jurisprudence").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "islamic_jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, '05734ee2-4e36-4421-8f08-c4b2bffe1451').
narrative_ontology:cs_kernel_codification('05734ee2-4e36-4421-8f08-c4b2bffe1451', fixed_text).
narrative_ontology:cs_authority_grounding('05734ee2-4e36-4421-8f08-c4b2bffe1451', lineage).
narrative_ontology:cs_interpretation_layer_present('05734ee2-4e36-4421-8f08-c4b2bffe1451').
narrative_ontology:cs_reading_relation('05734ee2-4e36-4421-8f08-c4b2bffe1451', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_reading_relation('05734ee2-4e36-4421-8f08-c4b2bffe1451', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('05734ee2-4e36-4421-8f08-c4b2bffe1451', foundational, ijma_is_binding_legal_source).
narrative_ontology:cs_axiom_status(ijma_is_binding_legal_source, holdable).
narrative_ontology:cs_axiom_grounding('05734ee2-4e36-4421-8f08-c4b2bffe1451', ijma_is_binding_legal_source, conventional).
narrative_ontology:cs_axiom('05734ee2-4e36-4421-8f08-c4b2bffe1451', foundational, taqlid_is_obligatory_for_laypersons).
narrative_ontology:cs_axiom_status(taqlid_is_obligatory_for_laypersons, holdable).
narrative_ontology:cs_axiom_grounding('05734ee2-4e36-4421-8f08-c4b2bffe1451', taqlid_is_obligatory_for_laypersons, conventional).
narrative_ontology:cs_reference_frame('05734ee2-4e36-4421-8f08-c4b2bffe1451', classical_madhhab_consensus).
narrative_ontology:cs_drift_state('05734ee2-4e36-4421-8f08-c4b2bffe1451', contemporary_global_islam, gap(stable, minor, false)).
narrative_ontology:cs_created_at('05734ee2-4e36-4421-8f08-c4b2bffe1451', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, conservative_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equality).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, ijma_as_authoritative_source).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, madhhab_fidelity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The traditional religious scholars who interpret and transmit classical fiqh, defining the boundaries of acceptable legal thought and enforcing adherence to established madhhab rulings. Their authority and social capital are directly tied to the persistence of taqlid.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama, agenda_setter,
    institutional, generational, identity_locked, global).

% The established schools of Islamic law (madhahib) and the educational/judicial structures that perpetuate them. They benefit from the stability and legitimacy conferred by the obligation of taqlid, which ensures their interpretive framework remains dominant.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Individuals who find spiritual and social security in adhering to established legal traditions, viewing taqlid as a path to religious certainty and communal unity. They benefit from clear guidance and the avoidance of perceived innovation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, conservative_muslims, beneficiary,
    moderate, biographical, identity_locked, global).

% Individuals who seek to reinterpret Islamic texts in light of modern ethical concerns, human rights, or scientific advancements. They bear the cost of intellectual marginalization, social pressure, and potential accusations of heresy for deviating from established taqlid.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, global).

% Women who advocate for gender equality and justice within Islamic legal frameworks, often finding classical fiqh rulings to be patriarchal or discriminatory. They are particularly victimized by the rigidity of taqlid, which limits avenues for legal reform and equal status.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equality, payer,
    powerless, biographical, trapped, global).

% Non-Muslim communities living in traditionalist-dominant contexts, who may be subject to classical dhimmi frameworks or other legal limitations derived from fiqh rulings. They bear the cost of legal inequality and social marginalization.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Scholars who advocate for ijtihad (independent legal reasoning) and contextual interpretations of Islamic law. They are often excluded from mainstream religious institutions and discourse, their views dismissed as illegitimate deviations from consensus.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    organized, biographical, constrained, global).

% International and local organizations that monitor human rights compliance, including religious freedom and gender equality, in contexts governed by Islamic law. They analyze the impact of taqlid on individual rights and advocate for legal reforms.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain doctrinal consistency, legal predictability, and communal unity across diverse Muslim populations by standardizing legal interpretation and preventing fragmentation into endless individual opinions.
% TRANSFER_FUNCTION: Transfers interpretive authority, social capital, and institutional legitimacy to classical fiqh schools and their contemporary adherents (ulama, madhhab institutions), while transferring compliance costs, limitations on legal agency, and social pressure to individuals, especially those seeking reform or equality.
% ABSENT_VOICES: Reformist scholars, feminist theologians, and secular legal experts who advocate for contextualized interpretations or human rights-based legal reform are often marginalized, silenced, or actively excluded from traditionalist religious institutions and public discourse.
% DISAPPEARANCE_RATIONALE: If the obligation of taqlid and the authoritative status of classical fiqh schools vanished overnight, Islamic legal thought would fragment into diverse interpretations. This would lead to significant shifts in personal status laws, social norms, and the structure of religious authority globally, as individuals and communities would be compelled to engage in independent reasoning (ijtihad) or adopt new interpretive frameworks.
% FOUNDING_PROBLEM: To prevent fragmentation and innovation (bid'ah) in Islamic law after the formative period, ensuring stability, continuity, and orthodoxy of the tradition by establishing a clear methodology for legal adherence.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist scholars and institutions assert the problem of fragmentation and bid'ah is still live, citing the need to preserve orthodoxy. Reformist scholars, human rights advocates, and sociological studies of religious authority attest that the founding problem is substantially superseded by modern challenges, and the arrangement now primarily serves to maintain existing power structures and resist reform, rather than genuinely preventing chaos.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.75) because the obligation of taqlid limits individual legal agency and imposes adherence to rulings that may not align with contemporary needs or ethical frameworks, particularly for women and minorities. Suppression is also high (0.80) due to the institutionalized nature of religious authority, social pressure, and the marginalization of dissenting voices. The theater ratio is low (0.15) because the enforcement of taqlid is largely genuine and functional, not merely performative, though it faces increasing external challenges. Accessibility collapse is high (0.85) as alternatives to madhhab adherence are structurally and socially difficult to pursue. Resistance is moderate (0.60) reflecting ongoing efforts by progressive Muslims and human rights advocates to challenge the rigidity of taqlid.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of traditional ulama and conservative Muslims, this constraint is a necessary 'rope' for preserving religious orthodoxy and communal cohesion. From the perspective of progressive Muslims, women, and minorities, it operates as a 'snare' that limits their rights and agency under the guise of religious tradition. The engine's classification as 'tangled_rope' captures this dual function, acknowledging both the coordination and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional ulama and madhhab institutions are clear beneficiaries, as their authority and social capital are directly derived from this constraint. Conservative Muslims also benefit from the perceived stability and clarity. Progressive Muslims, women seeking equality, and religious minorities are victims, bearing the costs of limited legal agency, social pressure, and legal inequality. Reformist scholars are excluded, as their interpretive methodologies are deemed illegitimate by this framework. Human rights advocates act as observers, analyzing the constraint's impact.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent fragmentation and maintain doctrinal stability in Islamic law. While this function remains 'contested' (as per the six questions), the high and increasing extractiveness and suppression suggest that the constraint has drifted. It now serves significantly to maintain existing power structures and resist reform, rather than solely fulfilling its original coordination function. The persistence of the constraint, despite the 'contested' status of its founding problem, indicates a potential for mandatrophy, where the structure outlives its original justification and becomes primarily extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijma_authenticity_vs_institutionalization,
    'Is the ''ijma'' (consensus) claimed by classical fiqh schools a genuine, organic consensus of the Muslim community, or an institutionalized construct that serves to legitimize specific power structures?',
    'Historical-critical analysis of the formation of madhhabs and the evolution of ijma doctrine, alongside sociological studies of contemporary religious authority and dissent.',
    'If ijma is primarily an institutional construct, the constraint''s ''rope'' function is diminished, and its ''snare'' characteristics (extraction, suppression) are amplified, potentially reclassifying it closer to a pure snare. If it''s a genuine consensus, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_authenticity_vs_institutionalization, conceptual, 'Ambiguity regarding the nature of ''ijma'' as either organic consensus or institutionalized power.').

omega_variable(
    taqlid_structural_vs_internalized_suppression,
    'Is the suppression of independent legal reasoning (ijtihad) primarily structural (institutional barriers, social ostracism) or internalized (a belief among Muslims that they lack the capacity or right to interpret texts independently)?',
    'Post-exit suppression trajectory: if individuals continue to self-censor or feel incapable of ijtihad even after leaving traditionalist institutions, it suggests a significant internalized component. Comparative studies of Muslim communities with varying degrees of institutional control.',
    'If internalized suppression is dominant, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would make the constraint more resilient to external challenges and harder to dismantle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_structural_vs_internalized_suppression, empirical, 'Structural vs. internalized suppression mechanism for taqlid.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the Quran and Hadith substrate primarily a source of divine, immutable law requiring strict adherence to classical interpretations, or a historical and ethical text open to reinterpretation in light of evolving human understanding and context?',
    'Theological and philosophical discourse on hermeneutics and the nature of religious authority, alongside empirical observation of how different communities actually engage with sacred texts.',
    'If framed as immutable divine law, the traditionalist taqlid reading gains stronger legitimacy within its own framework, making challenges appear as deviations from divine will. If framed as an ethical text open to reinterpretation, the traditionalist reading''s claims to absolute authority weaken, potentially shifting its classification towards a more contested ''tangled_rope'' or even ''snare'' from an external perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the fundamental framing of the Quran and Hadith kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1000, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(qura_tr_t1200, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1200, 0.18).
narrative_ontology:measurement(qura_tr_t1400, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1400, 0.15).
narrative_ontology:measurement(qura_tr_t1600, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1600, 0.12).
narrative_ontology:measurement(qura_tr_t1800, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(qura_tr_t2024, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(qura_be_t1000, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1000, 0.4).
narrative_ontology:measurement(qura_be_t1200, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1200, 0.55).
narrative_ontology:measurement(qura_be_t1400, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1400, 0.65).
narrative_ontology:measurement(qura_be_t1600, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1600, 0.7).
narrative_ontology:measurement(qura_be_t1800, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1800, 0.72).
narrative_ontology:measurement(qura_be_t2024, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1000, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(qura_su_t1200, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1200, 0.6).
narrative_ontology:measurement(qura_su_t1400, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1400, 0.7).
narrative_ontology:measurement(qura_su_t1600, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1600, 0.75).
narrative_ontology:measurement(qura_su_t1800, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1800, 0.78).
narrative_ontology:measurement(qura_su_t2024, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_hadith_substrate' kernel. Each reading represents a distinct structural claim about how Islamic legal authority is constituted and applied, with differing ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
