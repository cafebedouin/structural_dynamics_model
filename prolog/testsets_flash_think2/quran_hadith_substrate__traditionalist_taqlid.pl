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
 *   human_readable: Traditionalist Taqlid (Islamic Jurisprudence)
 *   domain: religious/legal/social
 *
 * SUMMARY:
 *   This constraint describes the traditionalist reading of Islamic
 *   jurisprudence, where classical fiqh schools (madhhabs) are considered to
 *   represent authoritative consensus (ijma), and contemporary Muslims are
 *   obligated to follow their established rulings via taqlid (emulation).
 *   While presented as a mechanism for legal certainty and unity, this
 *   reading is increasingly contested for its high extraction from
 *   progressive voices and its role in maintaining the authority of
 *   traditional religious institutions. The constraint is claimed as a Rope
 *   by its beneficiaries, but its operational metrics reflect a Tangled Rope
 *   due to significant extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.7).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.8).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.7).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Traditionalist Taqlid (Islamic Jurisprudence)").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal/social").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, 'de4ada41-0a30-4d30-a30e-eba22034fd16').
narrative_ontology:cs_kernel_codification('de4ada41-0a30-4d30-a30e-eba22034fd16', fixed_text).
narrative_ontology:cs_authority_grounding('de4ada41-0a30-4d30-a30e-eba22034fd16', lineage).
narrative_ontology:cs_interpretation_layer_present('de4ada41-0a30-4d30-a30e-eba22034fd16').
narrative_ontology:cs_reading_relation('de4ada41-0a30-4d30-a30e-eba22034fd16', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_reading_relation('de4ada41-0a30-4d30-a30e-eba22034fd16', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('de4ada41-0a30-4d30-a30e-eba22034fd16', foundational, ijma_of_madhhabs_is_authoritative).
narrative_ontology:cs_axiom_status(ijma_of_madhhabs_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('de4ada41-0a30-4d30-a30e-eba22034fd16', ijma_of_madhhabs_is_authoritative, conventional).
narrative_ontology:cs_axiom('de4ada41-0a30-4d30-a30e-eba22034fd16', foundational, taqlid_is_obligatory_for_lay_muslims).
narrative_ontology:cs_axiom_status(taqlid_is_obligatory_for_lay_muslims, holdable).
narrative_ontology:cs_axiom_grounding('de4ada41-0a30-4d30-a30e-eba22034fd16', taqlid_is_obligatory_for_lay_muslims, conventional).
narrative_ontology:cs_reference_frame('de4ada41-0a30-4d30-a30e-eba22034fd16', classical_fiqh_methodology).
narrative_ontology:cs_drift_state('de4ada41-0a30-4d30-a30e-eba22034fd16', contemporary_globalized_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('de4ada41-0a30-4d30-a30e-eba22034fd16', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, conservative_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equality).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The traditional religious scholars who interpret and transmit classical fiqh rulings. They benefit from the authority and social capital derived from the obligation of taqlid, which positions them as indispensable intermediaries to divine law. They actively enforce adherence to madhhab rulings.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama, agenda_setter,
    institutional, generational, identity_locked, global).

% The established schools of Islamic law (madhhabs) and the institutions (mosques, seminaries, universities) that perpetuate their teachings. They receive financial support, prestige, and continued relevance through the adherence to taqlid.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Muslims who find spiritual and social stability in adhering to established madhhab rulings. They benefit from clear guidance and a sense of communal unity, avoiding the perceived chaos of individual interpretation. Their identity is often deeply tied to this tradition.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, conservative_muslims, beneficiary,
    moderate, biographical, identity_locked, global).

% Muslims who seek to reinterpret Islamic law in light of contemporary ethics, human rights, or public interest (maslaha). They bear the cost of being marginalized, labeled as innovators, or facing social pressure for departing from traditional rulings. Their exit options are constrained by social ties and the desire to remain within the Muslim community.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, global).

% Women who are often disadvantaged by classical fiqh rulings in areas like family law, inheritance, and testimony. They bear the direct costs of legal inequality and face significant barriers to challenging these norms due to the entrenched authority of traditional interpretations. Their options are severely limited.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equality, payer,
    powerless, biographical, trapped, global).

% Non-Muslims living in traditionalist-dominant contexts, who may be subject to classical dhimmi frameworks or other legal restrictions derived from fiqh. They bear the costs of unequal legal status and limited civil liberties, with virtually no exit options within the legal system.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Scholars who advocate for ijtihad (independent reasoning) and contextual interpretations. They are often excluded from mainstream religious institutions and discourse, despite their intellectual contributions, because their approach challenges the authority of taqlid. Their influence is limited by the constraint's enforcement mechanisms.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    powerful, biographical, constrained, global).

% Government bodies in Muslim-majority countries that may selectively incorporate or defer to classical fiqh rulings, particularly in personal status laws. They observe the social and political implications of taqlid and may face pressure to either uphold or reform these traditions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, state_legal_systems, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal certainty and unity within the Muslim community by establishing a clear methodology for deriving rulings from sacred texts, thereby preventing fragmentation and arbitrary interpretation.
% TRANSFER_FUNCTION: Transfers interpretive authority and social capital from individual Muslims and reformist scholars to established fiqh schools and their contemporary representatives (ulama), along with financial and social support for these institutions.
% ABSENT_VOICES: Reformist scholars, women's rights advocates, and religious minorities who advocate for contextualized interpretations or equal rights are often marginalized or excluded from traditional interpretive circles. Their perspectives are systematically underrepresented in the discourse that shapes the constraint.
% DISAPPEARANCE_RATIONALE: If the obligation of taqlid and the authority of classical fiqh schools vanished overnight, there would be a profound reordering of religious authority, legal practice, and social norms within Muslim communities globally. Individual interpretation (ijtihad) would become more prevalent, leading to diverse legal opinions and potentially challenging existing power structures and institutional beneficiaries.
% FOUNDING_PROBLEM: To prevent fragmentation and arbitrary interpretation of Islamic law after the era of the Prophet and his companions, ensuring consistency, scholarly rigor, and communal unity in legal rulings.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist ulama and madhhab institutions assert the problem is still live, citing the need for stability and protection against unqualified interpretations. Reformist scholars and progressive Muslim movements argue that the original problem of fragmentation has been substantially solved, and the arrangement now primarily serves to maintain power and resist necessary reforms; legislative-hearing testimony and independent academic analysis from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.70) is high because the constraint limits interpretive freedom and often perpetuates rulings that disadvantage certain groups (e.g., women, minorities), channeling authority and social capital to traditional institutions. Suppression (0.80) is very high, as alternative interpretations (ijtihad) are actively discouraged or condemned, and those who deviate face social, academic, and sometimes legal repercussions. Theater ratio (0.40) is moderate; while there is genuine belief in the system's divine grounding, a growing portion of its maintenance is performative defense of institutional power against reformist challenges. Accessibility collapse (0.75) is high because the institutional and social barriers to independent interpretation are substantial. Resistance (0.55) is moderate, reflecting ongoing, albeit often marginalized, reformist movements.
 *
 * PERSPECTIVAL GAP:
 *   The traditionalist ulama and madhhab institutions experience this constraint as a legitimate and necessary Rope, ensuring stability and fidelity to divine law. For progressive Muslims, women seeking equality, and religious minorities, the same structure operates as a Snare or highly extractive Tangled Rope, limiting their rights and voices. The engine's per-seat classification will reflect this divergence based on the declared power, exit options, and beneficiary/victim status of each stakeholder.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional ulama and madhhab institutions are clear beneficiaries (low d) as they gain authority and resources. Conservative Muslims also benefit from the stability and clear guidance (low d). Progressive Muslims, women, and religious minorities are targets (high d) as they bear the costs of limited interpretive freedom, legal inequality, and social marginalization. Reformist scholars are excluded, their alternative interpretations suppressed, placing them at the target end of the spectrum.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijma_nature_ambiguity,
    'Is ''ijma'' (consensus) a historical fact (consensus of early scholars) or an ongoing, dynamic process that can be re-established in each era?',
    'Theological and jurisprudential debate, potentially leading to a shift in accepted methodology within influential scholarly circles. Empirical observation of how ''consensus'' is invoked and challenged in contemporary legal discourse.',
    'If ijma is dynamic, the constraint''s suppression of contemporary ijtihad is less justified, potentially lowering extractiveness and suppression. If purely historical, the constraint''s claim to fidelity is strengthened, but its relevance to new issues is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_nature_ambiguity, conceptual, 'Ambiguity regarding the nature and scope of authoritative consensus (ijma).').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative interpretations primarily structural (institutional barriers, social pressure) or internalized (self-censorship, identity fusion with tradition)?',
    'Post-exit suppression trajectory: if individuals continue to self-censor or adhere to traditional rulings even after leaving traditionalist-dominant social contexts, it suggests a significant internalized component. Sociological studies on identity formation within traditionalist communities.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would make exit options like ''constrained'' or ''mobile'' less effective in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for interpretive freedom.').

omega_variable(
    mandate_vs_power_maintenance,
    'To what extent does the constraint genuinely solve the founding problem of legal fragmentation, versus serving as a mechanism for maintaining the institutional power and social relevance of traditional ulama and madhhabs?',
    'Comparative analysis of legal systems that have adopted more flexible interpretive methodologies: if they achieve legal coherence without the same level of institutionalized taqlid, it suggests the power-maintenance function is dominant. Historical analysis of how the constraint adapted (or failed to adapt) to new challenges.',
    'If primarily power maintenance, the constraint''s extractiveness is more clearly unjustified, and its classification shifts closer to a pure Snare. If the coordination function remains genuinely critical, the Tangled Rope classification is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_vs_power_maintenance, empirical, 'Distinguishing genuine coordination from power maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1000, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(qura_tr_t1200, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1200, 0.15).
narrative_ontology:measurement(qura_tr_t1400, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1400, 0.2).
narrative_ontology:measurement(qura_tr_t1600, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1600, 0.25).
narrative_ontology:measurement(qura_tr_t1800, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(qura_tr_t2024, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(qura_be_t1000, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1000, 0.45).
narrative_ontology:measurement(qura_be_t1200, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1200, 0.5).
narrative_ontology:measurement(qura_be_t1400, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1400, 0.55).
narrative_ontology:measurement(qura_be_t1600, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1600, 0.6).
narrative_ontology:measurement(qura_be_t1800, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement(qura_be_t2024, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1000, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(qura_su_t1200, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1200, 0.58).
narrative_ontology:measurement(qura_su_t1400, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1400, 0.65).
narrative_ontology:measurement(qura_su_t1600, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(qura_su_t1800, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(qura_su_t2024, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
