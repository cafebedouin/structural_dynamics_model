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
 *   This constraint represents the 'progressive abrogation' reading of
 *   Qur'anic gender verses. It posits that later, universal egalitarian
 *   principles in the Qur'an (e.g., 49:13 on universal human dignity)
 *   supersede earlier, gender-specific rules (e.g., 4:11, 2:282, 4:34) via
 *   the principle of naskh (abrogation). This reading is highly extractive
 *   from traditional authority structures and scholars, as it demands a
 *   complete normative reversal and grants women full legal parity. It
 *   carries a high risk of epistemic violence for communities whose identity
 *   is bound to literal readings, and high exit costs for scholars adopting
 *   it within traditional institutions. The claimed type is 'snare' because
 *   its persistence depends on actively suppressing traditional
 *   interpretations and enforcing a new hermeneutical hierarchy, with clear
 *   victims in the traditional establishment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.95).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.88).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.95).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, snare).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Progressive Abrogation of Gender-Specific Qur'anic Verses").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, 'a74747f4-701e-4882-b991-a922e8e3bd19').
narrative_ontology:cs_kernel_codification('a74747f4-701e-4882-b991-a922e8e3bd19', fixed_text).
narrative_ontology:cs_authority_grounding('a74747f4-701e-4882-b991-a922e8e3bd19', lineage).
narrative_ontology:cs_interpretation_layer_present('a74747f4-701e-4882-b991-a922e8e3bd19').
narrative_ontology:cs_reading_relation('a74747f4-701e-4882-b991-a922e8e3bd19', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('a74747f4-701e-4882-b991-a922e8e3bd19', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('a74747f4-701e-4882-b991-a922e8e3bd19', foundational, universal_human_dignity_supersedes_specific_rules).
narrative_ontology:cs_axiom_status(universal_human_dignity_supersedes_specific_rules, holdable).
narrative_ontology:cs_axiom_grounding('a74747f4-701e-4882-b991-a922e8e3bd19', universal_human_dignity_supersedes_specific_rules, deontological).
narrative_ontology:cs_axiom('a74747f4-701e-4882-b991-a922e8e3bd19', foundational, naskh_applies_to_ethical_legal_verses).
narrative_ontology:cs_axiom_status(naskh_applies_to_ethical_legal_verses, holdable).
narrative_ontology:cs_axiom_grounding('a74747f4-701e-4882-b991-a922e8e3bd19', naskh_applies_to_ethical_legal_verses, conventional).
narrative_ontology:cs_reference_frame('a74747f4-701e-4882-b991-a922e8e3bd19', quranic_egalitarian_trajectory).
narrative_ontology:cs_drift_state('a74747f4-701e-4882-b991-a922e8e3bd19', contemporary_islamic_feminist_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('a74747f4-701e-4882-b991-a922e8e3bd19', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, muslim_women_seeking_parity).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_islamic_institutions).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, scholars_adhering_to_literal_readings).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_readings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for this reading, gaining intellectual and moral authority within reformist circles. They face significant backlash and potential marginalization from traditional institutions but find new platforms and audiences. This reading vindicates their reformist agenda.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars, beneficiary,
    organized, generational, constrained, global).

% Benefit from the legal and social implications of this reading, which grants them full legal parity and challenges patriarchal interpretations. Their identity is often deeply intertwined with their faith, making 'exit' from Islam unthinkable, so this reading offers a path to justice within their religious framework.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, muslim_women_seeking_parity, beneficiary,
    powerless, biographical, identity_locked, global).

% Experience a comprehensive delegitimization of their authority structures, which are often built upon literal interpretations of gender-specific verses. Adopting this reading would require a fundamental re-evaluation of centuries of jurisprudence, threatening their institutional identity and power. They actively resist this interpretation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_islamic_institutions, payer,
    institutional, civilizational, trapped, global).

% Bear the cost of having their scholarly work and interpretations challenged as outdated or unjust. Their careers and intellectual identities are often bound to traditional hermeneutical methods that prioritize literal readings and historical consensus. Adopting this reading would mean abandoning their established academic and religious positions.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, scholars_adhering_to_literal_readings, payer,
    powerful, generational, identity_locked, global).

% Face a risk of epistemic violence, as their deeply held beliefs and social structures, often derived from literal interpretations, are invalidated. Their communal identity and sense of religious authenticity are challenged, leading to internal conflict and resistance to change. Exit from their community or faith is often not a viable option.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_readings, payer,
    moderate, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutical framework for reconciling seemingly contradictory Qur'anic verses on gender, allowing for a consistent, egalitarian ethical trajectory within Islamic thought.
% TRANSFER_FUNCTION: Transfers normative authority from earlier, gender-specific verses to later, universal principles of human dignity, effectively shifting legal and social rights towards women and away from traditional male-centric interpretations.
% ABSENT_VOICES: Early Islamic jurists and exegetes whose interpretations form the bedrock of traditional gender roles are 'absent' in the sense that their historical context and methods are re-evaluated and, in some cases, superseded by this reading. Their voices are present in the historical record but are re-contextualized or abrogated by this progressive approach.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the intellectual and social momentum for gender parity within Islamic legal thought would be severely hampered. Progressive scholars would lose a key hermeneutical tool, and Muslim women seeking equality within their faith would face renewed challenges from traditional interpretations, leading to a significant rearrangement of reformist efforts and legal advocacy.
% FOUNDING_PROBLEM: The apparent contradiction between early Qur'anic verses that seem to establish gender hierarchy and later verses emphasizing universal human dignity and equality, creating tension for modern Muslims seeking to reconcile faith with contemporary ethical standards.
% FOUNDING_PROBLEM_CORROBORATION: Progressive Muslim theologians, feminist scholars, and human rights advocates within Muslim-majority contexts widely attest to the ongoing nature of this problem. Traditional scholars acknowledge the textual tension but dispute the proposed solution, framing it as an interpretive challenge rather than a contradiction requiring abrogation. The problem's existence is broadly corroborated, though its resolution is highly contested.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.95, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.95) because this reading fundamentally reconfigures centuries of Islamic jurisprudence, effectively 'extracting' normative power from established interpretations and transferring it to a new, progressive framework. Suppression (0.88) is also very high, as this reading requires actively challenging and delegitimizing traditional hermeneutics and the institutions that uphold them. Resistance is high (0.9) from traditionalists who view this as an illegitimate reinterpretation. Accessibility collapse is low (0.2) because this reading is an active intellectual and social project, not a natural law; alternatives (literal and contextual readings) are robustly defended. Theater ratio is low (0.1) as this is a direct, confrontational interpretive move with little performative cover.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of progressive scholars and Muslim women, this reading is a 'rope' or 'scaffold' – a necessary coordination mechanism to achieve justice and reconcile faith with modern ethics. From the perspective of traditional institutions and scholars, it is a 'snare' – an aggressive, illegitimate reinterpretation that undermines divine law and their established authority. The engine's classification as 'snare' reflects the structural reality of its high extractiveness and suppression from the perspective of those it targets, regardless of the beneficiaries' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive Islamic scholars and Muslim women seeking parity are beneficiaries (d near 0.0), as this reading empowers their reformist and egalitarian agendas. Traditional Islamic institutions and scholars adhering to literal readings are clear targets (d near 1.0), as their authority and interpretations are directly challenged and delegitimized. Communities bound to literal readings are also targets, facing identity-locked costs. The 'extraction' here is a normative one: the extraction of interpretive authority and social power from one group to another.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it is a live interpretive project, not an atrophied function. Instead, it represents an active contest over the mandate itself. The high extractiveness and suppression are not signs of decay but of a forceful, ongoing redefinition of the mandate. The classification as 'snare' prevents mislabeling this as a 'rope' (pure coordination) by highlighting the coercive and asymmetric nature of the normative shift it seeks to impose on traditional structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_naskh_application,
    'Is the principle of naskh (abrogation) legitimately applicable to ethical and legal verses in the manner proposed by the progressive abrogation reading, or is its application limited to specific ritual/legal contexts?',
    'Consensus among leading Islamic legal theorists on the scope and methodology of naskh, or a widely accepted re-evaluation of classical usul al-fiqh (principles of jurisprudence) that explicitly endorses this application.',
    'If legitimate, the reading gains significant internal Islamic legal force, increasing its effective extractiveness from traditional interpretations. If illegitimate, the reading loses its primary hermeneutical tool, reducing its capacity to challenge existing norms and potentially reclassifying it as a ''piton'' (theatrical performance without real legal force).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_naskh_application, conceptual, 'The conceptual validity of applying naskh to achieve gender parity.').

omega_variable(
    epistemic_violence_vs_liberation,
    'At what point does the ''extraction'' from traditional communities, in the name of progressive abrogation, cross the line into epistemic violence, and how is this balanced against the ''liberation'' experienced by beneficiaries?',
    'Qualitative sociological studies on the lived experiences of communities undergoing this interpretive shift, combined with ethical frameworks for inter-communal dialogue and recognition of diverse religious identities.',
    'If the ''epistemic violence'' is deemed severe and unmitigated, the constraint''s overall ethical standing is compromised, even if it benefits some. If the ''liberation'' is widely embraced without undue coercion, the ethical justification for the extraction is strengthened. This impacts the normative evaluation of the constraint, potentially shifting its ''claimed_type'' from ''snare'' to ''scaffold'' if the transition is managed ethically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_violence_vs_liberation, preference, 'Ethical balance between challenging traditional norms and respecting communal identity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional resistance, legal barriers) or internalized (cognitive patterns, identity fusion within traditional communities)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism (e.g., institutional pressure) is removed, reclassify as partially internalized. This would involve observing communities that have adopted the progressive reading and assessing the persistence of internal resistance to change.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — traditional communities carry the suppression with them after external barriers are removed, making the shift more difficult and prolonged. This would reinforce the ''snare'' classification due to the deep-seated nature of the resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in traditional communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1980, quranic_gender_verses__progressive_abrogation, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(qura_tr_t1990, quranic_gender_verses__progressive_abrogation, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(qura_tr_t2000, quranic_gender_verses__progressive_abrogation, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(qura_tr_t2010, quranic_gender_verses__progressive_abrogation, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(qura_tr_t2024, quranic_gender_verses__progressive_abrogation, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t1980, quranic_gender_verses__progressive_abrogation, base_extractiveness, 1980, 0.85).
narrative_ontology:measurement(qura_be_t1990, quranic_gender_verses__progressive_abrogation, base_extractiveness, 1990, 0.88).
narrative_ontology:measurement(qura_be_t2000, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2000, 0.91).
narrative_ontology:measurement(qura_be_t2010, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2010, 0.93).
narrative_ontology:measurement(qura_be_t2024, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1980, quranic_gender_verses__progressive_abrogation, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(qura_su_t1990, quranic_gender_verses__progressive_abrogation, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(qura_su_t2000, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(qura_su_t2010, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement(qura_su_t2024, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quranic_gender_verses' kernel. This 'progressive abrogation' reading directly challenges the 'literal_hierarchical' and 'contextual_egalitarian' readings by proposing a complete normative reversal via naskh.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
