% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Quran 9:5 as Contextual Defensive Warfare Restriction
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the contextual_defensive reading of
 *   the quran_9_5_scope kernel. The reading restricts 9:5's permission to
 *   fight to the specific historical category of treaty-breaking aggressors
 *   in 7th-century Medina, preserving the operative force of peaceful
 *   coexistence verses. It is claimed as a tangled_rope: it performs genuine
 *   coordination (stable pluralist legal order, treaty credibility, minority
 *   protection) while extracting compliance costs from those who would invoke
 *   universal offensive jihad (literalist universalist scholars, expansionist
 *   political actors). The constraint requires active enforcement through
 *   scholarly consensus, state policy, and educational transmission to
 *   maintain its interpretive boundary against the abrogating reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.22).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.15).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.22).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Quran 9:5 as Contextual Defensive Warfare Restriction").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "islamic_jurisprudence/hermeneutics/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__contextual_defensive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, 'a41f18d7-66d1-48fd-96b4-6e844fcf40d4').
narrative_ontology:cs_kernel_codification('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', fixed_text).
narrative_ontology:cs_authority_grounding('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', lineage).
narrative_ontology:cs_interpretation_layer_present('a41f18d7-66d1-48fd-96b4-6e844fcf40d4').
narrative_ontology:cs_reading_relation('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', foundational, verse_9_5_particular_to_treaty_breakers).
narrative_ontology:cs_axiom_status(verse_9_5_particular_to_treaty_breakers, holdable).
narrative_ontology:cs_axiom_grounding('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', verse_9_5_particular_to_treaty_breakers, empirically_contingent).
narrative_ontology:cs_axiom('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', foundational, peaceful_verses_not_abrogated).
narrative_ontology:cs_axiom_status(peaceful_verses_not_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', peaceful_verses_not_abrogated, deontological).
narrative_ontology:cs_axiom('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', secondary, treaty_obligation_supersedes_religious_difference).
narrative_ontology:cs_axiom_status(treaty_obligation_supersedes_religious_difference, holdable).
narrative_ontology:cs_axiom_grounding('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', treaty_obligation_supersedes_religious_difference, conventional).
narrative_ontology:cs_reference_frame('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', prophetic_medinan_practice).
narrative_ontology:cs_drift_state('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', classical_imperial_fiqh_codification, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a41f18d7-66d1-48fd-96b4-6e844fcf40d4', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, pluralist_scholarly_traditions).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, non_muslim_minorities_under_treaty).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_violating_polytheist_groups).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, aggressive_opponents_breaching_pacts).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, treaty_obligations_supersede_religious_difference).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, defensive_warfare_only_principle).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, pluralist_coexistence_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State actors that adopt the contextual defensive reading as governing legal framework. They structure military and diplomatic policy around treaty compliance and defensive-only engagement. The reading enables peaceful pluralism and international legitimacy; abandoning it would require restructuring foreign relations and domestic law.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary).

% Scholarly networks and institutions (e.g., Azhar, Zaytuna, various madhhab traditions) that maintain and teach the contextual defensive reading. Their interpretive authority and institutional continuity depend on this reading's coherence. Exit means abandoning a centuries-old hermeneutical framework and the communities that sustain it.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, pluralist_scholarly_traditions, beneficiary,
    organized, generational, constrained, global).

% Non-Muslim communities living under treaty protections (dhimma or modern citizenship equivalents) in Muslim-majority polities. The contextual reading structurally protects their security and religious autonomy; the alternative readings remove that protection. They have no exit from the constraint's operation — their fate is decided by which reading prevails.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, non_muslim_minorities_under_treaty, beneficiary,
    powerless, biographical, trapped, local).

% Historical referent: the specific Medinan tribes who broke the Treaty of Hudaybiyyah and allied with Meccan forces against the Muslim community. The constraint's coercive force (permission to fight) applies ONLY to this category — those who initiate violation. Their 'exit' is compliance with treaty terms; the constraint does not target them for identity but for breach.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_violating_polytheist_groups, payer,
    moderate, immediate, constrained, local).

% Generalized structural position: any group that enters a treaty with the Muslim polity and then violates it to wage war. The reading's permission to fight applies exclusively to this behavioral category, not to polytheists or non-Muslims per se. Exit is cessation of aggression and return to treaty compliance.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, aggressive_opponents_breaching_pacts, payer,
    moderate, biographical, constrained, regional).

% Scholarly voices (classical and contemporary) who read 9:5 as abrogating peaceful verses and establishing universal offensive jihad. They are structurally excluded from the interpretive community that sustains this reading — their framework treats the contextual reading as invalid. Their identity is fused to the abrogation thesis; exit would require abandoning their entire hermeneutical project.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, literalist_universalist_scholars, excluded,
    organized, generational, identity_locked, global).

% Scholarly voices who read 9:5 as a time-bound 7th-century political directive with no eternal legal force. They are excluded from this reading's framework because they deny the verse's ongoing legal authority altogether — they do not debate its scope, they reject its bindingness. Exit from exclusion would require accepting the verse as legally binding in some form.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, progressive_reformist_scholars, excluded,
    organized, generational, constrained, global).

% Analytical seat examining the structural relationships between the three readings of the quran_9_5_scope kernel. Sees how each reading constructs different beneficiary/victim sets, different coercion profiles, and different legitimacy economies. No stake in any reading's victory.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, comparative_quranic_hermeneutics_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable hermeneutical framework that preserves peaceful coexistence verses (e.g., 2:256, 5:48, 60:8) as operative law while restricting 9:5's permission to fight to a specific historical category (treaty-breakers who initiate aggression). This solves the coordination problem of maintaining Islamic legal continuity without perpetual warfare obligation.
% TRANSFER_FUNCTION: Transfers the burden of proof for military action from 'non-Muslim identity' to 'treaty violation and aggression initiation.' The constraint moves the legal trigger for warfare from religious difference to behavioral breach. Gains (peaceful pluralism, treaty stability, international legitimacy) accrue to integrationist states and minority communities; costs (restriction on offensive capacity) are borne by actors who would invoke universal jihad.
% ABSENT_VOICES: The literalist universalist scholars (excluded) would object that this reading nullifies a clear divine command for universal jihad. The progressive reformist scholars (excluded) would object that this reading still treats a 7th-century wartime directive as binding law. Both are structurally absent from the interpretive community that authorizes this reading — the first by identity-locked commitment to abrogation, the second by rejection of the verse's legal authority.
% DISAPPEARANCE_RATIONALE: If the contextual defensive reading vanished overnight, the legal space would be occupied by either the abrogating universal reading (triggering universal offensive jihad obligation, collapse of treaty protections for minorities, destabilization of Muslim-majority state international relations) or the progressive synthesis reading (removing 9:5 from legal corpus entirely, creating vacuum in classical fiqh of warfare, delegitimizing traditional scholarly authority). Either successor restructures the Islamic legal order.
% FOUNDING_PROBLEM: The early Muslim community in Medina faced existential threat from Meccan polytheists who repeatedly violated treaties (notably Hudaybiyyah) and mobilized for extermination. The founding problem was: how to legally authorize defensive force against specific treacherous actors without converting that authorization into a permanent license for offensive warfare against all non-Muslims, thereby preserving the Quran's own pluralist verses and the Prophet's treaty practice.
% FOUNDING_PROBLEM_CORROBORATION: Classical tafsir tradition (Tabari, Ibn Kathir, Qurtubi) documents the specific historical occasion of revelation (asbab al-nuzul) linking 9:5 to the Hudaybiyyah treaty-breakers. Modern historians of early Islam (Watt, Donner, Crone) corroborate the treaty-violation context from non-Muslim sources. The founding problem is attested by the verse's own textual context (9:1-4 on treaty completion) and by the Prophet's subsequent practice (conquest of Mecca with general amnesty, treaties with non-Muslim polities). No credible scholarship disputes the historical particularity of the occasion.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.22) reflects the reading's restriction on actors who would wage offensive jihad — they bear the cost of forgone expansionist legitimacy. Suppression (0.15) is low because the reading operates through interpretive authority and legal structure, not coercion; its persistence depends on scholarly and institutional maintenance, not force. Theater ratio (0.18) captures the performative invocation of 9:5 by expansionist actors who must be rhetorically contained. Accessibility collapse (0.35) is moderate: alternatives (abrogating reading, progressive reading) remain live and structurally accessible. Resistance (0.45) reflects ongoing contestation from both excluded readings. Measurements show extractiveness peaking in classical imperial period (1000 AH) when expansionist states instrumentalized the abrogating reading, then declining as integrationist states reasserted the contextual reading in modern era.
 *
 * PERSPECTIVAL GAP:
 *   From the integrationist state seat, this is a rope (genuine coordination for pluralist order). From the literalist scholar seat, it is a snare (suppressing the true divine command). From the non-Muslim minority seat, it is a mountain (the only thing preventing their legal eradication). The engine computes these per-seat types from the structural data; the authored claim (tangled_rope) reflects the aggregate structural reality of coordination-with-asymmetric-costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist states and pluralist traditions are structural beneficiaries (d near 0.0) — the reading subsidizes their legitimacy, pluralism, and international standing. Non-Muslim minorities are trapped beneficiaries (d ~ 0.1) — they cannot exit the constraint's protection but depend on it existentially. Treaty violators are the designated targets (d ~ 0.9) but only when they initiate breach; the constraint's directionality toward them is conditional on their aggression. Literalist scholars are identity-locked excluded (d ~ 0.7) — their hermeneutical identity requires rejecting this reading. Progressive scholars are constrained excluded (d ~ 0.4) — they could engage but choose structural rejection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential defense against treaty-breakers) remains live: Muslim-majority states still face actors who violate agreements and initiate aggression. The reading has not atrophied into piton because its coordination function (treaty credibility, pluralist stability) is actively demanded by current geopolitical reality. However, theater ratio has risen as the reading is increasingly performed as identity marker rather than operational legal framework in some contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the quran_9_5_scope kernel a single constraint with contested interpretation, or are the three readings structurally distinct constraints sharing only a textual label?',
    'Apply ε-invariance test: if measuring extractiveness/suppression under each reading yields stable, divergent values that do not change with observable selection, they are distinct constraints. Current evidence suggests three distinct ε values (contextual_defensive ~0.22, abrogating_universal ~0.75, progressive_synthesis ~0.05).',
    'If distinct constraints, the kernel is a colloquial label masking structural divergence — each reading must be authored separately with network.affects_constraints links. If single constraint, ε would be observer-relative, violating DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel label conceals multiple ε-invariant constraints.').

omega_variable(
    abrogation_mechanism_structural_role,
    'Does the naskh (abrogation) doctrine function as a coordination mechanism (resolving textual tension) or an extraction mechanism (concentrating interpretive authority in literalist institutions)?',
    'Trace the institutional history of naskh: if its application correlates with expansionist state policy and scholarly patronage, extraction function dominates. If it resolves genuine textual contradictions across the corpus independent of policy, coordination function dominates.',
    'If extraction-dominant, the abrogating_universal reading is a snare using naskh as cover. If coordination-dominant, the contextual_defensive reading must demonstrate how it resolves the same textual tensions without naskh (e.g., through takhsis, historical contextualization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_mechanism_structural_role, empirical, 'Whether the abrogation doctrine is structurally coordinative or extractive.').

omega_variable(
    minority_protection_counterfactual,
    'Would non-Muslim minorities under Muslim rule have equivalent protections under the progressive_synthesis reading (which removes 9:5 from legal force entirely)?',
    'Compare minority outcomes in polities adopting progressive synthesis (e.g., Tunisia post-2011, Turkey pre-2000s) vs. contextual defensive (e.g., Morocco, Indonesia, pre-1979 Iran). Assess whether treaty-based protections (dhimma/citizenship) are more durable than secular constitutional guarantees in Muslim-majority contexts.',
    'If progressive synthesis provides equal or better protection, the contextual reading''s coordination claim for minorities weakens. If contextual reading provides superior protection, its tangled_rope status (coordination + asymmetric cost on literalists) is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_counterfactual, empirical, 'Whether minority protection requires the contextual reading''s specific structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qura_tr_t200, quran_9_5_scope__contextual_defensive, theater_ratio, 200, 0.1).
narrative_ontology:measurement(qura_tr_t400, quran_9_5_scope__contextual_defensive, theater_ratio, 400, 0.15).
narrative_ontology:measurement(qura_tr_t600, quran_9_5_scope__contextual_defensive, theater_ratio, 600, 0.18).
narrative_ontology:measurement(qura_tr_t800, quran_9_5_scope__contextual_defensive, theater_ratio, 800, 0.2).
narrative_ontology:measurement(qura_tr_t1000, quran_9_5_scope__contextual_defensive, theater_ratio, 1000, 0.22).
narrative_ontology:measurement(qura_tr_t1200, quran_9_5_scope__contextual_defensive, theater_ratio, 1200, 0.18).
narrative_ontology:measurement(qura_tr_t1400, quran_9_5_scope__contextual_defensive, theater_ratio, 1400, 0.18).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(qura_be_t200, quran_9_5_scope__contextual_defensive, base_extractiveness, 200, 0.12).
narrative_ontology:measurement(qura_be_t400, quran_9_5_scope__contextual_defensive, base_extractiveness, 400, 0.18).
narrative_ontology:measurement(qura_be_t600, quran_9_5_scope__contextual_defensive, base_extractiveness, 600, 0.22).
narrative_ontology:measurement(qura_be_t800, quran_9_5_scope__contextual_defensive, base_extractiveness, 800, 0.25).
narrative_ontology:measurement(qura_be_t1000, quran_9_5_scope__contextual_defensive, base_extractiveness, 1000, 0.28).
narrative_ontology:measurement(qura_be_t1200, quran_9_5_scope__contextual_defensive, base_extractiveness, 1200, 0.22).
narrative_ontology:measurement(qura_be_t1400, quran_9_5_scope__contextual_defensive, base_extractiveness, 1400, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(qura_su_t200, quran_9_5_scope__contextual_defensive, suppression_requirement, 200, 0.12).
narrative_ontology:measurement(qura_su_t400, quran_9_5_scope__contextual_defensive, suppression_requirement, 400, 0.15).
narrative_ontology:measurement(qura_su_t600, quran_9_5_scope__contextual_defensive, suppression_requirement, 600, 0.15).
narrative_ontology:measurement(qura_su_t800, quran_9_5_scope__contextual_defensive, suppression_requirement, 800, 0.15).
narrative_ontology:measurement(qura_su_t1000, quran_9_5_scope__contextual_defensive, suppression_requirement, 1000, 0.15).
narrative_ontology:measurement(qura_su_t1200, quran_9_5_scope__contextual_defensive, suppression_requirement, 1200, 0.15).
narrative_ontology:measurement(qura_su_t1400, quran_9_5_scope__contextual_defensive, suppression_requirement, 1400, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__contextual_defensive, 0.08).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, dhimma_contract_structure).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, islamic_international_law_siyar).

% DUAL FORMULATION NOTE:
% This constraint is one member of the quran_9_5_scope constraint family. The three readings (contextual_defensive, abrogating_universal, progressive_synthesis) are distinct ε-invariant constraints linked by shared textual label but divergent beneficiary/victim structures, extractiveness values, and operational logics. The contextual_defensive reading coordinates pluralist legal order at cost of restricting offensive jihad legitimacy; the abrogating_universal reading coordinates expansionist mobilization at cost of minority eradication and treaty collapse; the progressive_synthesis reading coordinates modern ethical alignment at cost of classical fiqh authority. Each has its own ε, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__contextual_defensive, powerless, 0.1).
constraint_indexing:directionality_override(quran_9_5_scope__contextual_defensive, organized, 0.7).
constraint_indexing:directionality_override(quran_9_5_scope__contextual_defensive, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
