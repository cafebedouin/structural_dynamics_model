% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Quran 9:5 Contextual Defensive Reading
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the contextual_defensive reading of Quranic
 *   verse 9:5 (the 'sword verse'), which holds that the verse addresses a
 *   specific 7th-century Medinan crisis of treaty-breaking by allied pagan
 *   tribes and does not abrogate prior peaceful verses. The reading
 *   prioritizes treaty obligations and restricts permitted warfare to
 *   defensive reciprocity. It functions as a hermeneutic rule that
 *   coordinates peaceful pluralism for Muslim-majority states and protected
 *   minorities while conditionally authorizing violence against treaty
 *   violators. The constraint is actively enforced by classical jurists and
 *   integrationist institutions against the simpler abrogating-universal
 *   reading.
 *
 * KEY AGENTS:
 *   - - integrationist_muslim_states: Primary beneficiary (institutional/constrained) â gain theological architecture for domestic and international pluralism
 *   - - peaceful_religious_minorities: Secondary beneficiary (moderate/constrained) â protected from aggressive expansion by interpretive limitation
 *   - - classical_jurists_contextualist: Agenda-setter (institutional/constrained) â maintain asbab al-nuzul framework and enforce contextual hermeneutics
 *   - - treaty_violators: Primary target (organized/trapped) â exposed to defensive military authorization when they break treaties
 *   - - expansionist_jihadist_movements: Excluded voice (organized/identity_locked) â structurally barred from institutional discourse by the reading's non-abrogation premise
 *   - - international_human_rights_observers: Analytical observer (institutional/analytical) â monitor application consistency with humanitarian law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.45).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.6).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.45).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Quran 9:5 Contextual Defensive Reading").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "islamic_jurisprudence/hermeneutics/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__contextual_defensive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, 'e2dc3b54-fe69-423a-bb90-3a267f8cb597').
narrative_ontology:cs_kernel_codification('e2dc3b54-fe69-423a-bb90-3a267f8cb597', fixed_text).
narrative_ontology:cs_authority_grounding('e2dc3b54-fe69-423a-bb90-3a267f8cb597', lineage).
narrative_ontology:cs_interpretation_layer_present('e2dc3b54-fe69-423a-bb90-3a267f8cb597').
narrative_ontology:cs_reading_relation('e2dc3b54-fe69-423a-bb90-3a267f8cb597', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('e2dc3b54-fe69-423a-bb90-3a267f8cb597', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('e2dc3b54-fe69-423a-bb90-3a267f8cb597', foundational, non_abrogation_of_peaceful_verses).
narrative_ontology:cs_axiom_status(non_abrogation_of_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('e2dc3b54-fe69-423a-bb90-3a267f8cb597', non_abrogation_of_peaceful_verses, theological).
narrative_ontology:cs_axiom('e2dc3b54-fe69-423a-bb90-3a267f8cb597', foundational, defensive_reciprocity_only).
narrative_ontology:cs_axiom_status(defensive_reciprocity_only, holdable).
narrative_ontology:cs_axiom_grounding('e2dc3b54-fe69-423a-bb90-3a267f8cb597', defensive_reciprocity_only, conventional).
narrative_ontology:cs_reference_frame('e2dc3b54-fe69-423a-bb90-3a267f8cb597', prophetic_medina_treaty_order).
narrative_ontology:cs_drift_state('e2dc3b54-fe69-423a-bb90-3a267f8cb597', contemporary_nation_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2dc3b54-fe69-423a-bb90-3a267f8cb597', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, peaceful_religious_minorities).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_violators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern pluralistic societies and rely on hermeneutic frameworks that permit coexistence with non-Muslim minorities and foreign states while preserving Islamic legitimacy. This reading allows diplomatic normalization and domestic religious diversity without theological contradiction, but they cannot exit the framework of Islamic legitimacy without losing their constitutive identity.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_states, beneficiary,
    institutional, generational, constrained, national).

% Non-Muslim communities residing in or adjacent to Muslim-majority polities whose security depends on theological constraints that prohibit aggressive expansion. Their protected status is contingent on the persistence of the contextual reading, and they have limited exit options from the geopolitical and legal context in which they live.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, peaceful_religious_minorities, beneficiary,
    moderate, biographical, constrained, national).

% Scholars and interpretive institutions that maintain the asbab al-nuzul framework and classical tafsir traditions restricting 9:5 to its historical occasion. They set the interpretive agenda by teaching, fatwa, and hermeneutic gatekeeping, actively enforcing the reading against abrogationist simplification that would collapse the tradition's pluralistic jurisprudence.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, classical_jurists_contextualist, agenda_setter,
    institutional, generational, constrained, global).

% Communities or states that break treaties with Muslim polities and are thereby exposed to defensive military action that the contextual reading authorizes. They bear the cost of the constraint's conditional permission of violence, and once designated as treaty-breakers within this framework, their options to avoid hostilities are severely constrained.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_violators, payer,
    organized, immediate, trapped, regional).

% Movements advocating universal offensive jihad and the abrogation of peaceful verses. They are structurally excluded from institutional Islamic discourse in integrationist states because the contextual reading denies them their core theological license, and their identity is fused with the abrogationist premise.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, expansionist_jihadist_movements, excluded,
    organized, biographical, identity_locked, global).

% Monitor whether Islamic legal interpretations comply with international humanitarian law norms of distinction and proportionality. They observe whether the contextual reading is applied consistently to limit violence or instrumentalized to justify selective military action.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, international_human_rights_observers, observer,
    institutional, generational, analytical, global).

narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates relations between Muslim and non-Muslim polities by establishing that Quranic permission of warfare is strictly defensive and triggered only by treaty violation, thereby stabilizing inter-communal expectations and enabling long-term diplomatic coexistence under a shared theological framework.
% TRANSFER_FUNCTION: Moves the authorization to conduct military action from any Muslim polity against any non-Muslim to a strictly conditional scenario of defensive reciprocity against treaty-breakers; transfers security and legal stability to treaty-honoring communities and integrationist states.
% ABSENT_VOICES: Expansionist jihadist scholars and movements who hold the abrogating-universal reading are excluded from the interpretive table in integrationist institutional and state contexts; their objections are pre-empted by hermeneutic rules that treat historical context as dispositive and non-abrogation as settled.
% DISAPPEARANCE_RATIONALE: If the contextual reading vanished, integrationist states would lose the theological architecture for treaty-based pluralism, abrogationist readings would gain institutional ground, and the security of peaceful religious minorities would destabilize as the permissibility of offensive jihad became canonically viable across mainstream institutions.
% FOUNDING_PROBLEM: The 7th-century Medinan polity faced specific treaty violations by allied pagan tribes in the immediate post-Hudaybiyyah period, threatening the community's survival; the verse was revealed to address this discrete diplomatic-military crisis of betrayal by treaty parties.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary historians and critical scholars outside the Islamic theological tradition corroborate the historical context of treaty-breaking at Hudaybiyyah and its aftermath. Secular historians and orientalists provide independent attestation of the specific 7th-century military-diplomatic crisis, while integrationist Muslim scholars attest to the contextual occasion from within the tradition.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.45) is moderate because the constraint authorizes violence, but only in a narrow, defensive, treaty-conditioned scenario. Suppression (0.60) is higher than extraction because the reading's persistence requires active scholarly enforcement against the intellectually simpler abrogationist alternative. Theater ratio (0.30) reflects increasing instrumentalization of the reading by modern states seeking diplomatic legitimacy, though the coordination function remains substantive. Accessibility collapse (0.50) is moderate: abrogationist readings persist in counter-publics and jihadist discourse, but are marginalized in official institutional contexts. Resistance (0.60) is substantial due to sustained abrogationist scholarly and militant opposition. The temporal series trace a slow ramp in suppression requirement as the abrogationist challenge intensified from early Islam through the modern nation-state era.
 *
 * PERSPECTIVAL GAP:
 *   Integrationist states and peaceful minorities experience the constraint as protective coordination that stabilizes inter-communal order. Treaty violators experience it as the removal of theological protection that exposes them to authorized retaliation. Expansionist movements experience it as hermeneutic suppression that denies them their preferred political license. The engine computes this divergence from structural role and exit data.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist states and religious minorities are structural beneficiaries (low d) because the constraint subsidizes their security and legitimacy. Treaty violators are structural targets (high d) because the constraint amplifies the cost of their treaty violation through authorized violence. Classical jurists sit near symmetric (moderate d): they enforce and maintain the constraint but do not personally capture its extraction. Expansionist movements, though excluded, would compute as full targets because the constraint's existence blocks their ideological program.
 *
 * MANDATROPHY ANALYSIS:
 *   The contextual reading prevents misclassification of the underlying textual kernel as a pure snare. By restricting the victim set to treaty violators and embedding the verse within a broader Quranic architecture of treaty fidelity, the reading preserves a genuine coordination function (peaceful pluralism) alongside its conditional extraction. If the contextual frame were abandoned, the same text would likely compute as a snare under the abrogating-universal reading. The R5 genealogy flags a potential mandatrophy because the founding problem (7th-century treaty crisis) is dead, yet the constraint persists because it has been repurposed for modern coordination. This is drift rather than simple atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_occasion_vs_eternal_scope,
    'Does the historical occasion of 9:5 fully exhaust its normative scope, or does the text retain residual legal force beyond 7th-century Medina?',
    'Comparative legal history of Quranic verses treated as historically bounded versus eternal commands; philological analysis of the verse''s grammatical scope markers and classical tafsir transmission.',
    'If the historical occasion fully exhausts scope, the constraint becomes a scaffold or historical relic; if residual force remains, the constraint remains a living tangled rope with active conditional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_occasion_vs_eternal_scope, conceptual, 'Whether the verse''s normative force is bounded to its original historical occasion.').

omega_variable(
    modern_treaty_violator_identification,
    'Who qualifies as a treaty-breaker under modern conditions â sovereign states, non-state militias, apostate individuals â and does the victim set expand beyond the historical referent?',
    'Empirical tracking of which actors are actually designated treaty-breakers by states and non-state actors applying this reading in contemporary conflicts.',
    'An expanding victim set under modern application would increase extractiveness and shift classification toward snare; a fixed historical referent would keep extraction bounded and preserve the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modern_treaty_violator_identification, empirical, 'Modern expansion of the treaty-breaker category and its impact on victim scope.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of abrogationist readings structural (state censorship, institutional gatekeeping) or internalized (theological consensus that renders abrogation unthinkable)?',
    'Observing whether abrogationist readings resurge when institutional enforcement weakens, such as in failed states or diaspora communities outside official scholarly control.',
    'If internalized, effective suppression is higher than the structural measure suggests and the constraint is more robust; if purely structural, it is fragile and dependent on state power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of rival readings.').

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the contested kernel quran_9_5_scope. How would classification change if the abrogating_universal or progressive_synthesis reading were adopted instead?',
    'Comparative analysis of sibling constraint stories in the same kernel family.',
    'Abrogating_universal would produce a snare with a universal victim set and high extraction; progressive_synthesis would dissolve the legal constraint entirely, producing minimal extraction but also eliminating the coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Sibling reading structural deltas and their classification consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t280, quran_9_5_scope__contextual_defensive, theater_ratio, 280, 0.15).
narrative_ontology:measurement(qura_tr_t560, quran_9_5_scope__contextual_defensive, theater_ratio, 560, 0.2).
narrative_ontology:measurement(qura_tr_t840, quran_9_5_scope__contextual_defensive, theater_ratio, 840, 0.25).
narrative_ontology:measurement(qura_tr_t1120, quran_9_5_scope__contextual_defensive, theater_ratio, 1120, 0.28).
narrative_ontology:measurement(qura_tr_t1400, quran_9_5_scope__contextual_defensive, theater_ratio, 1400, 0.3).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(qura_be_t280, quran_9_5_scope__contextual_defensive, base_extractiveness, 280, 0.35).
narrative_ontology:measurement(qura_be_t560, quran_9_5_scope__contextual_defensive, base_extractiveness, 560, 0.4).
narrative_ontology:measurement(qura_be_t840, quran_9_5_scope__contextual_defensive, base_extractiveness, 840, 0.42).
narrative_ontology:measurement(qura_be_t1120, quran_9_5_scope__contextual_defensive, base_extractiveness, 1120, 0.44).
narrative_ontology:measurement(qura_be_t1400, quran_9_5_scope__contextual_defensive, base_extractiveness, 1400, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(qura_su_t280, quran_9_5_scope__contextual_defensive, suppression_requirement, 280, 0.3).
narrative_ontology:measurement(qura_su_t560, quran_9_5_scope__contextual_defensive, suppression_requirement, 560, 0.4).
narrative_ontology:measurement(qura_su_t840, quran_9_5_scope__contextual_defensive, suppression_requirement, 840, 0.5).
narrative_ontology:measurement(qura_su_t1120, quran_9_5_scope__contextual_defensive, suppression_requirement, 1120, 0.58).
narrative_ontology:measurement(qura_su_t1400, quran_9_5_scope__contextual_defensive, suppression_requirement, 1400, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% The natural-language label 'Quran 9:5' conflates three structurally distinct constraints. The abrogating_universal reading produces a high-extraction snare with a universal victim set. The contextual_defensive reading produces a tangled rope with bounded extraction against treaty violators and active coordination for pluralism. The progressive_synthesis reading dissolves the legal constraint entirely, treating the verse as a time-bound political directive. Each reading carries a different epsilon, beneficiary structure, and enforcement profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
