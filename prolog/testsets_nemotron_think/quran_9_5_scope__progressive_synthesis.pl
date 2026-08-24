% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__progressive_synthesis, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Quran 9:5 as Time-Bound Political Directive (Progressive Synthesis Reading)
 *   domain: religious/hermeneutic/political_theology
 *
 * SUMMARY:
 *   This constraint story models the progressive_synthesis reading of Quran
 *   9:5 (the 'Sword Verse'). The reading holds that the verse was a
 *   time-bound 7th-century political directive addressing specific
 *   treaty-breaking polytheist tribes, not an eternal legal command. The
 *   Quran's ethical trajectory — discernible through the arc from
 *   particularistic warfare rules to universal principles of justice, human
 *   dignity, and 'no compulsion in religion' (2:256) — supersedes the verse's
 *   literalist application. Consequently, the verse exits active constraint
 *   space entirely: neither polytheists nor Muslims are constrained by its
 *   directive today. The beneficiaries of this expiration are
 *   secular-pluralist frameworks and progressive Muslim interpreters; the
 *   victims are textualist authority structures whose identity and legitimacy
 *   are fused to the verse's ongoing binding force, and literalist adherents
 *   who are identity-locked into a reading that extracts from them via
 *   cognitive capture and social marginalization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.25).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.3).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.25).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, scaffold).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Quran 9:5 as Time-Bound Political Directive (Progressive Synthesis Reading)").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "religious/hermeneutic/political_theology").

narrative_ontology:has_sunset_clause(quran_9_5_scope__progressive_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, 'bfe8fd75-c344-4ea7-97ca-6794495bdbcc').
narrative_ontology:cs_kernel_codification('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', fixed_text).
narrative_ontology:cs_authority_grounding('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', lineage).
narrative_ontology:cs_interpretation_layer_present('bfe8fd75-c344-4ea7-97ca-6794495bdbcc').
narrative_ontology:cs_reading_relation('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', quran_9_5_scope__contextual_defensive, coexists_with).
narrative_ontology:cs_axiom('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', foundational, quranic_ethical_trajectory_supersedes_literal_command).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_supersedes_literal_command, holdable).
narrative_ontology:cs_axiom_grounding('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', quranic_ethical_trajectory_supersedes_literal_command, deontological).
narrative_ontology:cs_axiom('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', foundational, revelation_is_historically_situated).
narrative_ontology:cs_axiom_status(revelation_is_historically_situated, holdable).
narrative_ontology:cs_axiom_grounding('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', revelation_is_historically_situated, empirically_contingent).
narrative_ontology:cs_reference_frame('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', classical_tafsir_consensus).
narrative_ontology:cs_drift_state('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', modern_hermeneutical_turn, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bfe8fd75-c344-4ea7-97ca-6794495bdbcc', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, progressive_muslim_interpreters).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, literalist_adherents).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, quranic_ethical_trajectory_supersedes_literalist_reading).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, historical_contextualization_of_revelation).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, no_compulsion_in_religion_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the verse's expiration as binding law because it removes a textual obstacle to pluralistic governance, religious freedom, and equal citizenship. Their frameworks gain legitimacy when scriptural literalism loses coercive force.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks, beneficiary,
    organized, generational, mobile, global).

% Claim the verse establishes universal offensive jihad as standing law. Their authority derives from maintaining the classical consensus (ijma) on the verse's abrogating force. They bear extraction costs through cognitive capture: their identity is fused to the literalist reading, forcing them to defend a position that undermines their legitimacy in modern contexts and fuels radicalization they cannot control.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_authority_structures, payer,
    institutional, generational, identity_locked, global).

% Develop and advocate the historical-contextual and ethical-trajectory readings. They gain intellectual authority and institutional space (academic, some state appointments) but face hostility, marginalization, and accusations of heresy from textualist structures. Their exit from mainstream recognition is constrained by gatekeeping.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, progressive_muslim_interpreters, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, progressive_muslim_interpreters, agenda_setter).

% Ordinary believers who accept textualist authority's reading as religious obligation. They bear psychological and social costs: either cognitive dissonance living in pluralistic societies, or radicalization toward violence, or withdrawal. Their identity is fused to the reading; exit means abandoning their self-concept as faithful Muslims.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, literalist_adherents, payer,
    moderate, biographical, identity_locked, global).

% The 7th-century polytheist tribes of Arabia who were the verse's original addressees. They had no voice in the revelation or its later interpretation. They are structurally excluded from any contemporary conversation about the verse's scope.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, polytheist_historical_reference, excluded,
    powerless, immediate, trapped, local).

% The hermeneutical principle that the Quran's ethical vector (tawhid, justice, human dignity) moves from particular historical instantiations toward universal moral norms. It is not an actor but the analytical standard by which this reading judges the verse's expired status.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, quranic_ethical_trajectory, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quran_9_5_scope__progressive_synthesis, quranic_ethical_trajectory).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically: coordinated 7th-century Medinan community defense against treaty-breaking polytheist alliances by providing a time-limited mobilization directive. Currently: coordinates contemporary Muslim ethical reasoning toward pluralism by demonstrating that the verse's historical particularity is superseded by the Quran's universal ethical trajectory (tawhid, human dignity, no compulsion).
% TRANSFER_FUNCTION: Historically transferred martial obligation and risk from the Muslim community onto polytheist tribes (as targets) and onto individual believers (as duty). Currently transfers interpretive authority and legitimacy from textualist authority structures (who claim the verse's universal binding force) to progressive/ethical frameworks (who read the verse as historically bounded).
% ABSENT_VOICES: The 7th-century polytheist tribes (original targets) are historically silenced. Contemporary Muslims living under authoritarian regimes that enforce textualist readings cannot access or advocate progressive readings without peril. Ex-Muslims and religious minorities in Muslim-majority contexts who bear the brunt of literalist enforcement are rarely consulted in hermeneutical debates.
% DISAPPEARANCE_RATIONALE: From this reading's perspective, the verse's binding claim has already disappeared as a matter of hermeneutical fact — the world would not rearrange materially because the directive is historically expired. However, textualist authority structures and literalist adherents violently contest this; for them, the verse's disappearance as binding law would rearrange their entire epistemic and social order. The verdict is therefore contested across the structural divide.
% FOUNDING_PROBLEM: The nascent Medinan community (622-632 CE) faced existential military threat from Meccan polytheists and their tribal allies who repeatedly violated treaties (e.g., Banu Qurayza, Banu Nadir, Meccan coalition at Battle of the Trench). The community needed a mobilization directive to unify defense and deter betrayal.
% FOUNDING_PROBLEM_CORROBORATION: Classical tafsir literature (Tabari, Ibn Kathir, Qurtubi) documents the asbab al-nuzul (occasions of revelation) linking 9:5 to specific treaty violations by named tribes. Modern historians (W. Montgomery Watt, Fred Donner, Patricia Crone) corroborate the political-military context of 7th-century Arabian tribal warfare and treaty diplomacy. Textualist authorities (e.g., Ibn Taymiyya's heirs, contemporary Salafi institutions) contest the 'dead' status, arguing the verse's ruling is eternal — but they do so from within the benefiting authority structure, not from independent historical evidence.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, contested).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).
:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.25 at interval end) reflects the reading's assessment: the verse itself extracts little directly today, but the textualist claim that it binds creates indirect extraction (cognitive capture of adherents, opportunity cost of pluralism). Suppression (0.3) is low for the verse itself but moderate for the textualist enforcement apparatus. Theater ratio (0.45) is significant: textualist authorities perform the verse's universality through ritualized citation while its actual historical particularity is suppressed in their discourse. Accessibility collapse (0.35) is moderate: alternative readings exist and are intellectually accessible, but identity-locked agents cannot reach them. Resistance (0.7) is high: textualist institutions actively resist this reading through fatwas, education systems, and state power where available.
 *
 * PERSPECTIVAL GAP:
 *   The textualist seat experiences this constraint as a Mountain (divine law, unchangeable, emerges_naturally) — their claimed_type diverges radically from this reading's claimed_type (Scaffold). The engine computes this divergence from the structural data: for textualists, exit_options=identity_locked and power=institutional create high effective extraction even if base ε is low; for progressives, exit_options=mobile and power=organized create low effective extraction. The same verse produces Mountain classification for one seat and Scaffold for another — exactly the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular_pluralist_frameworks and progressive_muslim_interpreters are beneficiaries (d near 0.0): they gain legitimacy and operational space when the verse's binding claim expires. Textualist_authority_structures and literalist_adherents are payers/victims (d near 1.0): the former lose authority-grounding; the latter bear cognitive capture costs. The directionality derivation from beneficiary/victim declarations + identity_locked exit captures this: identity_locked agents cannot exit the extractive reading even when it harms them, so d approaches 1.0 despite their not being the constraint's original target. Polytheist_historical_reference is excluded (no structural position in current constraint space). Quranic_ethical_trajectory is the analytical observer (d=0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The verse's founding problem (7th-century existential defense) is dead. The arrangement (classical consensus on universal offensive jihad) persists as a zombie constraint maintained by textualist authorities who extract authority from its enforcement. This reading resolves mandatrophy by declaring the sunset clause activated: the verse was a Scaffold whose transition function (ethical trajectory toward universal justice) has been fulfilled. The coordination function (community survival) is complete; the extraction function (universal jihad) is void. The mandatrophy_resolved flag should be true — the mandate has outlived its function, and the ethical trajectory provides the superseding framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_certainty,
    'How certain is the historical consensus that 9:5 addresses only specific treaty-breaking tribes versus a general polytheist population?',
    'Comparative analysis of asbab al-nuzul reports across tafsir traditions, correlated with sira/maghazi literature and epigraphic evidence from 7th-century Arabia.',
    'If historical context is ambiguous, the ''time-bound'' claim weakens and the verse''s scope becomes contestable rather than settled — strengthening abrogating_universal or contextual_defensive readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_certainty, empirical, 'Epistemic certainty of the historical particularity claim.').

omega_variable(
    ethical_trajectory_objectivity,
    'Is the ''Quranic ethical trajectory'' an objectively derivable hermeneutical principle or a reader-dependent projection?',
    'Formal analysis of Quranic intra-textual coherence: statistical measurement of value-term frequency shifts (e.g., ''justice'', ''mercy'', ''compulsion'') across chronological surah ordering, tested against reader-blind classification.',
    'If objectively derivable, the supersession claim has Mountain-like force; if reader-dependent, it is a contested interpretive choice — making this reading''s classification preference-class rather than empirical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_trajectory_objectivity, conceptual, 'Ontological status of the ethical trajectory hermeneutic.').

omega_variable(
    textualist_capture_mechanism,
    'Are textualist authorities identity-locked due to internalized cognitive patterns (internalized suppression) or structural barriers to exit (professional, social, institutional)?',
    'Longitudinal study of textualist scholars who defect to progressive readings: measure whether suppression persists post-exit (internalized) or dissolves (structural).',
    'If internalized, the constraint''s effective suppression on textualist agents is higher than structural measures suggest — they carry the suppression with them. This would increase χ for the textualist seat and strengthen the Snare/Tangled Rope classification from their perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_capture_mechanism, empirical, 'Structural vs. internalized suppression mechanism for textualist identity lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 0, 1392).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q95_prog_tr_t0, quran_9_5_scope__progressive_synthesis, theater_ratio, 0, 0.1).
narrative_ontology:measurement(q95_prog_tr_t100, quran_9_5_scope__progressive_synthesis, theater_ratio, 100, 0.15).
narrative_ontology:measurement(q95_prog_tr_t300, quran_9_5_scope__progressive_synthesis, theater_ratio, 300, 0.25).
narrative_ontology:measurement(q95_prog_tr_t600, quran_9_5_scope__progressive_synthesis, theater_ratio, 600, 0.35).
narrative_ontology:measurement(q95_prog_tr_t900, quran_9_5_scope__progressive_synthesis, theater_ratio, 900, 0.4).
narrative_ontology:measurement(q95_prog_tr_t1200, quran_9_5_scope__progressive_synthesis, theater_ratio, 1200, 0.45).
narrative_ontology:measurement(q95_prog_tr_t1392, quran_9_5_scope__progressive_synthesis, theater_ratio, 1392, 0.5).

% Extraction over time
narrative_ontology:measurement(q95_prog_be_t0, quran_9_5_scope__progressive_synthesis, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(q95_prog_be_t100, quran_9_5_scope__progressive_synthesis, base_extractiveness, 100, 0.75).
narrative_ontology:measurement(q95_prog_be_t300, quran_9_5_scope__progressive_synthesis, base_extractiveness, 300, 0.6).
narrative_ontology:measurement(q95_prog_be_t600, quran_9_5_scope__progressive_synthesis, base_extractiveness, 600, 0.4).
narrative_ontology:measurement(q95_prog_be_t900, quran_9_5_scope__progressive_synthesis, base_extractiveness, 900, 0.25).
narrative_ontology:measurement(q95_prog_be_t1200, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1200, 0.15).
narrative_ontology:measurement(q95_prog_be_t1392, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1392, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(q95_prog_su_t0, quran_9_5_scope__progressive_synthesis, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(q95_prog_su_t100, quran_9_5_scope__progressive_synthesis, suppression_requirement, 100, 0.8).
narrative_ontology:measurement(q95_prog_su_t300, quran_9_5_scope__progressive_synthesis, suppression_requirement, 300, 0.6).
narrative_ontology:measurement(q95_prog_su_t600, quran_9_5_scope__progressive_synthesis, suppression_requirement, 600, 0.45).
narrative_ontology:measurement(q95_prog_su_t900, quran_9_5_scope__progressive_synthesis, suppression_requirement, 900, 0.35).
narrative_ontology:measurement(q95_prog_su_t1200, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1200, 0.3).
narrative_ontology:measurement(q95_prog_su_t1392, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1392, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__progressive_synthesis, 0.08).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% This constraint story decomposes the kernel quran_9_5_scope into three readings with distinct ε values. abrogating_universal: high ε (universal offensive jihad extracts from Muslims and non-Muslims). contextual_defensive: moderate ε (defensive coordination with some extraction via treaty enforcement). progressive_synthesis: low ε (verse expired, ethical trajectory supersedes). The ε-invariance principle requires separate stories because the same verse label covers structurally distinct claims with different extraction profiles, failure modes, and stakeholder sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__progressive_synthesis, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
