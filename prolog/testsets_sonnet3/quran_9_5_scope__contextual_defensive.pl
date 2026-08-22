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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Contextual-Defensive Reading of Q9:5 (Sword Verse) — Treaty-Bound Warfare Doctrine
 *   domain: religious/legal/political theology
 *
 * SUMMARY:
 *   This story instantiates the contextual-defensive reading of the so-called
 *   'Sword Verse' (Q9:5) within the Islamic hermeneutic tradition — one of
 *   three structurally distinct readings of the same kernel text. This
 *   reading holds that the verse's martial language was addressed to specific
 *   treaty-breaking Meccan polytheist tribes circa 630 CE, that it does not
 *   abrogate the Quran's numerous peaceful and pluralistic verses, and that
 *   its scope is conditioned on prior treaty violation or aggression rather
 *   than religious status alone. It is authored here as its own constraint,
 *   independent of the abrogating-universal reading (constraint_id: to be
 *   authored separately, claiming universal offensive obligation and full
 *   abrogation of peaceful verses) and the progressive-synthesis reading
 *   (claiming the verse is a superseded time-bound directive). Each reading
 *   produces a materially different victim set, different beneficiaries, and
 *   a different epsilon — per the epsilon-invariance principle these are
 *   three constraints, not one constraint measured three ways.
 *
 * KEY AGENTS:
 *   - contextualist_jurists: Primary agenda-setters (institutional/identity_locked) — administer and defend the reading through fatwa and curricula
 *   - treaty_violating_combatant_factions: Bounded target of sanctioned force under this reading's own terms (moderate/constrained)
 *   - integrationist_muslim_majority_states: Primary beneficiary — grounds peaceful foreign policy and minority law in scripture (institutional/arbitrage)
 *   - religious_minorities_under_treaty_protection: Downstream beneficiary whose physical safety depends on this reading prevailing (powerless/trapped)
 *   - literalist_universalist_scholars and secular_progressive_reformers: Excluded competing readings within the same kernel dispute
 *   - counter_terrorism_and_security_analysts: Analytical observer tracking which reading dominates in a given jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.28).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.32).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.28).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Contextual-Defensive Reading of Q9:5 (Sword Verse) — Treaty-Bound Warfare Doctrine").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "religious/legal/political theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__contextual_defensive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, '55310a48-37d1-435e-8c0a-63b7a37c3340').
narrative_ontology:cs_kernel_codification('55310a48-37d1-435e-8c0a-63b7a37c3340', fixed_text).
narrative_ontology:cs_authority_grounding('55310a48-37d1-435e-8c0a-63b7a37c3340', lineage).
narrative_ontology:cs_interpretation_layer_present('55310a48-37d1-435e-8c0a-63b7a37c3340').
narrative_ontology:cs_reading_relation('55310a48-37d1-435e-8c0a-63b7a37c3340', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('55310a48-37d1-435e-8c0a-63b7a37c3340', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('55310a48-37d1-435e-8c0a-63b7a37c3340', foundational, treaty_condition_precedes_sanction).
narrative_ontology:cs_axiom_status(treaty_condition_precedes_sanction, holdable).
narrative_ontology:cs_axiom_grounding('55310a48-37d1-435e-8c0a-63b7a37c3340', treaty_condition_precedes_sanction, conventional).
narrative_ontology:cs_axiom('55310a48-37d1-435e-8c0a-63b7a37c3340', foundational, peaceful_verses_remain_uncancelled).
narrative_ontology:cs_axiom_status(peaceful_verses_remain_uncancelled, holdable).
narrative_ontology:cs_axiom_grounding('55310a48-37d1-435e-8c0a-63b7a37c3340', peaceful_verses_remain_uncancelled, conventional).
narrative_ontology:cs_reference_frame('55310a48-37d1-435e-8c0a-63b7a37c3340', classical_asbab_al_nuzul_contextualism).
narrative_ontology:cs_drift_state('55310a48-37d1-435e-8c0a-63b7a37c3340', post_colonial_interfaith_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('55310a48-37d1-435e-8c0a-63b7a37c3340', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, religious_minorities_under_treaty_protection).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, interfaith_coexistence_advocates).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, muslim_populations_in_pluralist_democracies).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_violating_combatant_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Islamic scholars and jurisprudential bodies who read Q9:5 through occasion-of-revelation (asbab al-nuzul) analysis, tying its scope to the specific Meccan polytheist tribes who broke the Hudaybiyyah treaty. They administer this reading through fatwa councils, seminary curricula, and interfaith declarations (e.g. A Common Word), and their institutional standing depends on the reading holding against both literalist-universalist and secular-abolitionist challengers.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, contextualist_jurists, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Under this reading, only parties who have broken a prior treaty or initiated aggression fall within the verse's sanctioned scope of warfare. They bear the reading's sanctioned use of force, but only conditionally — the reading requires their own prior violation as the triggering act, and the sanction lifts if they honor obligations, seek peace, or embrace protection (per the verse's own exemption clauses).
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_violating_combatant_factions, payer,
    moderate, immediate, constrained, regional).

% Governments and religious establishments in pluralist or minority-coexisting Muslim-majority contexts use this reading to ground foreign policy, minority-rights law, and diplomatic engagement in a textual doctrine of treaty-primacy and defensive-only warfare. It lets them present continuity with scripture while pursuing peaceful international relations and protecting non-Muslim citizens under a coexistence norm.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary,
    institutional, generational, arbitrage, national).

% Non-Muslim populations living under Muslim-majority governance benefit directly from a doctrine that reads the verse as excluding peaceful, treaty-abiding non-combatants from its scope. Their physical safety and legal standing depend materially on this reading prevailing over the abrogating-universal reading in the jurisdictions where they live; they have no independent capacity to adjudicate the hermeneutic dispute themselves.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, religious_minorities_under_treaty_protection, beneficiary,
    powerless, biographical, trapped, national).

% Scholars and movements holding the abrogating-universal reading (that 9:5 nullifies peaceful verses via naskh and establishes a standing offensive obligation) are structurally sidelined by this reading's institutional dominance in mainstream jurisprudential bodies and interfaith diplomacy. They would object that contextualism is a modern apologetic imposed on a text with a settled classical abrogation tradition, but their reading is treated as fringe or extremist in the venues where this reading is administered.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, literalist_universalist_scholars, excluded,
    organized, civilizational, identity_locked, global).

% Reformers holding the progressive-synthesis reading (that the verse is a time-bound directive superseded by an evolving ethical trajectory) find the contextual-defensive reading insufficiently emancipated from literal textual authority — it still treats the verse as binding law for a defined category of enemies rather than as historically superseded. They are often excluded from mainstream jurisprudential councils, which favor contextualism as the acceptable moderate position.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, secular_and_progressive_muslim_reformers, excluded,
    moderate, generational, constrained, global).

% State security services, academic Islamic-studies departments, and international bodies track which reading is dominant in a given jurisdiction's religious education and clerical training, because the abrogating-universal reading correlates with recruitment narratives used by militant groups. They neither benefit from nor pay into the doctrine's operation but rely on accurately identifying which reading a given institution actually teaches.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, counter_terrorism_and_security_analysts, observer,
    institutional, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__contextual_defensive, diffuse).
narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a textually-grounded basis for limiting religiously-sanctioned warfare to cases of prior treaty violation or aggression, coordinating expectations among Muslim jurists, states, and minority communities around a bounded, conditional doctrine of defensive conflict rather than open-ended religious war.
% TRANSFER_FUNCTION: Moves interpretive authority and moral legitimacy toward jurists and states who ground pluralistic coexistence in scripture, and away from movements claiming a standing universal-offensive mandate; moves physical security toward treaty-abiding minorities and away from any claim that their mere non-Muslim status places them within the verse's sanctioned target set.
% ABSENT_VOICES: Literalist-universalist scholars and progressive-synthesis reformers are both structurally marginalized in the venues (state religious councils, interfaith bodies, mainstream seminaries) where this reading is administered — the former as extremist, the latter as insufficiently orthodox — even though both hold coherent, textually-argued positions within the broader exegetical tradition.
% DISAPPEARANCE_RATIONALE: If the contextual-defensive reading lost institutional dominance, jurisdictions currently grounding minority protections and peaceful foreign policy in this exegesis would lose their scriptural warrant for that stance, opening space for the abrogating-universal reading to claim doctrinal primacy in clerical training, fatwa issuance, and state religious policy — with direct consequences for the legal and physical standing of religious minorities in affected states.
% FOUNDING_PROBLEM: How to reconcile Q9:5's harsh martial language, revealed amid specific treaty-breaking conflicts with Meccan polytheist tribes in 630 CE, with the Quran's numerous other verses counseling patience, coexistence, and non-compulsion in religion — without either discarding the verse or letting it swallow the rest of the text.
% FOUNDING_PROBLEM_CORROBORATION: Classical exegetes (al-Tabari, Ibn Kathir in qualified form) and contemporary bodies such as Al-Azhar and the Amman Message attest that context-bound reading has deep roots predating modern apologetics; however, Salafi-jihadist ideologues and some classical abrogation theorists (following a stricter naskh doctrine) dispute that the founding problem is resolved this way, arguing the contextualist reading is a post-colonial-era accommodation rather than the historically dominant position. No fully disinterested third party outside the exegetical-political dispute itself corroborates either side conclusively — the corroboration is contested along the same lines as the reading itself.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low-moderate (0.28) because, by this reading's own terms, the sanctioned use of force is narrowly conditioned on prior treaty violation and lifts once the condition is no longer met — the reading itself builds in an exit for anyone who ceases aggression. Suppression (0.32) captures the real but bounded coercive content: even a defensively-scoped doctrine of sanctioned warfare is coercive toward the parties it targets, and the reading's institutional dominance in state religious apparatus does exercise real suppressive force against rival exegetical claims (both literalist-universalist and progressive). Theater ratio is low (0.22) — the doctrinal apparatus (fatwa councils, seminary curricula, interfaith declarations) performs real interpretive and diplomatic work rather than empty ritual, though the rising trajectory reflects increasing performative deployment of the reading in 20th/21st century interfaith diplomacy and counter-extremism messaging. Accessibility collapse is moderate (0.4) — this is a genuinely contested textual question and the classical tradition itself contains real interpretive plurality, so alternatives have not collapsed the way they would for a settled empirical or logical matter. Resistance is comparatively high (0.55) because this reading is actively contested by both a well-organized literalist-universalist tradition with deep classical roots and a progressive-reformist tradition — it must be actively defended, not merely assumed.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist states and treaty-protected minorities sit near the beneficiary end: the reading's prevalence directly underwrites their legal and physical security, and they exercise no coercive machinery to sustain it themselves — they receive its protective effect. Treaty-violating combatant factions sit as the reading's bounded payer set, but critically their entry into that set is conditioned on their own prior act (treaty violation), which is why this reading's extraction is authored much lower than the abrogating-universal sibling would author for the same nominal verse — the victim set here is narrow and conditional, not the entire non-Muslim world. Contextualist jurists hold agenda-setting power (institutional, identity_locked) because their scholarly and institutional identity is substantially constituted by successfully defending this reading against both siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling a specific 7th-century martial verse with the Quran's broader ethical and coexistence-oriented content — remains genuinely contested rather than settled, which is why founding_problem_status is authored as 'contested' rather than 'dead.' This reading does not claim the problem has vanished (which would risk mandatrophy in the direction of an obsolete doctrine persisting past its function); it claims the problem is best solved by contextual bounding rather than universal abrogation or historical supersession. The corpus should not read this story as adjudicating which of the three kernel readings is correct — it reports the contextual-defensive reading's own structural self-account, including its institutional stakes and its genuinely contested corroboration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asbab_al_nuzul_epistemic_status,
    'Does occasion-of-revelation (asbab al-nuzul) methodology carry sufficient historical-critical warrant to bound a verse''s legal scope against its plain textual universality, or is contextualism a retrospective interpretive move driven by modern political and diplomatic needs rather than classical exegetical consensus?',
    'Comparative textual-historical analysis of classical tafsir corpus (al-Tabari, al-Qurtubi, Ibn Kathir) tracing whether contextual bounding of 9:5 predates the 19th/20th century apologetic and reformist movements, cross-referenced against the historical timeline of when abrogation (naskh) claims for this verse first appear versus when contextualist counter-readings emerge.',
    'If contextualism is shown to be a genuinely ancient minority position within the classical tradition, this reading''s claim to textual fidelity strengthens considerably. If contextualism is shown to be predominantly a 19th-20th century development responding to colonial and post-colonial political pressures, this reading''s beneficiary structure (integrationist states, minority protections) would appear more clearly as motivating the interpretation rather than following from it — which would not make the reading false, but would relocate its justification from historical-textual grounds to normative-political ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asbab_al_nuzul_epistemic_status, empirical, 'Whether contextual bounding of Q9:5 has deep classical roots or is substantially a modern development.').

omega_variable(
    kernel_contest_location,
    'This constraint is one of three readings of the quran_9_5_scope kernel (abrogating_universal, contextual_defensive, progressive_synthesis). Where precisely does the disagreement between the abrogating_universal and contextual_defensive readings locate? Is it a disagreement about historical fact (what the occasion of revelation actually was), about legal method (whether naskh/abrogation is the correct interpretive tool at all), or about theological commitment (what kind of book the Quran is taken to be)?',
    'This is not resolvable by data alone — it requires mapping which specific interpretive premises each reading''s proponents would identify as load-bearing, likely through close comparative analysis of the classical abrogation (naskh) literature versus the classical asbab al-nuzul literature to see where they actually diverge versus where they are talking past each other.',
    'If the disagreement is primarily about historical fact, it is in principle empirically narrowable. If it is primarily about legal method (is naskh a valid tool, and does it apply here), it is a jurisprudential dispute internal to Islamic legal theory. If it is primarily theological (nature of scriptural authority), it may be irreducible to textual-historical argument at all — which would mean the three kernel readings are not converging positions in an ongoing scholarly debate but stable, coexisting theological commitments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_location, conceptual, 'Where the structural disagreement among the three kernel readings is actually located: historical fact, legal method, or theological commitment.').

omega_variable(
    victim_set_boundary_precision,
    'The contextual_defensive reading bounds the victim set to ''treaty-violating combatant factions,'' but classical and contemporary jurists disagree sharply on how narrowly or broadly ''treaty violation'' should be construed — does it require an explicit, documented breach of a specific treaty, or can it be extended analogically to any polytheist group deemed to be in a state of ongoing hostility?',
    'Survey of contemporary fiqh councils'' formal rulings (Al-Azhar, OIC fiqh academy, national fatwa councils) on what evidentiary standard is required to classify a group as treaty-violating for purposes of applying this doctrine.',
    'A narrow construction (documented, specific breach) keeps the reading''s extraction low and its victim set genuinely small, consistent with the authored epsilon of 0.28. A loose analogical construction would functionally expand the victim set toward something closer to the abrogating_universal reading''s scope while retaining the contextualist label, which would mean the reading''s classification is unstable across its own practitioner community — some administer it narrowly, others loosely, under the same doctrinal name.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_precision, conceptual, 'How narrowly or broadly this reading''s own proponents construe the boundary of ''treaty violation'' that triggers the sanctioned-force clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 632, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t632, quran_9_5_scope__contextual_defensive, theater_ratio, 632, 0.1).
narrative_ontology:measurement(qura_tr_t900, quran_9_5_scope__contextual_defensive, theater_ratio, 900, 0.12).
narrative_ontology:measurement(qura_tr_t1250, quran_9_5_scope__contextual_defensive, theater_ratio, 1250, 0.14).
narrative_ontology:measurement(qura_tr_t1800, quran_9_5_scope__contextual_defensive, theater_ratio, 1800, 0.16).
narrative_ontology:measurement(qura_tr_t1950, quran_9_5_scope__contextual_defensive, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(qura_tr_t2001, quran_9_5_scope__contextual_defensive, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(qura_tr_t2024, quran_9_5_scope__contextual_defensive, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t632, quran_9_5_scope__contextual_defensive, base_extractiveness, 632, 0.15).
narrative_ontology:measurement(qura_be_t900, quran_9_5_scope__contextual_defensive, base_extractiveness, 900, 0.2).
narrative_ontology:measurement(qura_be_t1250, quran_9_5_scope__contextual_defensive, base_extractiveness, 1250, 0.22).
narrative_ontology:measurement(qura_be_t1800, quran_9_5_scope__contextual_defensive, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(qura_be_t1950, quran_9_5_scope__contextual_defensive, base_extractiveness, 1950, 0.26).
narrative_ontology:measurement(qura_be_t2001, quran_9_5_scope__contextual_defensive, base_extractiveness, 2001, 0.27).
narrative_ontology:measurement(qura_be_t2024, quran_9_5_scope__contextual_defensive, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t632, quran_9_5_scope__contextual_defensive, suppression_requirement, 632, 0.15).
narrative_ontology:measurement(qura_su_t900, quran_9_5_scope__contextual_defensive, suppression_requirement, 900, 0.18).
narrative_ontology:measurement(qura_su_t1250, quran_9_5_scope__contextual_defensive, suppression_requirement, 1250, 0.2).
narrative_ontology:measurement(qura_su_t1800, quran_9_5_scope__contextual_defensive, suppression_requirement, 1800, 0.22).
narrative_ontology:measurement(qura_su_t1950, quran_9_5_scope__contextual_defensive, suppression_requirement, 1950, 0.27).
narrative_ontology:measurement(qura_su_t2001, quran_9_5_scope__contextual_defensive, suppression_requirement, 2001, 0.3).
narrative_ontology:measurement(qura_su_t2024, quran_9_5_scope__contextual_defensive, suppression_requirement, 2024, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__contextual_defensive, 0.1).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three members of the quran_9_5_scope kernel family. abrogating_universal claims the verse nullifies peaceful verses and establishes standing universal offensive obligation (much higher authored epsilon, broader victim set encompassing all non-Muslim non-combatants historically construed as targets). progressive_synthesis claims the verse is a superseded time-bound directive with no binding legal force today (lower authored epsilon than this reading, no operative victim set at all since the verse is read as historically closed). This reading (contextual_defensive) sits structurally between them: it retains the verse as operative law but narrows its scope to a conditional, treaty-violation-triggered category. Each story carries its own stable epsilon under the epsilon-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
