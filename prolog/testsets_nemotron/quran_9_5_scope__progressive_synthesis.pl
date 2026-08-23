% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
 *   human_readable: Progressive Synthesis Reading of Q.9:5 — Time-Bound Directive Superseded by Ethical Trajectory
 *   domain: religious/hermeneutic/political_theology
 *
 * SUMMARY:
 *   This constraint story models the progressive synthesis reading of Quran
 *   9:5 ('the sword verse') as a scaffold constraint — a transitional
 *   hermeneutic structure that enables Muslims to move from
 *   literalist-textualist authority toward historical-contextual ethics. The
 *   reading declares the verse's directive time-bound and superseded by the
 *   Quran's own ethical trajectory (pluralism, religious freedom, human
 *   dignity). It does not merely reinterpret; it removes the verse from
 *   active constraint space entirely. Beneficiaries are the actors building
 *   inclusive Islamic modernities; victims are the authority structures whose
 *   legitimacy depends on the verse's perpetual binding force. The constraint
 *   is a scaffold because it carries an implicit sunset: once the ethical
 *   trajectory is fully institutionalized, the reading's negating work is
 *   complete — it has no steady-state function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.08).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.12).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.08).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, scaffold).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Progressive Synthesis Reading of Q.9:5 — Time-Bound Directive Superseded by Ethical Trajectory").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "religious/hermeneutic/political_theology").

narrative_ontology:has_sunset_clause(quran_9_5_scope__progressive_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, '1c67a773-3908-406d-a3aa-d394f1174c60').
narrative_ontology:cs_kernel_codification('1c67a773-3908-406d-a3aa-d394f1174c60', fixed_text).
narrative_ontology:cs_authority_grounding('1c67a773-3908-406d-a3aa-d394f1174c60', lineage).
narrative_ontology:cs_interpretation_layer_present('1c67a773-3908-406d-a3aa-d394f1174c60').
narrative_ontology:cs_reading_relation('1c67a773-3908-406d-a3aa-d394f1174c60', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('1c67a773-3908-406d-a3aa-d394f1174c60', quran_9_5_scope__contextual_defensive, coexists_with).
narrative_ontology:cs_axiom('1c67a773-3908-406d-a3aa-d394f1174c60', foundational, quranic_ethical_trajectory_supersedes_literal_text).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_supersedes_literal_text, holdable).
narrative_ontology:cs_axiom_grounding('1c67a773-3908-406d-a3aa-d394f1174c60', quranic_ethical_trajectory_supersedes_literal_text, deontological).
narrative_ontology:cs_axiom('1c67a773-3908-406d-a3aa-d394f1174c60', secondary, historical_contextualization_as_hermeneutic_necessity).
narrative_ontology:cs_axiom_status(historical_contextualization_as_hermeneutic_necessity, holdable).
narrative_ontology:cs_axiom_grounding('1c67a773-3908-406d-a3aa-d394f1174c60', historical_contextualization_as_hermeneutic_necessity, instrumental).
narrative_ontology:cs_reference_frame('1c67a773-3908-406d-a3aa-d394f1174c60', classical_tafsir_authority).
narrative_ontology:cs_drift_state('1c67a773-3908-406d-a3aa-d394f1174c60', contemporary_pluralist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1c67a773-3908-406d-a3aa-d394f1174c60', '2026-08-14T12:00:00Z').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, progressive_muslim_intellectuals).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, interfaith_dialogue_practitioners).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, human_rights_advocates_in_muslim_contexts).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, literalist_legal_traditionalists).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, salafi_jihadi_ideologues).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, quranic_ethical_trajectory_supersedes_literalist_application).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, historical_contextualization_as_hermeneutic_principle).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, progressive_revelation_within_scripture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and disseminate the progressive synthesis reading through academic work, public theology, and community engagement. Their authority derives from scholarly credentials and moral legitimacy within progressive Muslim spaces. They face institutional marginalization from traditional seminaries but possess exit options into secular academia and transnational civil society networks.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, progressive_muslim_intellectuals, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, progressive_muslim_intellectuals, beneficiary).

% Gain hermeneutic cover for inclusive citizenship models, religious freedom frameworks, and pluralist legal orders in Muslim-majority contexts. The reading's acceptance reduces theological friction between Islamic normativity and universal human rights instruments. No exit needed — they are the structural beneficiaries of the reading's diffusion.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks, beneficiary,
    institutional, civilizational, arbitrage, global).

% Use the reading as a bridge for theological dialogue with non-Muslim communities, demonstrating that classical 'sword verse' interpretations are not binding on contemporary Muslims. Their work gains credibility and funding when this reading is institutionally recognized. Exit is easy — they can pivot to other bridge-building frameworks.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, interfaith_dialogue_practitioners, beneficiary,
    moderate, biographical, mobile, regional).

% Deploy the reading in legal advocacy, constitutional reform, and rights litigation to counter literalist invocations of Q.9:5. Their constraint is political: in authoritarian or theocratic settings, advancing this reading carries personal risk. Exit means silence or exile — constrained but not trapped.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, human_rights_advocates_in_muslim_contexts, beneficiary,
    moderate, biographical, constrained, national).

% Classical madhhab institutions, state-appointed ulama councils, and seminary networks whose interpretive authority rests on textual fixity and the permanence of legal rulings. The progressive synthesis reading erodes their claim to be the exclusive custodians of scriptural meaning. Exit would require abandoning the epistemic framework that constitutes their institutional identity — they are identity-locked to the permanence of the text's legal force.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_authority_structures, payer,
    institutional, generational, identity_locked, global).

% Scholars and jurists who build careers on the fixity of hudud and jihad rulings. Their professional standing, publishing records, and discipleship networks depend on the verse's ongoing binding force. The reading directly threatens their epistemic capital. Exit means intellectual rupture — they cannot adopt the reading without negating their life's work.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, literalist_legal_traditionalists, payer,
    organized, biographical, identity_locked, global).

% Non-state actors who mobilize Q.9:5 as divine authorization for offensive warfare. The reading's acceptance in mainstream discourse delegitimizes their recruitment narrative and theological claim. They are trapped — the reading's success destroys their legitimating mythology, but they cannot exit the ideology without existential collapse of their movement's self-understanding.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, salafi_jihadi_ideologues, payer,
    organized, immediate, trapped, global).

% Academic specialists in tafsir, usul al-fiqh, and Quranic studies who analyze the reading's methodological coherence, historical plausibility, and reception history. They neither collect nor pay — they map the constraint's structural dynamics across the field.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, quranic_hermeneutics_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutic framework that allows Muslims to affirm the Quran's ethical authority while rejecting violent literalism, enabling participation in pluralist political orders without theological rupture.
% TRANSFER_FUNCTION: Moves interpretive authority from fixed-text literalism toward historical-contextual ethics; transfers legitimacy from classical juristic gatekeepers to progressive intellectuals and rights advocates; redirects the verse's coercive potential from polytheist submission to internal theological renewal.
% ABSENT_VOICES: Classical tafsir tradition (Tabari, Ibn Kathir, Qurtubi) and the living madhhab authorities who inherit their interpretive mantle — they would object that the reading severs the verse from its established legal effects (jihad fi sabilillah, treatment of mushrikin) but are excluded from the progressive synthesis conversation by epistemic incommensurability.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, progressive Muslim intellectuals would lose their primary hermeneutic tool for engaging pluralism; secular frameworks would face increased theological friction; literalist authorities would consolidate interpretive monopoly; jihadi recruiters would reclaim the verse unchallenged. The constraint actively structures the theological battlefield.
% FOUNDING_PROBLEM: How can the Quran function as ethical guidance for Muslims in modernity when specific verses (like 9:5) appear to mandate violence and religious coercion incompatible with pluralist coexistence?
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested by the entire tradition of Islamic reformism from Muhammad Abduh and Rashid Rida through Fazlur Rahman, Abdullahi An-Na'im, and Amina Wadud — all outside the classical beneficiary structure. Contemporary Muslim-majority states' constitutional struggles (Tunisia 2014, Indonesia's Pancasila debates) corroborate that the founding problem remains live in political practice, not merely academic theory.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Low extractiveness (0.08) because the reading extracts nothing from those it governs — it releases them from a prior extraction. Low suppression (0.12) because it operates through persuasion and institutional diffusion, not coercion. Moderate theater (0.18) because some institutional performances (conferences, fatwa councils, academic citations) exceed functional necessity. Low accessibility collapse (0.25) because literalist readings remain fully available and widely held — the constraint does not close alternatives. Moderate resistance (0.42) from textualist authorities who correctly perceive the reading as an existential threat to their interpretive monopoly. The measurement series shows the constraint's power growing (extraction falling, suppression falling) as the ethical trajectory gains institutional purchase across the 20th century.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive seat, this is a liberation — the Quran's ethical core is rescued from literalist capture. From the textualist seat, this is a theft — the verse's clear meaning is dissolved by an alien hermeneutic. From the jihadi seat, this is an existential attack — their divine warrant is revoked. The engine computes these divergent seat types from the single constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive intellectuals are agenda-setters (d ~0.2) — they build and maintain the scaffold. Secular-pluralist frameworks are pure beneficiaries (d ~0.0) — they receive the hermeneutic gift without maintaining it. Textualist authorities are identity-locked payers (d ~0.95) — the reading dissolves the epistemic ground they stand on. Jihadi ideologues are trapped payers (d ~1.0) — the reading destroys their legitimating mythology with no exit. The engine will compute these directionalities from the structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading resolves a genuine mandatrophy: the classical jihad fi sabilillah framework (a tangled rope of coordination and extraction) has atrophied — its coordination function (ordering inter-communal relations) is obsolete, its extraction function (mobilizing violence) is destructive. The scaffold provides a transitional structure that acknowledges the verse's historical reality while refusing its contemporary binding force. Once the ethical trajectory is fully sedimented in law and culture, the scaffold dissolves — its mandate is resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_trajectory_identification,
    'Is the ''Quranic ethical trajectory'' a discoverable textual pattern or a constructive projection by modern readers?',
    'Inter-textual analysis of Meccan vs. Medinan surahs, chronological Quranic semantics, and reception history across 14 centuries — does the trajectory exist in the text''s diachronic structure or only in modern retrospective reading?',
    'If discovered, the reading claims textual fidelity; if projected, it admits creative hermeneutic intervention. The latter makes the scaffold''s sunset explicit — the reading is a bridge, not a recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_trajectory_identification, conceptual, 'Ontological status of the ethical trajectory that supersedes 9:5').

omega_variable(
    sunset_condition_verification,
    'What observable institutional state constitutes the scaffold''s sunset — when is the ethical trajectory ''fully sedimented''?',
    'Constitutional recognition of religious freedom in Muslim-majority states, madhhab institutional adoption of historical-contextual methodology, cessation of literalist 9:5 invocation in state fatwas and jihadi propaganda.',
    'Without a verifiable sunset condition, the scaffold risks becoming a piton — a permanent performative negation that outlives its transitional purpose.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_condition_verification, empirical, 'Operationalizing the scaffold''s implicit sunset clause').

omega_variable(
    progressive_synthesis_unity,
    'Is ''progressive synthesis'' a single coherent reading or a family of readings with different supersession mechanisms (historical-critical, maqasid-based, philosophical, experiential)?',
    'Map the methodological diversity within progressive Muslim intellectual production — do Fazlur Rahman''s double movement, An-Na''im''s secular mediation, and Wadud''s gender-just reading share a structural core or only a directional orientation?',
    'If a family, this constraint story oversimplifies; each variant would need its own story with distinct beneficiary/victim structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(progressive_synthesis_unity, conceptual, 'Internal coherence of the progressive synthesis reading').

omega_variable(
    committer_frame_irreducible_disagreement,
    'Does the kernel''s contestation reflect an irreducible disagreement about the verse''s ontology (what the verse IS) rather than its application (what the verse MEANS)?',
    'Analyze whether sibling readings can agree on the verse''s textual features while diverging on its normative status — if not, the kernel hosts an ontological fracture, not an interpretive dispute.',
    'If ontological, no hermeneutic bridge can resolve the contest; the constraint family represents incommensurable frameworks. The progressive synthesis reading''s claim to supersede rather than interpret would be structurally honest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_irreducible_disagreement, conceptual, 'Whether the kernel contestation is ontological or hermeneutic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q95ps_tr_t1900, quran_9_5_scope__progressive_synthesis, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(q95ps_tr_t1930, quran_9_5_scope__progressive_synthesis, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(q95ps_tr_t1960, quran_9_5_scope__progressive_synthesis, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(q95ps_tr_t1980, quran_9_5_scope__progressive_synthesis, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(q95ps_tr_t2000, quran_9_5_scope__progressive_synthesis, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(q95ps_tr_t2025, quran_9_5_scope__progressive_synthesis, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(q95ps_be_t1900, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(q95ps_be_t1930, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1930, 0.45).
narrative_ontology:measurement(q95ps_be_t1960, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(q95ps_be_t1980, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1980, 0.18).
narrative_ontology:measurement(q95ps_be_t2000, quran_9_5_scope__progressive_synthesis, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(q95ps_be_t2025, quran_9_5_scope__progressive_synthesis, base_extractiveness, 2025, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(q95ps_su_t1900, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(q95ps_su_t1930, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement(q95ps_su_t1960, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(q95ps_su_t1980, quran_9_5_scope__progressive_synthesis, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement(q95ps_su_t2000, quran_9_5_scope__progressive_synthesis, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(q95ps_su_t2025, quran_9_5_scope__progressive_synthesis, suppression_requirement, 2025, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__progressive_synthesis, 0.06).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, islamic_reform_historical_contextualization).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, muslim_minority_fiqh_citizenship).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, jihadi_recruitment_theological_warrant).

% DUAL FORMULATION NOTE:
% This constraint is the progressive_synthesis reading of the quran_9_5_scope kernel. The kernel hosts three structurally distinct constraints: (1) abrogating_universal — high extraction, universal scope, active enforcement; (2) contextual_defensive — moderate extraction, regional-historical scope, conditional enforcement; (3) progressive_synthesis — near-zero extraction, global-ethical scope, no enforcement. They share the verse as referent but instantiate different constraints with different ε, different stakeholder structures, different types. The progressive synthesis reading forecloses the abrogating_universal reading within any single coherent framework (one cannot hold both that 9:5 is eternally binding and that it is historically superseded), coexists with the contextual_defensive reading (different parties hold them simultaneously in contemporary discourse), and influences the contextual_defensive reading by raising the hermeneutic bar — contextual_defensive must now justify why historical context preserves binding force while ethical trajectory does not.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__progressive_synthesis, institutional, 0.95).
constraint_indexing:directionality_override(quran_9_5_scope__progressive_synthesis, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
