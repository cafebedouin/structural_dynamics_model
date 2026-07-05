% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Naskh: Chronological Abrogation of Quranic Verses
 *   domain: religious/legal/theological
 *
 * SUMMARY:
 *   Classical naskh doctrine holds that where two Quranic verses on the same
 *   legal or theological topic conflict, the verse revealed later in
 *   chronological order takes legal precedence, and the earlier verse's
 *   ruling is abrogated (though the earlier verse retains recitational and
 *   spiritual value). This reading depends on a specialized chronology
 *   apparatus (asbab al-nuzul, sira dating) administered by certified
 *   scholarship, and it produces a fixed supersession hierarchy that
 *   classical fiqh schools and state religious courts rely on for legal
 *   predictability. The number of verses classical scholars have claimed as
 *   abrogated has varied dramatically across centuries and schools — from
 *   single digits to several hundred — which is itself evidence that the
 *   chronological-supersession framework, while genuinely solving a
 *   coordination problem (a workable decision procedure for apparent
 *   contradictions), also concentrates interpretive authority in institutions
 *   that certify chronology and administer the resulting rulings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.52).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.61).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.52).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh: Chronological Abrogation of Quranic Verses").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/legal/theological").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, 'b040cc5b-9783-4596-aae9-845070b74fbf').
narrative_ontology:cs_kernel_codification('b040cc5b-9783-4596-aae9-845070b74fbf', formalized).
narrative_ontology:cs_authority_grounding('b040cc5b-9783-4596-aae9-845070b74fbf', lineage).
narrative_ontology:cs_interpretation_layer_present('b040cc5b-9783-4596-aae9-845070b74fbf').
narrative_ontology:cs_reading_relation('b040cc5b-9783-4596-aae9-845070b74fbf', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('b040cc5b-9783-4596-aae9-845070b74fbf', naskh_principle__progressive_restriction, influences).
narrative_ontology:cs_axiom('b040cc5b-9783-4596-aae9-845070b74fbf', foundational, later_revelation_legally_supersedes_earlier).
narrative_ontology:cs_axiom_status(later_revelation_legally_supersedes_earlier, holdable).
narrative_ontology:cs_axiom_grounding('b040cc5b-9783-4596-aae9-845070b74fbf', later_revelation_legally_supersedes_earlier, conventional).
narrative_ontology:cs_axiom('b040cc5b-9783-4596-aae9-845070b74fbf', secondary, abrogated_verse_retains_recitation_value_only).
narrative_ontology:cs_axiom_status(abrogated_verse_retains_recitation_value_only, holdable).
narrative_ontology:cs_axiom_grounding('b040cc5b-9783-4596-aae9-845070b74fbf', abrogated_verse_retains_recitation_value_only, conventional).
narrative_ontology:cs_reference_frame('b040cc5b-9783-4596-aae9-845070b74fbf', chronological_supersession_hierarchy).
narrative_ontology:cs_drift_state('b040cc5b-9783-4596-aae9-845070b74fbf', contemporary_quranic_studies_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b040cc5b-9783-4596-aae9-845070b74fbf', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_fiqh_schools).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, state_appointed_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, hadith_chronology_scholars).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, lay_readers_of_scripture).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, reformist_interpreters).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, minority_legal_schools_favoring_harmonization).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, chronological_supersession_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, asbab_al_nuzul_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit the abrogation-rulings lists (nasikh wa mansukh literature) that fix which verses control legal outcomes. They administer the interpretive hierarchy through fatwa councils, madrasa curricula, and codified fiqh manuals, and their institutional authority rests on being the certified adjudicators of which verse chronologically supersedes which.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_fiqh_schools, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Apply settled abrogation hierarchies in state courts and personal-status law, gaining predictable, appealable rulings and insulation from theological controversy. They can invoke a closed chronological ranking rather than adjudicate contested exegesis case by case.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, state_appointed_jurists, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, state_appointed_jurists, agenda_setter).

% Their specialized discipline (asbab al-nuzul, sira-based dating) is the load-bearing evidentiary apparatus the abrogation hierarchy depends on. Institutional demand for their expertise, and their scholarly standing, depend on the framework remaining the operative one.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, hadith_chronology_scholars, beneficiary,
    organized, generational, constrained, global).

% Encounter verses that appear to counsel patience, tolerance, or peaceable conduct and are told by certified scholarship that a later verse has legally nullified them, without independent means to verify chronology or contest the ruling. Their direct engagement with scripture is structurally subordinated to a specialist chronology they cannot access or adjudicate themselves.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_readers_of_scripture, payer,
    powerless, biographical, trapped, global).

% Argue for contextual or thematic readings that would preserve the legal force of earlier verses; face marginalization, accusations of heterodoxy, and exclusion from state religious authority structures when their harmonizing readings conflict with the settled abrogation lists.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, reformist_interpreters, payer,
    moderate, biographical, constrained, national).

% Historically maintained smaller lists of abrogated verses or rejected large portions of the classical naskh catalogue, but were marginalized as the dominant Sunni schools consolidated a maximalist abrogation framework; their narrower readings survive mainly in specialist literature, not in applied law.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, minority_legal_schools_favoring_harmonization, excluded,
    moderate, generational, constrained, regional).

% Examine the historical development of the abrogation lists, noting that the number of claimed abrogated verses has varied enormously across centuries (from under 10 to over 500), and analyze whether chronological dating claims are independently verifiable or retrojected to support particular legal conclusions.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contemporary_quranic_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, diffuse).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable rule for resolving apparent legal contradictions between Quranic verses: when two rulings conflict, the later-revealed one controls, giving courts and jurists a tractable decision procedure instead of open-ended case-by-case reconciliation.
% TRANSFER_FUNCTION: Moves interpretive authority and legal certainty toward institutions that certify chronology and administer the resulting rulings hierarchy, and away from individual readers, harmonizing exegetes, and minority schools whose contextual or thematic readings would otherwise carry independent legal weight.
% ABSENT_VOICES: Lay readers who encounter the abrogated verses directly and experience their apparent nullification have no forum to contest the chronological ruling; minority schools that historically resisted maximalist abrogation lists are excluded from contemporary state religious authority structures that adopted the dominant framework.
% DISAPPEARANCE_RATIONALE: If classical abrogation doctrine were abandoned overnight, fiqh manuals built on nasikh-mansukh hierarchies would require wholesale revision, state personal-status codes citing abrogated-verse rulings would need re-derivation from harmonizing or progressive-restriction frameworks, and the specialized asbab al-nuzul chronology discipline would lose its load-bearing legal function (though retaining historical/devotional interest) — a substantial institutional and doctrinal reorganization, not a null event.
% FOUNDING_PROBLEM: Early Muslim jurists faced verses on the same topics (wine, warfare, inheritance, qibla direction) that gave apparently different or conflicting practical instructions, revealed at different points during a 23-year revelation period responding to changing communal circumstances; a decision procedure was needed to determine which ruling governed practice.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Quranic studies scholars operating outside the classical fiqh institutions (including scholars within the Islamic tradition, e.g. proponents of the progressive-restriction and contextual-harmonization readings) attest that the chronological dating underlying many abrogation claims is not independently verifiable and that the size of the abrogation lists grew historically in ways that track juristic convenience as much as textual necessity; this corroboration comes from outside the beneficiary set (classical fiqh schools and hadith chronology scholars), who themselves regard the founding problem as still live and the doctrine as its necessary solution.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 by interval end): the doctrine performs real coordination work (a usable decision procedure exists) but that work is bundled with a genuine transfer of interpretive authority away from lay readers and minority schools toward institutions certifying chronology. Suppression is higher (0.61) because sustaining the hierarchy requires active exclusion of harmonizing and progressive-restriction readings from state-sanctioned legal authority — accessibility_collapse (0.58) reflects that once a verse is placed on a certified abrogation list, an ordinary reader has no practical route back to its legal force. Theater ratio is modest but rising (0.10 to 0.28) as the historical record shows abrogation lists growing well beyond what chronological evidence alone would support, suggesting some maintenance of the hierarchy has become performative certification of prior scholarly consensus rather than fresh chronological analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical fiqh schools and state jurists are structural beneficiaries: they collect the legal certainty and institutional authority the hierarchy provides, and their exit options are effectively arbitrage-grade (they can shift emphasis between abrogation claims as needed without losing institutional standing). Hadith chronology scholars co-benefit because their specialized discipline is the hierarchy's evidentiary backbone. Lay readers are trapped: they cannot independently verify chronological claims and have no forum to contest a ruling once certified. Reformist interpreters and minority harmonizing schools bear a targeted cost — their competing readings are excluded from applied law, which is precisely the enforcement the framework requires to remain operative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (irreconcilable practical instructions across a 23-year revelation) was genuinely live in the early community. Whether it remains live today, or whether contextual/thematic reading tools developed since have rendered the chronological-supersession procedure unnecessary for resolving apparent contradictions, is exactly the contested question the sibling readings dispute. Classifying this as tangled_rope rather than snare or rope reflects that the coordination function (a workable decision procedure) is real and was genuinely needed, while the persistence of an expanding abrogation list beyond independently verifiable chronology, and the exclusion of competing frameworks from state legal authority, constitutes the asymmetric extraction the tangled-rope classification requires enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chronology_verifiability,
    'Is the chronological ordering underlying specific abrogation claims independently verifiable from historical evidence, or is it substantially reconstructed to support a legal conclusion already favored on other grounds?',
    'Independent historical-critical dating of individual verses/surahs against non-scriptural evidence (early manuscripts, non-Muslim contemporaneous sources, internal textual markers) compared against the chronology asserted in classical asbab al-nuzul literature; divergence between independently dateable verses and traditionally assigned abrogation order would indicate retrojection.',
    'If chronology is substantially independently verifiable, the coordination function is more genuine and less extractive than authored here. If chronology is substantially retrojected to fit desired legal outcomes, the extraction component is understated and the classification should move toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chronology_verifiability, empirical, 'Whether classical abrogation chronology is independently verifiable or retrojected.').

omega_variable(
    naturalness_of_supersession_reading,
    'Is chronological supersession the naturally correct reading of apparent Quranic contradictions, or a constructed interpretive choice that happens to concentrate authority in certifying institutions?',
    'Comparative analysis of how the same apparently-conflicting verse pairs are resolved across the three sibling readings (classical_abrogation, contextual_harmonization, progressive_restriction) by scholars with no institutional stake in any one framework prevailing; convergence toward one reading among institutionally disinterested scholars would suggest greater naturalness.',
    'This ambiguity determines whether the beneficiary structure documented here reflects incidental byproduct of a correct reading or the reading''s actual selection mechanism. It bears directly on whether classical_abrogation is better modeled as tangled_rope (mixed genuine coordination and extraction, as authored) or should be reconsidered toward snare if the beneficiary capture is closer to definitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_supersession_reading, conceptual, 'Whether chronological-supersession is a natural or constructed resolution mechanism, and what that implies for institutional beneficiary capture.').

omega_variable(
    abrogation_list_variance,
    'Why has the number of Quranic verses claimed as abrogated varied so widely across classical scholars and centuries (from fewer than 10 to several hundred), and what does that variance imply about the doctrine''s evidentiary discipline?',
    'Historical survey of major abrogation-list compilations across centuries and schools, correlated against the legal or political contexts in which each compilation was produced.',
    'If list expansion correlates with legal-political convenience rather than new chronological evidence, this supports treating a portion of the observed extraction as accumulated rent rather than founding coordination cost, and would elevate the theater_ratio trajectory''s diagnostic weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_list_variance, empirical, 'What drives historical variance in the size of classical abrogation lists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__classical_abrogation, theater_ratio, 20, 0.14).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__classical_abrogation, theater_ratio, 40, 0.19).
narrative_ontology:measurement(nask_tr_t60, naskh_principle__classical_abrogation, theater_ratio, 60, 0.22).
narrative_ontology:measurement(nask_tr_t80, naskh_principle__classical_abrogation, theater_ratio, 80, 0.25).
narrative_ontology:measurement(nask_tr_t100, naskh_principle__classical_abrogation, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(nask_be_t20, naskh_principle__classical_abrogation, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(nask_be_t40, naskh_principle__classical_abrogation, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(nask_be_t60, naskh_principle__classical_abrogation, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(nask_be_t80, naskh_principle__classical_abrogation, base_extractiveness, 80, 0.5).
narrative_ontology:measurement(nask_be_t100, naskh_principle__classical_abrogation, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(nask_su_t20, naskh_principle__classical_abrogation, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(nask_su_t40, naskh_principle__classical_abrogation, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(nask_su_t60, naskh_principle__classical_abrogation, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(nask_su_t80, naskh_principle__classical_abrogation, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(nask_su_t100, naskh_principle__classical_abrogation, suppression_requirement, 100, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(naskh_principle__classical_abrogation, 0.12).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% This story is one of three constraints emitted from the naskh_principle kernel. classical_abrogation (this file) authors a fixed supersession hierarchy with named victims (lay readers, reformist interpreters, minority harmonizing schools) and computes as tangled_rope. contextual_harmonization and progressive_restriction are separate files with their own beneficiary/victim structures and their own epsilon values — per the epsilon-invariance principle, they are not alternate measurements of this constraint but structurally distinct constraints linked here for contamination-propagation and family-tracing purposes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__classical_abrogation, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
