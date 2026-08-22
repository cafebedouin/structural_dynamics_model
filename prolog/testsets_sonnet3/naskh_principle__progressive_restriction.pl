% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Naskh as Progressive Restriction (Divine Pedagogy Reading)
 *   domain: religious/legal/hermeneutic
 *
 * SUMMARY:
 *   Across several legal domains (alcohol consumption, the ethics of war,
 *   marital and inheritance permissions, treatment of captives), the Quran
 *   contains verses revealed at different points that appear to move from
 *   more permissive rulings to more restrictive ones on the same subject. The
 *   progressive-restriction reading resolves the appearance of contradiction
 *   by framing the sequence as pedagogical: God gradually prepared the early
 *   community for a stricter final standard rather than either canceling the
 *   earlier text (abrogation) or leaving both rulings simultaneously
 *   operative in different contexts (harmonization). This reading is taught
 *   in many seminaries as settled method and used to justify treating the
 *   temporally later, more restrictive verse as binding law, with the earlier
 *   verse retained in recitation but treated as historically instructive
 *   rather than legally operative today.
 *
 * KEY AGENTS:
 *   - restrictive_school_jurists: Primary agenda-setter (institutional/arbitrage) — administers and teaches the reading, issues rulings built on it
 *   - evolutionary_hermeneutics_scholars: Beneficiary (organized/mobile) — gains academic and da'wah legitimacy from the developmental narrative
 *   - permissive_text_practitioners: Primary target (powerless/trapped) — loses legal standing for practices grounded in earlier verses
 *   - minority_madhhab_adherents: Secondary target (moderate/constrained) — traditions built on earlier verses lose comparative legitimacy
 *   - contextual_harmonization_scholars: Excluded rival reading (organized/mobile) — structurally must be wrong for this reading to cohere
 *   - quranic_text_corpus: Analytical observer — the fixed shared text every reading interprets differently
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.52).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.58).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.52).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Naskh as Progressive Restriction (Divine Pedagogy Reading)").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "religious/legal/hermeneutic").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '72958274-9a57-4fea-af62-a170f85df7ba').
narrative_ontology:cs_kernel_codification('72958274-9a57-4fea-af62-a170f85df7ba', distributed).
narrative_ontology:cs_authority_grounding('72958274-9a57-4fea-af62-a170f85df7ba', lineage).
narrative_ontology:cs_interpretation_layer_present('72958274-9a57-4fea-af62-a170f85df7ba').
narrative_ontology:cs_reading_relation('72958274-9a57-4fea-af62-a170f85df7ba', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('72958274-9a57-4fea-af62-a170f85df7ba', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_axiom('72958274-9a57-4fea-af62-a170f85df7ba', foundational, permissive_verses_are_transitional_accommodation).
narrative_ontology:cs_axiom_status(permissive_verses_are_transitional_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('72958274-9a57-4fea-af62-a170f85df7ba', permissive_verses_are_transitional_accommodation, theological).
narrative_ontology:cs_axiom('72958274-9a57-4fea-af62-a170f85df7ba', foundational, later_restriction_expresses_final_divine_intent).
narrative_ontology:cs_axiom_status(later_restriction_expresses_final_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('72958274-9a57-4fea-af62-a170f85df7ba', later_restriction_expresses_final_divine_intent, theological).
narrative_ontology:cs_axiom('72958274-9a57-4fea-af62-a170f85df7ba', secondary, textual_retention_without_operative_force).
narrative_ontology:cs_axiom_status(textual_retention_without_operative_force, holdable).
narrative_ontology:cs_axiom_grounding('72958274-9a57-4fea-af62-a170f85df7ba', textual_retention_without_operative_force, conventional).
narrative_ontology:cs_reference_frame('72958274-9a57-4fea-af62-a170f85df7ba', revelatory_sequence_as_moral_pedagogy).
narrative_ontology:cs_drift_state('72958274-9a57-4fea-af62-a170f85df7ba', contemporary_legal_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('72958274-9a57-4fea-af62-a170f85df7ba', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, restrictive_school_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, evolutionary_hermeneutics_scholars).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, permissive_text_practitioners).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, minority_madhhab_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and teach the pedagogical-progression reading in seminaries and fatwa councils, treating the latest restrictive verse on a topic as final divine intent. They administer curricula, certify scholars, and issue rulings that treat earlier permissive verses as superseded accommodations. Their institutional standing depends on this reading being taught as settled method.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, restrictive_school_jurists, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, restrictive_school_jurists, beneficiary).

% Build academic and da'wah careers on the pedagogical-development model of revelation, using it to explain apparent contradictions to modern and comparative audiences. They gain intellectual legitimacy and platform from the reading's coherence and are not bound to any single jurisdiction's enforcement of it.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, evolutionary_hermeneutics_scholars, beneficiary,
    organized, generational, mobile, continental).

% Individuals or communities who continue practices grounded in earlier permissive verses (certain commercial, marital, or social arrangements) find their practice declared obsolete accommodation rather than valid law. They cannot appeal to the earlier verse's continuing force because the progressive-restriction reading treats it as a transitional stage already superseded pedagogically, not as coexisting law they could still choose.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, permissive_text_practitioners, payer,
    powerless, biographical, trapped, local).

% Belong to legal traditions that historically permitted practices later restricted verses are read to close off. Under this reading their positions are recast as arrested at an earlier pedagogical stage rather than as legitimate contextual applications, weakening their standing in interfaith and intra-Islamic legal dialogue and in state legal codes drawing on majority readings.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, minority_madhhab_adherents, payer,
    moderate, generational, constrained, regional).

% Hold that every verse remains valid within its situational context and object to treating permissive verses as merely transitional. They are marginalized in curricula dominated by the progression narrative, which structurally needs the permissive verses to be superseded stages rather than standing, context-bound law.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextual_harmonization_scholars, excluded,
    organized, generational, mobile, continental).

% The fixed textual corpus itself, containing both the earlier permissive and later restrictive verses; it does not adjudicate between readings but is the shared object every reading interprets.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, quranic_text_corpus, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(naskh_principle__progressive_restriction, quranic_text_corpus).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent developmental narrative that resolves apparent tension between permissive and restrictive verses on the same topic without declaring any verse false or removed from the canon, preserving textual integrity while yielding one operative ruling.
% TRANSFER_FUNCTION: Moves interpretive authority and legal legitimacy from communities and practitioners who rely on earlier permissive verses to the jurists and institutions who administer the 'final stage' restrictive ruling; moves standing away from minority legal traditions toward the schools that control the progression narrative.
% ABSENT_VOICES: Contextual-harmonization scholars, who hold that permissive verses remain independently valid within their revelatory context, are structurally sidelined — the progressive-restriction reading requires their premise to be false for its own coherence, so their objection is argued against rather than incorporated.
% DISAPPEARANCE_RATIONALE: If the pedagogical-progression reading were abandoned, communities currently barred from practices grounded in earlier permissive verses could reassert those verses as still-valid law (per contextual harmonization) or the ruling could instead be handled through classical chronological abrogation with different textual criteria — either way, current fatwa councils' authority over 'final stage' determinations would lose its interpretive basis and legal practice in several areas would reopen.
% FOUNDING_PROBLEM: Early Muslim jurists needed to explain why the Quran contains verses on the same topic (alcohol, warfare conduct, slavery, marital permissions) that appear to move from more permissive to more restrictive over the revelatory timeline, without asserting God changed His mind or that parts of the revealed text became false.
% FOUNDING_PROBLEM_CORROBORATION: Restrictive-school jurists and evolutionary hermeneutics scholars attest the pedagogical model remains theologically necessary and doctrinally live. Contextual-harmonization scholars and comparative historians of Islamic law, outside the beneficiary set, attest the underlying interpretive problem is better handled by situational specification and that the progression narrative now functions primarily to consolidate one school's rulings as final rather than to solve a live exegetical puzzle.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) is moderate: the reading does not seize material resources but reallocates interpretive authority and legal standing away from practitioners of earlier-verse practices toward the jurists who administer the 'final stage' determination. Suppression (0.58) reflects that the reading requires actively arguing down the rival contextual-harmonization premise and treating continued reliance on permissive verses as a category error (misunderstanding pedagogy as law) rather than a live legal option — this is coercive in the sense of foreclosing an interpretive path, not merely disagreeing with it. Theater ratio is low-moderate (0.28) because the exegetical and pedagogical work is substantively real scholarship, not empty performance; the theatrical component is the periodic re-litigation of 'why the earlier verse doesn't apply now' functioning more to reassure adherents than resolve genuine doctrinal uncertainty.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat this is a rope: a coherent, coordination-preserving solution to an apparent textual tension that avoids the theologically costly move of declaring scripture internally contradictory or partially void. From the payer seat (practitioners and minority traditions whose legal grounding is redefined as superseded pedagogy) it operates as active extraction of interpretive legitimacy, requiring ongoing doctrinal enforcement to hold. The engine should compute this divergence from the structural beneficiary/victim/enforcement data rather than from the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Restrictive-school jurists and evolutionary hermeneutics scholars are declared beneficiaries: the reading is the basis of their institutional and intellectual authority, giving them low derived directionality (near-beneficiary). Permissive-text practitioners are declared victims with trapped exit — a local community cannot simply relocate to a jurisdiction recognizing the earlier verse as still operative, giving them high derived directionality (near-target). Minority madhhab adherents have constrained rather than trapped exit — they retain some standing within their own tradition but lose ground in cross-tradition legal and political contexts, placing them at moderate-high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (explaining the permissive-to-restrictive sequence without declaring scripture self-contradictory) remains genuinely live as an exegetical question — this is not a pure zombie mandate. What is contested is whether THIS SPECIFIC resolution (pedagogical progression, with the earlier verse's operative force treated as closed) is still the best-fit resolution, or whether it has hardened into a device that forecloses contextual-harmonization readings mainly because it now underwrites specific jurists' institutional authority. The tangled_rope classification captures this: real coordination function (resolving apparent contradiction, preserving textual integrity) plus active, ongoing extraction (redefining minority and historical practices as obsolete) sustained by enforcement (fatwa authority, curricular gatekeeping).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogy_vs_abrogation_vs_context_indeterminacy,
    'Is the correct account of sequential permissive-to-restrictive Quranic verses pedagogical progression (this reading), chronological abrogation (classical_abrogation), or contextual specification with no supersession at all (contextual_harmonization)?',
    'No empirical resolution mechanism exists; the three readings rest on differing premises about revelation, textual finality, and the nature of divine communication that are internal to Islamic theological method (usul al-fiqh) and cannot be adjudicated by external evidence. Resolution, if any, occurs through scholarly consensus (ijma) shifts or sustained juristic argument, not discovery.',
    'Under the pedagogical-progression reading, permissive-text practitioners and minority madhhab adherents bear extraction as their positions are recast as arrested development. Under contextual_harmonization, the same practitioners would be treated as holding valid, context-bound law with no supersession, eliminating their victim status entirely. The choice of reading is dispositive for who is classified as beneficiary versus victim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pedagogy_vs_abrogation_vs_context_indeterminacy, conceptual, 'The three sibling readings of the naskh kernel are not resolvable by external evidence; each is a live, internally coherent theological commitment with different victim sets.').

omega_variable(
    pedagogy_versus_permanent_closure,
    'Does the pedagogical-progression reading logically require that the earlier permissive stage is PERMANENTLY closed for all future communities, or only that it was appropriately superseded for the specific 7th-century Arabian context that received it?',
    'Close textual and historical analysis of whether the Quran or hadith corpus frames the restriction as context-specific (tied to the maturity of that particular early community) or as a universal, trans-historical closure. Comparative analysis across the different topics (alcohol vs. warfare vs. marital law) where the pattern is invoked, since the reading may not generalize uniformly.',
    'If the restriction is context-specific rather than universally closing, contemporary communities in different social conditions could argue the ''permissive stage'' pedagogy applies to their own context, substantially reducing the extraction currently borne by permissive-text practitioners. If universal, current jurist authority over the ''final stage'' is more strongly grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogy_versus_permanent_closure, conceptual, 'Whether the pedagogical closure this reading asserts is context-bound or trans-historically permanent is itself under-determined within the reading''s own premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__progressive_restriction, theater_ratio, 20, 0.18).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__progressive_restriction, theater_ratio, 40, 0.21).
narrative_ontology:measurement(nask_tr_t60, naskh_principle__progressive_restriction, theater_ratio, 60, 0.24).
narrative_ontology:measurement(nask_tr_t80, naskh_principle__progressive_restriction, theater_ratio, 80, 0.26).
narrative_ontology:measurement(nask_tr_t100, naskh_principle__progressive_restriction, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nask_be_t20, naskh_principle__progressive_restriction, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(nask_be_t40, naskh_principle__progressive_restriction, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(nask_be_t60, naskh_principle__progressive_restriction, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(nask_be_t80, naskh_principle__progressive_restriction, base_extractiveness, 80, 0.5).
narrative_ontology:measurement(nask_be_t100, naskh_principle__progressive_restriction, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(nask_su_t20, naskh_principle__progressive_restriction, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(nask_su_t40, naskh_principle__progressive_restriction, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(nask_su_t60, naskh_principle__progressive_restriction, suppression_requirement, 60, 0.53).
narrative_ontology:measurement(nask_su_t80, naskh_principle__progressive_restriction, suppression_requirement, 80, 0.56).
narrative_ontology:measurement(nask_su_t100, naskh_principle__progressive_restriction, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__contextual_harmonization).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the naskh kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: classical_abrogation (later verses void earlier ones), contextual_harmonization (all verses remain independently valid within context), and progressive_restriction (this story — permissive-to-restrictive movement is pedagogical development, not invalidation or coexistence). The three share the same underlying textual corpus but diverge sharply in beneficiary/victim structure and in which communities' legal practices are treated as currently operative. They are linked here rather than merged because averaging their epsilon values would misrepresent all three; each reading is a live, internally coherent theological commitment with its own extraction profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
