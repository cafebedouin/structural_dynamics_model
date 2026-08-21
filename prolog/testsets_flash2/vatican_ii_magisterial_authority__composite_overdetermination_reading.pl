% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority: Composite Overdetermination Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint models Vatican II's magisterial authority as a composite,
 *   overdetermined text encoding incompatible ecclesiological visions through
 *   ambiguous compromise formulations. This reading asserts that the
 *   Council's texts were deliberately crafted to achieve supermajority votes
 *   by allowing both 'continuity' and 'rupture' interpretations to coexist
 *   within the same documents. The true locus of authority thus shifts to
 *   hermeneutical control, leading to structural implementation divergence
 *   rather than a unified vision. The 10-12% rejection votes on key documents
 *   are seen as a signal of unresolved theological incompatibility embedded
 *   in the final texts, not merely minor dissent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'eb820d33-6a99-4529-9b27-ae3f60572802').
narrative_ontology:cs_kernel_codification('eb820d33-6a99-4529-9b27-ae3f60572802', fixed_text).
narrative_ontology:cs_authority_grounding('eb820d33-6a99-4529-9b27-ae3f60572802', lineage).
narrative_ontology:cs_interpretation_layer_present('eb820d33-6a99-4529-9b27-ae3f60572802').
narrative_ontology:cs_reading_relation('eb820d33-6a99-4529-9b27-ae3f60572802', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb820d33-6a99-4529-9b27-ae3f60572802', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('eb820d33-6a99-4529-9b27-ae3f60572802', foundational, conciliar_texts_encode_incompatible_visions).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_incompatible_visions, holdable).
narrative_ontology:cs_axiom_grounding('eb820d33-6a99-4529-9b27-ae3f60572802', conciliar_texts_encode_incompatible_visions, empirically_contingent).
narrative_ontology:cs_axiom('eb820d33-6a99-4529-9b27-ae3f60572802', foundational, hermeneutical_control_is_locus_of_authority).
narrative_ontology:cs_axiom_status(hermeneutical_control_is_locus_of_authority, holdable).
narrative_ontology:cs_axiom_grounding('eb820d33-6a99-4529-9b27-ae3f60572802', hermeneutical_control_is_locus_of_authority, conventional).
narrative_ontology:cs_reference_frame('eb820d33-6a99-4529-9b27-ae3f60572802', conciliar_compromise_formulation).
narrative_ontology:cs_drift_state('eb820d33-6a99-4529-9b27-ae3f60572802', contemporary_hermeneutical_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eb820d33-6a99-4529-9b27-ae3f60572802', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, theological_establishment).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_factions).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reformers).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_faithful).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the interpretation and implementation of Vatican II texts, leveraging their ambiguity to maintain central authority and manage internal dissent. Benefits from the ability to selectively emphasize aspects of the texts to suit current magisterial priorities.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_curia, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the ongoing hermeneutical debate, as it provides a continuous field for academic and magisterial theological work. Their careers are often built on navigating and interpreting the complex legacy of the Council.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, theological_establishment, beneficiary,
    organized, biographical, constrained, global).

% Bear the cost of perceived rupture and ambiguity, feeling alienated from the post-conciliar Church. Their identity is often tied to pre-conciliar traditions, making exit from the Church unthinkable but forcing them into a position of internal resistance and dissent.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_factions, payer,
    moderate, generational, identity_locked, global).

% Bear the cost of perceived insufficient change and the suppression of radical reform. They see the ambiguities as holding back necessary evolution, but their commitment to the Church's mission keeps them engaged despite frustration.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reformers, payer,
    moderate, generational, identity_locked, global).

% Experience confusion and division stemming from the conflicting interpretations of the Council. They are often caught between different theological camps, leading to a sense of instability in their religious practice and understanding.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_faithful, payer,
    powerless, biographical, constrained, global).

% Analyze the historical context, drafting processes, and reception of Vatican II, often highlighting the political and theological compromises embedded in the texts. Their work provides critical insight into the constraint's formation and persistence.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, ecclesial_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed the Catholic Church to achieve supermajority consensus on conciliar documents by incorporating diverse, sometimes conflicting, theological viewpoints through ambiguous language, thereby preventing schism at the time of the Council.
% TRANSFER_FUNCTION: Transfers hermeneutical control and the power to define 'authentic' interpretation to the Roman Curia and its favored theological schools, from factions advocating for clear rupture or strict continuity.
% ABSENT_VOICES: Those who voted against certain conciliar documents (the 10-12% rejection votes) represent voices whose theological incompatibilities were embedded in the final texts but whose clear dissent was ultimately overridden by the compromise formulations. Their explicit theological positions are now marginalized in official discourse.
% DISAPPEARANCE_RATIONALE: If the composite, overdetermined nature of Vatican II's texts vanished, and a single, unambiguous interpretation (either continuity or rupture) became universally accepted, the entire post-conciliar ecclesial landscape would fundamentally reorganize. The current power dynamics of interpretation would collapse, leading to either a definitive schism or a radical realignment of theological and institutional structures.
% FOUNDING_PROBLEM: The Catholic Church faced deep internal divisions and external pressures for modernization in the mid-20th century, requiring a Council to address these challenges and articulate its mission in the modern world while maintaining unity.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesial historians and sociologists of religion, from outside the Curia and theological establishment, corroborate that the problem of internal division and external relevance remains live, exacerbated by the very ambiguities designed to solve it. The ongoing 'hermeneutic of continuity' vs. 'hermeneutic of rupture' debate is direct evidence.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the ambiguity allows the central authority to selectively emphasize aspects of the texts, effectively extracting compliance from diverse factions while denying their full theological positions. Suppression (0.70) is high because the system actively manages and suppresses interpretations that too strongly favor either extreme (traditionalist or progressive), maintaining the 'unity' of the compromise. Theater ratio (0.40) reflects the performative aspect of maintaining a 'hermeneutic of continuity' while actual practice and interpretation diverge significantly. The rising trend in extractiveness and suppression over time reflects the increasing institutional effort required to manage the inherent tensions and prevent outright schism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Roman Curia, the ambiguity is a necessary tool for maintaining unity and guiding the Church. From the perspective of traditionalists and progressives, it is a source of frustration and a means of suppressing their respective visions. The engine's classification will highlight this divergence, showing how a 'tangled rope' for the payers is framed as a 'rope' or even 'mountain' by the agenda-setters.
 *
 * DIRECTIONALITY LOGIC:
 *   The Roman Curia and the theological establishment are beneficiaries, as they gain hermeneutical control and a continuous field for their work. Traditionalist factions, progressive reformers, and the lay faithful are payers, bearing the costs of ambiguity, perceived alienation, and internal division. Their 'identity_locked' exit options reflect their deep commitment to the Church, making outright departure unthinkable despite the costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a simple 'rope' (pure coordination) by highlighting the asymmetric extraction of hermeneutical control and the active suppression of alternative interpretations. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine coordination function of preventing immediate schism at the Council. The mandatrophy is not about the founding problem disappearing, but about the solution (ambiguity) becoming a source of ongoing extraction and division, rather than a temporary bridge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberate_ambiguity_intent,
    'To what extent was the ambiguity in Vatican II texts a deliberate strategy to achieve supermajority votes, versus an unavoidable outcome of complex theological debate?',
    'Further historical research into conciliar archives, including private correspondence and drafting committee notes, to uncover explicit statements of intent regarding compromise language.',
    'If deliberate, it strengthens the ''tangled rope'' classification by emphasizing the intentional design for managing dissent and extracting compliance. If unavoidable, it might slightly reduce the perceived extractiveness by framing ambiguity as a byproduct of genuine theological struggle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_ambiguity_intent, empirical, 'Intent behind the ambiguous formulations in Vatican II texts.').

omega_variable(
    hermeneutical_control_locus,
    'Is the Roman Curia''s hermeneutical control over Vatican II texts a structural necessity for ecclesial unity, or an institutional power grab enabled by ambiguity?',
    'Analysis of alternative models of theological reception and interpretation in other Christian traditions, or counterfactual analysis of a hypothetical decentralized interpretive authority within Catholicism.',
    'If structurally necessary, the extraction is a cost of coordination. If a power grab, it reinforces the ''snare'' aspects of the ''tangled rope'' classification, highlighting the coercive nature of the control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_control_locus, conceptual, 'Nature of hermeneutical control: necessary coordination vs. extractive power.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative interpretations structural (institutional power, doctrinal enforcement) or internalized (self-censorship, fear of marginalization)?',
    'Post-exit suppression trajectory: if theologians or faithful who leave the official structures continue to self-censor or face social isolation, reclassify as partially internalized. If suppression only operates within official channels, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making exit less effective. If purely structural, exit offers clearer relief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for theological dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.4).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.5).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Vatican II Magisterial Authority' kernel. It posits that the Council's texts are a composite of incompatible visions, designed to achieve consensus through ambiguity. This differs from the 'continuity' reading (which emphasizes organic development) and the 'rupture' reading (which emphasizes a fundamental break). All three readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
