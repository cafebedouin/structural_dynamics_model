% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Progressive-Abrogation Reading of Qur'anic Gender Verses (Naskh via 49:13)
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   This story authors ONE reading — progressive_abrogation — of the
 *   contested Qur'anic gender-verses kernel. This reading holds that later,
 *   more universal revelations (paradigmatically 49:13's declaration of
 *   universal human dignity across peoples) doctrinally abrogate (naskh) the
 *   earlier gender-specific legal verses (4:11 inheritance, 2:282 testimony,
 *   4:34 guardianship), producing full legal gender parity as the correct
 *   contemporary ruling. The referent for extractiveness is the standing
 *   arrangement under contest as THIS reading sees it: an interpretive and
 *   institutional order in which literal/specific gender rulings remain
 *   operative fiqh, which this reading regards as an illegitimately frozen
 *   partial reading of an incomplete revelatory trajectory. This is not a
 *   story about which reading is correct; it is a story about the structural
 *   shape of holding this particular reading. Two sibling constraints —
 *   literal_hierarchical and contextual_egalitarian — exist as separate
 *   stories with their own ε and stakeholder structures; this file does not
 *   average over them or import their metrics.
 *
 * KEY AGENTS:
 *   - progressive_islamic_scholars: agenda-setters who construct and administer the reading
 *   - reformist_muslim_women: primary beneficiaries of the legal parity outcome
 *   - traditional_fiqh_authorities: institutional payers whose authority is delegitimized
 *   - scholars_committed_to_naskh_orthodoxy: identity-locked payers whose methodology is directly contradicted
 *   - communities_bound_to_literal_gender_rulings: powerless, trapped payers whose settled arrangements are unsettled
 *   - secular_legal_reform_bodies: excluded interested parties
 *   - comparative_religion_scholars: analytical observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.81).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.62).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.81).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Progressive-Abrogation Reading of Qur'anic Gender Verses (Naskh via 49:13)").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '77e4479b-d82b-48ee-a9ea-16f9fd01b222').
narrative_ontology:cs_kernel_codification('77e4479b-d82b-48ee-a9ea-16f9fd01b222', fixed_text).
narrative_ontology:cs_authority_grounding('77e4479b-d82b-48ee-a9ea-16f9fd01b222', practice).
narrative_ontology:cs_interpretation_layer_present('77e4479b-d82b-48ee-a9ea-16f9fd01b222').
narrative_ontology:cs_reading_relation('77e4479b-d82b-48ee-a9ea-16f9fd01b222', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('77e4479b-d82b-48ee-a9ea-16f9fd01b222', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_axiom('77e4479b-d82b-48ee-a9ea-16f9fd01b222', foundational, later_universal_principle_abrogates_earlier_specific_rule).
narrative_ontology:cs_axiom_status(later_universal_principle_abrogates_earlier_specific_rule, holdable).
narrative_ontology:cs_axiom_grounding('77e4479b-d82b-48ee-a9ea-16f9fd01b222', later_universal_principle_abrogates_earlier_specific_rule, conventional).
narrative_ontology:cs_axiom('77e4479b-d82b-48ee-a9ea-16f9fd01b222', foundational, revelatory_trajectory_terminates_in_full_gender_parity).
narrative_ontology:cs_axiom_status(revelatory_trajectory_terminates_in_full_gender_parity, holdable).
narrative_ontology:cs_axiom_grounding('77e4479b-d82b-48ee-a9ea-16f9fd01b222', revelatory_trajectory_terminates_in_full_gender_parity, instrumental).
narrative_ontology:cs_reference_frame('77e4479b-d82b-48ee-a9ea-16f9fd01b222', classical_naskh_specific_verse_pair_methodology).
narrative_ontology:cs_drift_state('77e4479b-d82b-48ee-a9ea-16f9fd01b222', contemporary_gender_equality_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('77e4479b-d82b-48ee-a9ea-16f9fd01b222', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, reformist_muslim_women).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, egalitarian_muslim_movements).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_fiqh_authorities).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, scholars_committed_to_naskh_orthodoxy).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_gender_rulings).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, universal_human_dignity_supersedes_particularized_ordinance).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, quranic_revelation_as_progressive_trajectory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Construct and advocate the abrogation-based reading, arguing 49:13's universal dignity principle legally supersedes 4:11, 2:282, and 4:34 as later-in-revelation-order or higher-in-principle. They administer the interpretive apparatus that determines which verses are treated as abrogated. Their institutional standing depends on the reading gaining traction; exiting the argument means losing the platform they have built around it.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars, beneficiary).

% Gain a textually-grounded route to full legal parity in inheritance, testimony, and guardianship without having to reject Qur'anic authority outright. The reading gives them standing to contest literalist rulings inside religious institutions rather than only in secular courts. Their exit options remain constrained by family and community embeddedness even where the reading succeeds.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, reformist_muslim_women, beneficiary,
    moderate, biographical, constrained, global).

% Their entire interpretive authority rests on naskh being a narrow, verse-specific doctrine (later verse abrogates earlier verse on the SAME topic) rather than a trajectory-wide principle that lets one thematic verse override unrelated specific legal verses. This reading, if adopted, delegitimizes centuries of accumulated fiqh on inheritance, testimony, and guardianship at a stroke. They cannot simply exit the tradition whose authority they embody without ceasing to be what they are.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_fiqh_authorities, payer,
    institutional, civilizational, trapped, global).

% Have built careers on classical naskh methodology (specific verse-pair abrogation with established chronology and occasion-of-revelation evidence). Adopting the progressive-abrogation reading would require abandoning the methodological rules that license their scholarly authority in the first place, since 49:13 does not share a legal subject matter with 4:11/4:34/2:282 under classical naskh criteria. Their professional identity is fused with the narrower doctrine; abandoning it is not a career move but a self-dissolution.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, scholars_committed_to_naskh_orthodoxy, payer,
    organized, generational, identity_locked, global).

% Have organized inheritance practices, family law, and social identity around the literal readings for generations. A wholesale reclassification of these verses as abrogated threatens inheritance settlements already made, marital and custodial arrangements structured on the differentiated rules, and communal self-understanding. They have no forum in which to contest the reinterpretation and experience it as imposed from outside rather than debated from within.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_gender_rulings, payer,
    powerless, generational, trapped, regional).

% Would benefit from a religiously-legitimated egalitarian reading that eases codification of gender-equal family law, but are not party to the internal Islamic hermeneutic dispute and have no standing to adjudicate whose reading of naskh is correct. Their interest in the outcome is real but they are structurally absent from the interpretive contest.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, secular_legal_reform_bodies, excluded,
    institutional, generational, analytical, national).

% Study the abrogation dispute as a case of contested textual authority and doctrinal innovation, documenting how the progressive-abrogation reading departs from classical naskh methodology and what authority claims underwrite each side.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, diffuse).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a textually-internal mechanism (naskh) by which a religious community committed to Qur'anic authority can arrive at gender-egalitarian legal outcomes without repudiating the text itself — coordinating reformist aspiration with continued claim to scriptural legitimacy.
% TRANSFER_FUNCTION: Moves interpretive authority and the legal outcomes that follow from it (inheritance shares, testimony weight, guardianship rights) away from communities and scholars whose standing rests on the literal/specific readings, toward scholars and constituencies whose standing rests on the trajectory/principle reading — a transfer of legitimacy and downstream legal entitlement, not merely of argument.
% ABSENT_VOICES: Communities bound to literal gender rulings have no forum inside the academic-hermeneutic dispute; the argument is conducted among credentialed scholars on both sides, while those whose settled family arrangements would be unsettled by a shift are not consulted. Secular reform bodies who would benefit are also outside the properly religious contest and cannot participate as adjudicators.
% DISAPPEARANCE_RATIONALE: If the progressive-abrogation reading vanished as a live claim, reformist arguments for gender parity would lose their strongest textually-internal warrant, traditional fiqh authorities would face less pressure to defend their methodology, and legal reform efforts in religiously-observant contexts would have to rely more heavily on maqasid-style contextualist arguments (a different reading, with different structural properties) or on secular-legal routes external to the tradition.
% FOUNDING_PROBLEM: The founding problem is the perceived tension between an explicit, differentiated legal text (4:11, 2:282, 4:34) and a later, more universal statement of human dignity (49:13) — reformist scholars needed a doctrinally legitimate mechanism, recognized within classical usul al-fiqh, to resolve this tension in favor of the universal principle rather than the particular rule.
% FOUNDING_PROBLEM_CORROBORATION: Progressive scholars attest the tension is real and unresolved by classical method, citing modern conditions unanticipated by classical jurists. Traditional fiqh authorities and naskh-orthodox scholars — parties outside the reading's own beneficiary set — attest that classical naskh doctrine already resolves the relationship (specific governs general; thematic breadth does not license abrogation across unrelated legal subject matter) and that no genuine unresolved tension exists within the classical framework; independent comparative-religion scholarship documents the dispute as live and unsettled rather than corroborating either side's genealogical claim outright.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored very high (0.81) because, BY THIS READING'S OWN LIGHTS, the standing arrangement it contests — continued fiqh reliance on the literal gender-specific verses — represents an illegitimate extraction of authority from women and from the correct revelatory trajectory, sustained by traditional institutions that benefit from the status quo ruling. Suppression is authored substantial but lower than extractiveness (0.62) because this reading, being itself an insurgent minority position within most traditional institutions, exercises real interpretive pressure but does not yet command enforcement machinery comparable to established fiqh bodies — its suppression is exercised mainly through delegitimation and academic/activist pressure rather than through institutional coercion. Accessibility collapse is authored low-moderate (0.35): the classical naskh-orthodox alternative and the contextual_egalitarian sibling both remain live, well-resourced, widely-held alternatives — this reading has not achieved anything like the alternative-foreclosing dominance a mountain or an entrenched snare would show. Resistance is authored very high (0.88): this is among the most actively contested doctrinal claims in modern Islamic legal thought, resisted by both classical scholarship and by communities whose family law rests on the literal rulings.
 *
 * PERSPECTIVAL GAP:
 *   From progressive_islamic_scholars' seat, this is a rope-like recovery of a suppressed egalitarian trajectory using a doctrinally legitimate tool (naskh) already internal to the tradition. From traditional_fiqh_authorities' and naskh-orthodox scholars' seats, this looks like extraction dressed as textual fidelity: a wholesale reassignment of legal authority achieved by stretching a narrow doctrine (verse-specific abrogation) into a sweeping thematic-trajectory claim it was never designed to support, with the coordination story (fidelity to 'the real Qur'an') serving as cover for a genuine transfer of authority away from them. The engine computes these divergent seat classifications from the declared power/exit/beneficiary structure; this file does not adjudicate which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive_islamic_scholars and reformist_muslim_women sit near the beneficiary end: the reading transfers legal standing and interpretive legitimacy to them. Traditional_fiqh_authorities and scholars_committed_to_naskh_orthodoxy sit near the target end — their institutional and professional authority is the thing being delegitimized by the very success of this reading; their exit options are trapped/identity_locked because abandoning their commitments constitutes self-dissolution rather than a portable career move. Communities_bound_to_literal_gender_rulings are powerless payers with trapped exit — they neither authored the dispute nor can exit the consequences of its resolution either way.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure coordination (a rope) by requiring the declared victim set — traditional_fiqh_authorities, naskh-orthodox scholars, and literalist communities all pay real costs in authority, professional standing, and settled arrangements. It equally prevents mislabeling as pure extraction (a snare) because a genuine coordination function exists: reformist constituencies committed to remaining within the tradition need SOME internally legitimate mechanism to reconcile universalist and particularist verses, and naskh is a doctrine the tradition itself supplies for exactly this kind of resolution — the dispute is over its scope of application, not over whether such a mechanism can exist at all. Hence tangled_rope: real coordination function (reconciling scripture with contemporary equity commitments for those who need to stay inside the tradition) plus real, named, asymmetric extraction (delegitimizing an entire prior authority structure) plus active enforcement (ongoing institutional contest over whose interpretive authority governs family law) — all three gate conditions are met.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_scope_legitimacy,
    'Does classical usul al-fiqh naskh doctrine, properly applied, permit a thematically-general later verse (49:13, on human dignity broadly) to abrogate legally-specific earlier verses on unrelated subject matter (inheritance shares, testimony procedure, marital guardianship), or does classical doctrine restrict naskh to verse-pairs addressing the same specific legal question?',
    'Systematic review of classical usul al-fiqh sources (al-Shafi''i, al-Suyuti''s al-Itqan, and subsequent naskh compilations) against the specific scope claim; consensus (or its absence) among recognized specialists in classical hermeneutic methodology, assessed independently of the outcome each side prefers.',
    'If classical doctrine does not support trajectory-wide naskh, this reading is a doctrinal innovation dressed in traditional vocabulary — closer to a snare on traditional authorities (illegitimate extraction of authority via methodological overreach). If classical precedent for principle-based abrogation exists, the reading has genuine internal legitimacy and the tangled_rope classification''s coordination function is stronger than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_scope_legitimacy, conceptual, 'Whether the reading''s core hermeneutic move is licensed by the tradition''s own methodological rules or exceeds them.').

omega_variable(
    committer_frame_kernel_disagreement,
    'This story instantiates the progressive_abrogation reading of the quranic_gender_verses kernel. The sibling readings (literal_hierarchical, contextual_egalitarian) are separate constraints with their own ε. Where is the disagreement actually located structurally: is it a disagreement about WHAT NASKH MEANS (methodology), about WHETHER THE TEXT HAS A TRAJECTORY AT ALL (hermeneutic premise), or about WHO HOLDS INTERPRETIVE AUTHORITY to make either determination (institutional/political)?',
    'Structural decomposition of each reading''s foundational axioms (see cs_structure.axioms across the three sibling files) to identify whether the readings share premises and diverge only on application, or diverge at the level of foundational commitment.',
    'If the disagreement is purely methodological, reconciliation within a shared framework may be possible over time. If it is a foundational premise disagreement (as the forecloses relation to literal_hierarchical suggests), the readings are not merely different applications of one method but incompatible frameworks — reconciliation would require one side abandoning a foundational axiom, which is precisely why exit costs are so high for scholars who cross between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_disagreement, conceptual, 'Locating where, structurally, the three sibling readings of the kernel actually diverge.').

omega_variable(
    epistemic_violence_vs_liberation_framing,
    'Is the delegitimization this reading imposes on literalist communities best understood as epistemic violence against a community whose identity is constitutively bound to the literal reading, or as the correction of a previously-extractive arrangement that those communities'' leadership had an interest in perpetuating?',
    'Longitudinal study of communities that have undergone this interpretive shift: do the previously-bound communities report the change as liberation, imposition, or a contested mix; does the reported experience differ between those with power within the old order (fiqh authorities) and those without (women within literalist communities)?',
    'This affects whether communities_bound_to_literal_gender_rulings is correctly classified as a straightforward payer or whether it should be split into sub-groups with opposed directionality (women within those communities may be simultaneous payers-of-disruption and beneficiaries-of-parity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_violence_vs_liberation_framing, preference, 'Whether the reading''s impact on literalist communities is best framed as harm or as correction, and whether this varies by position within those communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t8, quranic_gender_verses__progressive_abrogation, theater_ratio, 8, 0.15).
narrative_ontology:measurement(qura_tr_t16, quranic_gender_verses__progressive_abrogation, theater_ratio, 16, 0.19).
narrative_ontology:measurement(qura_tr_t24, quranic_gender_verses__progressive_abrogation, theater_ratio, 24, 0.22).
narrative_ontology:measurement(qura_tr_t32, quranic_gender_verses__progressive_abrogation, theater_ratio, 32, 0.25).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qura_be_t8, quranic_gender_verses__progressive_abrogation, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(qura_be_t16, quranic_gender_verses__progressive_abrogation, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(qura_be_t24, quranic_gender_verses__progressive_abrogation, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(qura_be_t32, quranic_gender_verses__progressive_abrogation, base_extractiveness, 32, 0.77).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qura_su_t8, quranic_gender_verses__progressive_abrogation, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(qura_su_t16, quranic_gender_verses__progressive_abrogation, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(qura_su_t24, quranic_gender_verses__progressive_abrogation, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(qura_su_t32, quranic_gender_verses__progressive_abrogation, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__progressive_abrogation, 0.1).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the quranic_gender_verses kernel: literal_hierarchical (the verses as timeless divine ordinance, unabrogated), contextual_egalitarian (the verses as historically-situated progressive steps requiring maqasid-based reinterpretation without formal abrogation), and this reading, progressive_abrogation (the verses as an incomplete trajectory formally superseded via naskh). Each reading is authored as its own constraint with its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked here rather than merged because measuring 'the constraint' by different readings' lights produces materially different extractiveness and different victim sets — a signal that these are structurally distinct constraints sharing a textual object, not one constraint with an ambiguous measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
