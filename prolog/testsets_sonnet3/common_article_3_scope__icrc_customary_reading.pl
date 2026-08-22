% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: Common Article 3 Scope as Customary-Law-Tracked Coordination Mechanism (ICRC Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions sets a minimum
 *   humanitarian floor for 'armed conflict not of an international
 *   character,' but does not define that phrase precisely. Since 1949, the
 *   ICRC's customary IHL studies and doctrine, combined with international
 *   tribunal jurisprudence (ICTY, ICTR, ICC) citing accumulated state
 *   practice, have functioned as the mechanism by which CA3's practical scope
 *   is determined and has gradually expanded to cover forms of organized
 *   violence unanticipated by the original drafters. This story authors the
 *   ICRC-customary reading as a distinct constraint: a procedural
 *   coordination mechanism for interpretive expansion, not a fixed rule. It
 *   sits in a kernel triplet alongside a state-centric reading (fixed
 *   intensity/organization thresholds) and an expansive human-rights reading
 *   (any organized violence triggers the floor) — those are separate
 *   constraints, not alternative measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.42).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.38).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "Common Article 3 Scope as Customary-Law-Tracked Coordination Mechanism (ICRC Reading)").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__icrc_customary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '7a776dd8-e946-4736-bdc2-df4295eb4d18').
narrative_ontology:cs_kernel_codification('7a776dd8-e946-4736-bdc2-df4295eb4d18', distributed).
narrative_ontology:cs_authority_grounding('7a776dd8-e946-4736-bdc2-df4295eb4d18', practice).
narrative_ontology:cs_interpretation_layer_present('7a776dd8-e946-4736-bdc2-df4295eb4d18').
narrative_ontology:cs_reading_relation('7a776dd8-e946-4736-bdc2-df4295eb4d18', common_article_3_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('7a776dd8-e946-4736-bdc2-df4295eb4d18', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('7a776dd8-e946-4736-bdc2-df4295eb4d18', foundational, custom_crystallizes_gradually_through_accumulated_practice).
narrative_ontology:cs_axiom_status(custom_crystallizes_gradually_through_accumulated_practice, holdable).
narrative_ontology:cs_axiom_grounding('7a776dd8-e946-4736-bdc2-df4295eb4d18', custom_crystallizes_gradually_through_accumulated_practice, conventional).
narrative_ontology:cs_axiom('7a776dd8-e946-4736-bdc2-df4295eb4d18', secondary, interpretive_authority_may_reside_in_non_state_institutions).
narrative_ontology:cs_axiom_status(interpretive_authority_may_reside_in_non_state_institutions, holdable).
narrative_ontology:cs_axiom_grounding('7a776dd8-e946-4736-bdc2-df4295eb4d18', interpretive_authority_may_reside_in_non_state_institutions, conventional).
narrative_ontology:cs_reference_frame('7a776dd8-e946-4736-bdc2-df4295eb4d18', id_1949_negotiated_textual_minimum).
narrative_ontology:cs_drift_state('7a776dd8-e946-4736-bdc2-df4295eb4d18', post_ictr_icty_jurisprudence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7a776dd8-e946-4736-bdc2-df4295eb4d18', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc_and_custodial_institutions).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, civilians_in_ambiguous_conflicts).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, detained_persons_in_non_international_conflicts).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_facing_contested_classification).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, non_state_armed_groups_denied_recognition).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, domestic_law_enforcement_targets_reclassified_as_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, troop_contributing_and_intervening_states).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, troop_contributing_and_intervening_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compiles and publishes customary IHL studies, issues commentaries, and advises tribunals and states on whether CA3's minimum protections apply to a given situation. Its interpretive output is treated as authoritative evidence of opinio juris even though it is not itself a state and cannot bind anyone by treaty. It gains institutional authority, funding relevance, and a durable role as the reference point every party cites, without bearing the costs of the classification decisions it shapes.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc_and_custodial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, icrc_and_custodial_institutions, beneficiary).

% Live inside situations of organized violence that do not cleanly fit either 'international armed conflict' or 'ordinary policing.' Whether they receive CA3's minimal protections against violence, torture, and unfair trial depends on whether accumulated state practice has crystallized around treating their situation as covered. They have no voice in that crystallization process and cannot exit the territory.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, civilians_in_ambiguous_conflicts, beneficiary,
    powerless, immediate, trapped, regional).

% Held by state forces or armed groups in conflicts whose classification is contested. If the evolving customary reading extends CA3, they gain enforceable minimum guarantees against summary execution and cruel treatment; if the reading is contested or unsettled, their protection depends entirely on which interpretive camp prevails in a given tribunal or diplomatic forum.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, detained_persons_in_non_international_conflicts, beneficiary,
    powerless, immediate, trapped, regional).

% Conduct counter-insurgency or internal security operations and must decide, often under litigation or international pressure, whether their operations are governed by CA3. Because the scope shifts with accumulating practice and ICRC commentary rather than a fixed treaty text, states cannot know in advance what standard they will be held to; they experience the customary-tracking mechanism as a moving target that can retroactively characterize past operations as violations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_facing_contested_classification, payer,
    institutional, biographical, constrained, national).

% Fighting forces whose classification as a party to a 'conflict' (versus mere criminal or terrorist actors) determines whether their own members receive CA3 protections if captured. The customary process that would recognize their situation as covered is controlled by state practice they have no standing to contribute to, so recognition tends to lag or never arrive even where the humanitarian rationale is identical.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, non_state_armed_groups_denied_recognition, payer,
    moderate, immediate, trapped, regional).

% Individuals engaged in what states initially characterize as ordinary criminal or public-order matters, who may later be treated as parties to a CA3-covered conflict once state practice shifts or violence intensifies. The shift can work against them too, subjecting them to conflict-law characterizations (e.g., 'combatant' framing) that carry different consequences than ordinary criminal process.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, domestic_law_enforcement_targets_reclassified_as_conflict, payer,
    powerless, immediate, trapped, national).

% Adjudicate individual cases by applying the customary-practice test to determine CA3 applicability, citing ICRC studies and state practice compilations as evidence. Their rulings themselves become part of the state practice record, creating a feedback loop where the interpretive apparatus is partly self-referential.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals, agenda_setter).

% Powerful states with deployable militaries can shape the customary record through their own practice and can afford legal advisory capacity to manage classification risk; weaker states cannot. The same mechanism that constrains a weak state's operations is more navigable, even instrumentalizable, by a state with resources to influence what counts as settled practice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, troop_contributing_and_intervening_states, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, troop_contributing_and_intervening_states, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for extending baseline humanitarian protections to new forms of organized violence without requiring every state to ratify a treaty amendment each time armed conflict evolves in form (proxy warfare, prolonged internal unrest, transnational non-state actors) — coordination is achieved through accretion of state practice and opinio juris rather than renegotiation.
% TRANSFER_FUNCTION: Moves interpretive authority over who receives minimum humanitarian protections from a fixed treaty text (agreed once, by treaty parties, in 1949) to an ongoing, diffuse process of practice-accumulation substantially curated and narrated by the ICRC and cited by tribunals — shifting practical determinative power toward the institutions that compile and certify 'customary' status.
% ABSENT_VOICES: Non-state armed groups and civilian populations in contested-classification zones have no standing in the state-practice-and-opinio-juris process that determines whether they are covered; the process formally counts only state conduct and statements as evidence, structurally excluding the parties whose treatment is most directly at stake.
% DISAPPEARANCE_RATIONALE: If the customary-tracking mechanism vanished, the field would revert to a fixed textual reading of CA3 (arguably the state-centric or expansive readings, depending on which is written into a replacement text) — states operating in classification-ambiguous conflicts would gain more predictability at the cost of the mechanism's capacity to extend protection to novel conflict forms without treaty renegotiation. Whether the world genuinely 'rearranges' or just formalizes what customary practice was already doing is disputed between ICRC-aligned commentators and state-sovereignty-focused scholars.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions' negotiators could not anticipate every future form of internal or hybrid armed conflict, and states were unwilling to accept a fully open-ended treaty obligation; the customary-law-tracking approach was meant to let CA3's protective floor keep pace with how organized violence actually evolves, without requiring a diplomatic conference each time.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and international criminal tribunals (as institutions whose authority derives from certifying customary status) attest the mechanism remains necessary and functioning. Independent international law scholars and several states' foreign ministries have testified in treaty-body and academic fora that the process has become a vehicle for interpretive expansion beyond what negotiating states consented to, and that 'evolving practice' functions as a one-way ratchet toward broader coverage rather than a neutral tracking mechanism — this is corroboration from outside the ICRC's own institutional position, though it comes from parties (states) who are also the payers under this reading, which is itself part of the contested picture.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, contested).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) reflects that the mechanism transfers real interpretive authority to institutions (ICRC, tribunals) whose classifications become self-reinforcing evidence of custom, at a cost borne unevenly by states facing unpredictable retroactive characterization and by armed groups/individuals with no standing in the practice-generating process. Suppression (0.38) is moderate: there is no direct coercive enforcement of a single interpretation, but tribunal rulings backed by ICC jurisdiction and reputational/diplomatic pressure make deviation costly for weaker states. Theater ratio (0.30) captures that a meaningful share of 'customary practice documentation' functions as institutional legitimation activity for the ICRC's own interpretive authority rather than pure fact-finding. Accessibility collapse (0.40) is moderate — states and tribunals retain some room to argue against a given customary claim, unlike a mountain where alternatives are foreclosed. Resistance (0.55) is substantial: sovereignty-focused states and scholars actively contest 'evolving custom' claims as illegitimate expansion beyond consent.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC and tribunals are structural beneficiaries/agenda-setters: their institutional relevance and interpretive authority grow as the mechanism operates, and they bear none of the compliance costs directly. Civilians and detainees in ambiguous conflicts are intended beneficiaries of the mechanism's protective expansion but have zero voice in the process that determines whether they are covered — a beneficiary group that is also structurally powerless and excluded from its own vindication process. States facing classification risk and non-state armed groups denied recognition are payers: the former bear unpredictable retroactive liability exposure, the latter bear denial of reciprocal protection. Powerful troop-contributing states occupy a hybrid position — nominally payers under the same mechanism, but with resources to shape the practice record in their favor, which is why they carry a secondary beneficiary role and an arbitrage-adjacent exit option relative to weaker states.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (treaty text cannot anticipate future conflict forms) remains partly live — hybrid and transnational conflicts do keep arising that strain any fixed 1949 text. But the mechanism's contested status turns on whether 'tracking evolving practice' still serves that founding function or has become a vehicle through which specific institutions (ICRC, tribunals) manufacture the practice they then certify as customary — a self-referential loop that the founding problem does not obviously require. This is exactly the kind of divergence the tangled_rope classification is built to hold: a genuine coordination function (avoiding constant treaty renegotiation) coexisting with asymmetric extraction (interpretive authority concentrating in unaccountable institutions, costs falling on states and non-state actors with no say in the process).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three readings of the common_article_3_scope kernel structurally diverge — is it the definition of ''conflict'' itself (state-centric vs. expansive) or the METHOD by which any definition is fixed over time (this reading''s procedural claim)?',
    'Compare tribunal opinions applying each reading to the same fact pattern (e.g., a specific internal unrest episode) and identify whether the disagreement is about intensity/organization thresholds, about whether any organized violence suffices, or about who gets to certify that a threshold has been met.',
    'If the disagreement is purely about the certifying process (this reading''s claim), then the icrc_customary_reading is compatible with either substantive outcome and functions as a meta-level coordination mechanism sitting above the other two, rather than as a rival substantive scope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether this reading is a rival substantive claim or a meta-level procedural claim relative to its siblings.').

omega_variable(
    practice_generation_self_reference,
    'Does the ICRC/tribunal citation loop (tribunals cite ICRC studies as evidence of custom; ICRC studies cite tribunal rulings as evidence of custom) constitute genuine independent evidence of state practice, or a circularity that manufactures apparent consensus?',
    'Trace citation chains in a sample of ICC/ICTY judgments and ICRC customary law study entries to determine what fraction of cited ''practice'' is independent state conduct versus prior tribunal/ICRC output citing itself.',
    'High circularity would support classifying part of the measured extraction as manufactured legitimacy (theater) rather than genuine coordination; low circularity would support the coordination framing more strongly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practice_generation_self_reference, empirical, 'Whether the customary-practice record is substantially self-referential.').

omega_variable(
    cs_framing_underdetermination_kernel_vs_procedure,
    'Is the correct kernel here ''CA3''s substantive scope'' (with this reading as one substantive claim among three) or ''the authority to determine CA3''s scope'' (with this reading as the ONLY reading that makes the authority question explicit, while the other two readings smuggle in an implicit authority claim)?',
    'Examine whether the state-centric and expansive readings each implicitly assume a determinate authority (state consent thresholds vs. universal humanitarian principle) without acknowledging the procedural question this reading foregrounds; if so, the kernel may actually be two-layered.',
    'If the kernel is two-layered, this reading''s cs_structure would need a different reference_frame than a flat three-way sibling comparison suggests, since it operates at a different structural layer than its siblings rather than merely offering a competing definition at the same layer. Retained as an open framing question rather than resolved unilaterally in this authoring pass, per the CS-framing under-determination guidance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination_kernel_vs_procedure, conceptual, 'Whether this reading sits at the same structural layer as its siblings or one layer above them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(comm_tr_t1965, common_article_3_scope__icrc_customary_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(comm_tr_t1980, common_article_3_scope__icrc_customary_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__icrc_customary_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__icrc_customary_reading, theater_ratio, 2005, 0.27).
narrative_ontology:measurement(comm_tr_t2015, common_article_3_scope__icrc_customary_reading, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__icrc_customary_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.2).
narrative_ontology:measurement(comm_be_t1965, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1965, 0.24).
narrative_ontology:measurement(comm_be_t1980, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1995, 0.33).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2005, 0.37).
narrative_ontology:measurement(comm_be_t2015, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement(comm_su_t1965, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1965, 0.24).
narrative_ontology:measurement(comm_su_t1980, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement(comm_su_t1995, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1995, 0.31).
narrative_ontology:measurement(comm_su_t2005, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2005, 0.34).
narrative_ontology:measurement(comm_su_t2015, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__icrc_customary_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the common_article_3_scope kernel. state_centric_reading authors CA3 as bound by fixed intensity/organization thresholds (lower extraction, higher predictability, favors defending states). expansive_human_rights_reading authors CA3 as triggered by any organized armed violence regardless of classification (higher protective reach, contested by states as unbounded). icrc_customary_reading (this story) authors the SCOPE-DETERMINING PROCESS itself as the constraint object — a procedural/coordination mechanism that permits gradual substantive drift between the other two poles without treaty renegotiation. All three share the same underlying kernel (what triggers CA3 protection) but diverge on where authority to answer that question sits and what evidentiary process counts. Per ε-invariance, each is authored as its own constraint with its own extraction profile rather than as three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
