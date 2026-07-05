% ============================================================================
% CONSTRAINT STORY: naskh_principle__contextual_harmonization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__contextual_harmonization, []).

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
 *   constraint_id: naskh_principle__contextual_harmonization
 *   human_readable: Contextual Harmonization Reading of Naskh (No Chronological Abrogation)
 *   domain: religious/legal
 *
 * SUMMARY:
 *   This story instantiates one specific reading of the contested naskh
 *   (abrogation) kernel within Islamic legal theory: the contextual
 *   harmonization position, which holds that apparently conflicting Quranic
 *   verses on the same legal matter are both fully valid, each governing
 *   within its own revelatory and situational context, rather than one verse
 *   superseding another chronologically. This is distinct from the
 *   classical_abrogation reading (later verses void earlier ones) and the
 *   progressive_restriction reading (revelation moves
 *   permissively-to-restrictively as pedagogy, without invalidating earlier
 *   text) — those are separate constraints with their own epsilon values,
 *   linked here only through the network and commentary, never blended into
 *   this one's classification.
 *
 * KEY AGENTS:
 *   - contextualist_jurists: agenda_setter (institutional/arbitrage) — administer and teach the harmonization method
 *   - reformist_theological_schools: beneficiary (organized/mobile) — draw on flexibility for adaptive contemporary rulings
 *   - litigants_seeking_definitive_rulings: payer (powerless/trapped) — bear the cost of reduced legal finality
 *   - classical_abrogation_school_jurists: payer/excluded (organized/identity_locked) — displaced institutional authority
 *   - comparative_islamic_law_scholars: observer (analytical) — study the interpretive competition across schools
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__contextual_harmonization, 0.42).
domain_priors:suppression_score(naskh_principle__contextual_harmonization, 0.38).
domain_priors:theater_ratio(naskh_principle__contextual_harmonization, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, extractiveness, 0.42).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__contextual_harmonization, tangled_rope).
narrative_ontology:human_readable(naskh_principle__contextual_harmonization, "Contextual Harmonization Reading of Naskh (No Chronological Abrogation)").
narrative_ontology:topic_domain(naskh_principle__contextual_harmonization, "religious/legal").

domain_priors:requires_active_enforcement(naskh_principle__contextual_harmonization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__contextual_harmonization, '485edaf4-a8dc-450c-a4bf-7c4b80eab5ef').
narrative_ontology:cs_kernel_codification('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', fixed_text).
narrative_ontology:cs_authority_grounding('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', lineage).
narrative_ontology:cs_interpretation_layer_present('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef').
narrative_ontology:cs_reading_relation('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', naskh_principle__classical_abrogation, forecloses).
narrative_ontology:cs_reading_relation('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', foundational, no_verse_is_textually_invalidated).
narrative_ontology:cs_axiom_status(no_verse_is_textually_invalidated, holdable).
narrative_ontology:cs_axiom_grounding('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', no_verse_is_textually_invalidated, deontological).
narrative_ontology:cs_axiom('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', foundational, contradiction_resolved_by_situational_scope_not_chronology).
narrative_ontology:cs_axiom_status(contradiction_resolved_by_situational_scope_not_chronology, holdable).
narrative_ontology:cs_axiom_grounding('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', contradiction_resolved_by_situational_scope_not_chronology, conventional).
narrative_ontology:cs_reference_frame('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', early_juristic_harmonization_tradition).
narrative_ontology:cs_drift_state('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', contemporary_reformist_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('485edaf4-a8dc-450c-a4bf-7c4b80eab5ef', '').
narrative_ontology:cs_kernel_id(naskh_principle__contextual_harmonization, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, contextualist_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, reformist_theological_schools).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, muslim_communities_seeking_adaptive_rulings).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, litigants_seeking_definitive_rulings).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, classical_abrogation_school_jurists).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, state_legal_systems_requiring_settled_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, muslim_communities_seeking_adaptive_rulings).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, quranic_textual_inerrancy).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, divine_coherence_of_revelation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and apply the asbab al-nuzul (occasions of revelation) methodology to argue that apparently conflicting verses each remain operative within their original situational scope. They administer the interpretive apparatus, train students in it, and issue rulings that preserve interpretive flexibility for their own school. They gain scholarly authority and adaptability precisely because no verse is permanently retired.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, contextualist_jurists, agenda_setter,
    institutional, generational, arbitrage, global).

% Draw on contextual harmonization to argue that verses read as harsh or restrictive by classical abrogation only applied to specific historical circumstances, enabling more adaptive contemporary rulings on matters like women's rights, warfare, and interfaith relations. They can move between interpretive frameworks depending on the ruling sought.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, reformist_theological_schools, beneficiary,
    organized, civilizational, mobile, global).

% Benefit when contextual reasoning permits rulings suited to their specific circumstances rather than a fixed chronological hierarchy, but also bear the cost of legal unpredictability — the same verse may be read differently by different contextualist scholars depending on which situational frame is invoked, leaving ordinary believers uncertain which ruling actually governs their case.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, muslim_communities_seeking_adaptive_rulings, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(naskh_principle__contextual_harmonization, muslim_communities_seeking_adaptive_rulings, payer).

% Bring disputes (marriage, inheritance, contract, criminal matters) before courts operating under contextual-harmonization jurisprudence and receive rulings that can be reopened or reargued by invoking a different situational context for the same verse. They cannot exit the legal system they are embedded in and bear the cost of reduced finality directly, sometimes across multiple appeals or generations of family litigation.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, litigants_seeking_definitive_rulings, payer,
    powerless, immediate, trapped, national).

% Have built centuries of jurisprudential authority on the chronological-supersession model (later verses abrogate earlier ones). Contextual harmonization directly displaces their interpretive method in jurisdictions and schools that adopt it, eroding their institutional standing and the finality their rulings once carried. Their professional identity is fused to the abrogation methodology, making genuine accommodation difficult without conceding the tradition's core claim.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, classical_abrogation_school_jurists, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__contextual_harmonization, classical_abrogation_school_jurists, excluded).

% Codify Islamic law into national statutes and need stable, citable rulings for administration, contract enforcement, and criminal justice. Contextual harmonization's case-by-case situational analysis complicates codification and invites relitigation of settled questions, raising administrative cost and legal uncertainty for the state apparatus.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, state_legal_systems_requiring_settled_law, payer,
    institutional, generational, constrained, national).

% Not an actor — a doctrinal proposition. Contextual harmonization is attractive theologically because it avoids conceding that any revealed verse was simply wrong, overridden, or discarded; every verse remains true within its frame. This is a vindicated doctrine, not a party that collects anything.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, quranic_textual_inerrancy, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(naskh_principle__contextual_harmonization, quranic_textual_inerrancy).

% Study how different madrasas, national courts, and reform movements select among naskh readings, documenting which interpretive choice tends to be invoked for which substantive outcome and by whom.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, comparative_islamic_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__contextual_harmonization, contextualist_jurists).
narrative_ontology:fixing_cost_class(naskh_principle__contextual_harmonization, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutic method allowing the Quranic corpus to be read as internally coherent despite verses that appear to conflict on the same legal topic, by locating the resolution in situational specificity rather than in declaring one verse textually superseded by another.
% TRANSFER_FUNCTION: Moves interpretive authority and legal finality away from jurists and legal systems who need settled, citable rulings, and toward scholars and communities who can invoke situational reframing to keep multiple readings alive; correspondingly moves legal certainty away from ordinary litigants and toward flexibility for interpretive elites.
% ABSENT_VOICES: Litigants whose cases are reopened or complicated by competing situational readings are rarely present in the scholarly debate about which naskh model is correct; classical abrogation jurists are increasingly excluded from institutions that have adopted contextualist curricula, despite having a direct stake in the outcome.
% DISAPPEARANCE_RATIONALE: Contextualist scholars would say the underlying coherence of revelation does not depend on this particular meta-doctrine and the world would barely change theologically; but the institutional structure built around contextual harmonization — training programs, fatwa councils, comparative-law curricula, and specific legal outcomes on family and criminal law that rest on situational readings — would visibly rearrange if the doctrine were displaced by classical abrogation or progressive restriction, since different verses would then govern different cases.
% FOUNDING_PROBLEM: Early and medieval jurists faced Quranic verses that appeared to give contradictory rulings on the same subject (e.g., alcohol, inheritance shares, warfare conduct) and needed a principled way to determine which ruling actually governs, without conceding that the Quran contains genuine internal contradiction.
% FOUNDING_PROBLEM_CORROBORATION: Contextualist jurists themselves attest the founding problem remains fully live because situational variation in real cases never stops arising. Classical abrogation jurists and several national codification bodies attest, from outside the contextualist school, that the founding problem was substantially resolved centuries ago through settled chronological rulings, and that contextual harmonization's persistence past that resolution mainly serves reformist and academic interpretive interests rather than solving a live textual problem.
narrative_ontology:disappearance_verdict(naskh_principle__contextual_harmonization, contested).
narrative_ontology:founding_problem_status(naskh_principle__contextual_harmonization, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__contextual_harmonization, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__contextual_harmonization, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__contextual_harmonization, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__contextual_harmonization_tests).
:- end_tests(naskh_principle__contextual_harmonization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) because the coordination function — theological coherence, avoiding the claim that scripture is self-contradictory — is genuine and substantial, but it comes paired with a real transfer: legal finality and predictability move away from ordinary litigants and state codification systems and toward interpretive elites who retain the discretion to reframe situational context case by case. Suppression is moderate (0.38) reflecting the active curricular and institutional work required to displace the older abrogation-based jurisprudence in schools and courts that adopt this reading; it is not close to the low-suppression profile of a genuine mountain because the doctrinal choice is actively defended and actively contested rather than simply given. Theater ratio is modest and rising slowly (0.15 to 0.28) as institutional adoption of contextualist curricula sometimes outpaces genuine case-level engagement with situational specificity — some invocation of 'context' functions as post-hoc justification for outcomes reached on other grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Contextualist jurists and reformist schools sit near the beneficiary end: they administer the method and gain flexibility and standing from its persistence. Litigants, classical abrogation jurists, and codifying state systems sit near the target end: litigants bear unpredictability directly and cannot exit the legal system; classical jurists lose institutional ground; states bear administrative cost. Muslim communities seeking adaptive rulings are genuinely dual-positioned — they benefit from flexibility in some cases and bear its cost (unpredictability) in others, hence the secondary payer role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — apparent textual contradiction within a claimed-inerrant scripture — remains genuinely live in the sense that new situational disputes keep arising, but classical jurists and codification bodies attest, from outside the contextualist school, that the specific historical contradictions this doctrine was built to resolve were substantially settled long ago through established chronological rulings. The tangled_rope classification (rather than a clean rope) reflects that the coordination function (doctrinal coherence) is real and does not exhaust what the constraint does: it also actively transfers legal finality away from parties who need it, which is why the classification requires both a beneficiary and a victim declaration plus active enforcement through curricula and court adoption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    situational_specification_vs_ad_hoc_reframing,
    'Is the asbab al-nuzul (occasions of revelation) methodology a genuine, bounded interpretive discipline, or does it function as an open-ended license to reframe any verse''s scope whenever a jurist wants a different outcome?',
    'Comparative analysis of contextualist rulings across schools and time periods: if situational specification converges on stable, predictable scope boundaries across independent jurists, the discipline is genuine; if scope boundaries vary opportunistically with desired outcome, the flexibility is closer to unconstrained discretion.',
    'If genuinely bounded, the constraint functions closer to a rope (real coordination gain, modest cost); if effectively unconstrained, the extraction from litigants and codifying institutions is substantially higher than the authored 0.42 and the classification would trend toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(situational_specification_vs_ad_hoc_reframing, empirical, 'Whether contextual specification is a disciplined method or an ad hoc reframing license.').

omega_variable(
    naskh_kernel_reading_selection,
    'Is contextual_harmonization the reading that should govern a given legal question, or do the classical_abrogation and progressive_restriction readings apply instead — and who has standing to decide which reading governs which case?',
    'This is the committer-structure question routed here per Rule 2: no empirical test resolves which reading is ''correct'' since all three are live positions within the broader Islamic legal tradition; resolution (if any) occurs through school affiliation, state codification choice, or comparative jurisprudential argument, not through data.',
    'Sibling reading classical_abrogation would replace this constraint''s flexible, case-sensitive structure with a fixed chronological hierarchy — collapsing the beneficiary set toward jurists who administer chronological rulings and reducing the payer burden on litigants seeking finality, at the cost of theological coherence claims. Sibling reading progressive_restriction would preserve validity of earlier verses similarly to this reading but frame the relationship as pedagogical trajectory rather than situational bounding, changing which verses are read as currently operative in borderline cases (e.g. alcohol, slavery, warfare conduct) without foreclosing either sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naskh_kernel_reading_selection, conceptual, 'Kernel-level reading selection among classical_abrogation, contextual_harmonization, and progressive_restriction; not resolvable by data, only by which framework a given jurist, school, or state adopts.').

omega_variable(
    coherence_doctrine_vs_constructed_flexibility,
    'Does the theological coherence this reading protects (avoiding scriptural self-contradiction) reflect a genuine feature of the text, or is coherence itself constructed by the interpretive method chosen to read it?',
    'Textual analysis independent of any single madrasa''s tradition, comparing how many apparent contradictions require situational specification versus how many would require it under alternative readings — a high, method-invariant contradiction count would support genuine textual tension; a low, method-dependent count would suggest coherence is partly an artifact of the chosen hermeneutic.',
    'If coherence is substantially constructed by the method, the vindicated_propositions (textual inerrancy, divine coherence) are partly circular — the reading vindicates a doctrine that the reading itself helped establish as necessary. This does not change beneficiaries/victims but bears on how much theological weight the coordination function can honestly claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coherence_doctrine_vs_constructed_flexibility, conceptual, 'Whether the coherence being protected is a discovered textual fact or a product of the interpretive method.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__contextual_harmonization, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__contextual_harmonization, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__contextual_harmonization, theater_ratio, 20, 0.18).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__contextual_harmonization, theater_ratio, 40, 0.21).
narrative_ontology:measurement(nask_tr_t60, naskh_principle__contextual_harmonization, theater_ratio, 60, 0.24).
narrative_ontology:measurement(nask_tr_t80, naskh_principle__contextual_harmonization, theater_ratio, 80, 0.26).
narrative_ontology:measurement(nask_tr_t100, naskh_principle__contextual_harmonization, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__contextual_harmonization, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(nask_be_t20, naskh_principle__contextual_harmonization, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(nask_be_t40, naskh_principle__contextual_harmonization, base_extractiveness, 40, 0.33).
narrative_ontology:measurement(nask_be_t60, naskh_principle__contextual_harmonization, base_extractiveness, 60, 0.37).
narrative_ontology:measurement(nask_be_t80, naskh_principle__contextual_harmonization, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(nask_be_t100, naskh_principle__contextual_harmonization, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__contextual_harmonization, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(nask_su_t20, naskh_principle__contextual_harmonization, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(nask_su_t40, naskh_principle__contextual_harmonization, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(nask_su_t60, naskh_principle__contextual_harmonization, suppression_requirement, 60, 0.34).
narrative_ontology:measurement(nask_su_t80, naskh_principle__contextual_harmonization, suppression_requirement, 80, 0.36).
narrative_ontology:measurement(nask_su_t100, naskh_principle__contextual_harmonization, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__contextual_harmonization, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__contextual_harmonization, 0.1).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the naskh principle' per the ε-invariance rule. classical_abrogation (chronological supersession, higher legal predictability, lower interpretive flexibility) and progressive_restriction (pedagogical trajectory, intermediate flexibility) are separate constraint stories with their own ε, beneficiaries, and victims. All three are linked bidirectionally through affects_constraints and share the same underlying kernel_id (naskh_principle) with distinct reading_ids in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__contextual_harmonization, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
