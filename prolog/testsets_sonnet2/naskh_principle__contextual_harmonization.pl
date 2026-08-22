% ============================================================================
% CONSTRAINT STORY: naskh_principle__contextual_harmonization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: naskh_principle__contextual_harmonization
 *   human_readable: Contextual Harmonization Reading of Naskh (No Textual Cancellation)
 *   domain: religious/legal/interpretive
 *
 * SUMMARY:
 *   This story instantiates the contextual harmonization reading of the naskh
 *   (abrogation) kernel within Islamic legal theory. Under this reading, no
 *   Quranic verse is ever cancelled by a later one; apparent contradictions
 *   are resolved by specifying the distinct revelatory circumstances (asbab
 *   al-nuzul) each verse addresses, so all verses retain permanent
 *   theological validity and situational legal potential. This is
 *   structurally distinct from the classical_abrogation reading (later verses
 *   definitively cancel earlier ones by chronology) and from the
 *   progressive_restriction reading (revelation moves
 *   permissive-to-restrictive as pedagogy, not cancellation or pure
 *   contextualization). Each reading has a different beneficiary/victim
 *   structure and a different ε: classical_abrogation offers definitive
 *   closure at the cost of treating some verses as textually dead;
 *   contextual_harmonization offers full textual vitality at the cost of
 *   interpretive indeterminacy and continuous reliance on specialist
 *   mediation; progressive_restriction offers a directional theological
 *   narrative that this reading's proponents tend to absorb into generic
 *   contextualism, erasing its distinct pedagogical claim.
 *
 * KEY AGENTS:
 *   - contextualist_jurists: primary agenda-setters (institutional/arbitrage) — administer the specification method and its authority
 *   - reformist_theologians: primary beneficiaries (organized/constrained) — gain adaptive legitimacy without conceding scriptural error
 *   - litigants_seeking_definitive_rulings: primary targets (powerless/trapped) — bear the cost of interpretive delay and indeterminacy
 *   - classical_school_jurists: institutional payers (powerful/identity_locked) — lose the authority to declare questions permanently settled
 *   - comparative_religion_scholars: analytical observers — document institutional function without adjudicating theology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__contextual_harmonization, 0.42).
domain_priors:suppression_score(naskh_principle__contextual_harmonization, 0.38).
domain_priors:theater_ratio(naskh_principle__contextual_harmonization, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, extractiveness, 0.42).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__contextual_harmonization, tangled_rope).
narrative_ontology:human_readable(naskh_principle__contextual_harmonization, "Contextual Harmonization Reading of Naskh (No Textual Cancellation)").
narrative_ontology:topic_domain(naskh_principle__contextual_harmonization, "religious/legal/interpretive").

domain_priors:requires_active_enforcement(naskh_principle__contextual_harmonization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__contextual_harmonization, '99fccd77-7231-45d0-a562-ebd9040b0b10').
narrative_ontology:cs_kernel_codification('99fccd77-7231-45d0-a562-ebd9040b0b10', fixed_text).
narrative_ontology:cs_authority_grounding('99fccd77-7231-45d0-a562-ebd9040b0b10', lineage).
narrative_ontology:cs_interpretation_layer_present('99fccd77-7231-45d0-a562-ebd9040b0b10').
narrative_ontology:cs_reading_relation('99fccd77-7231-45d0-a562-ebd9040b0b10', naskh_principle__classical_abrogation, forecloses).
narrative_ontology:cs_reading_relation('99fccd77-7231-45d0-a562-ebd9040b0b10', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('99fccd77-7231-45d0-a562-ebd9040b0b10', foundational, no_verse_is_ever_textually_cancelled).
narrative_ontology:cs_axiom_status(no_verse_is_ever_textually_cancelled, holdable).
narrative_ontology:cs_axiom_grounding('99fccd77-7231-45d0-a562-ebd9040b0b10', no_verse_is_ever_textually_cancelled, deontological).
narrative_ontology:cs_axiom('99fccd77-7231-45d0-a562-ebd9040b0b10', secondary, contradiction_signals_unspecified_context_not_conflict).
narrative_ontology:cs_axiom_status(contradiction_signals_unspecified_context_not_conflict, holdable).
narrative_ontology:cs_axiom_grounding('99fccd77-7231-45d0-a562-ebd9040b0b10', contradiction_signals_unspecified_context_not_conflict, conventional).
narrative_ontology:cs_reference_frame('99fccd77-7231-45d0-a562-ebd9040b0b10', classical_naskh_chronological_priority).
narrative_ontology:cs_drift_state('99fccd77-7231-45d0-a562-ebd9040b0b10', contemporary_reformist_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('99fccd77-7231-45d0-a562-ebd9040b0b10', '').
narrative_ontology:cs_kernel_id(naskh_principle__contextual_harmonization, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, contextualist_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, reformist_theologians).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, muslim_communities_seeking_flexible_rulings).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, litigants_seeking_definitive_rulings).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, classical_school_jurists).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, lay_believers_needing_clear_guidance).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, divine_textual_inerrancy).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, quranic_internal_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars who develop and apply asbab al-nuzul (occasions of revelation) analysis to preserve every verse's applicability by specifying the circumstances under which it governs. They administer the interpretive method itself, choosing which contextual factors count as relevant, and their authority rests on this method remaining the accepted way to handle textual tension. They can move between jurisdictions and schools that recognize their interpretive approach.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, contextualist_jurists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__contextual_harmonization, contextualist_jurists, beneficiary).

% Use the contextual harmonization framework to argue that verses read as harsh or restrictive by classical schools were addressed to specific historical circumstances and should not bind identically today. They gain theological legitimacy for adaptive readings without having to claim any verse was ever wrong or superseded, but their arguments only work within communities that accept the harmonization method as valid.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, reformist_theologians, beneficiary,
    organized, generational, constrained, global).

% Ordinary believers and local religious authorities who want rulings responsive to changed circumstances (finance, family law, criminal justice) without abandoning belief in the Quran's complete validity. They benefit from having a theologically respectable path to contextual adaptation, but must rely on contextualist jurists to perform the specification work for them.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, muslim_communities_seeking_flexible_rulings, beneficiary,
    moderate, biographical, constrained, national).

% Individuals before religious courts or seeking fatwa on concrete disputes (inheritance, divorce, contracts) who need one applicable rule now. Under this reading, resolving apparent contradiction requires establishing which specific context governs their case, which can be contested, slow, and dependent on which jurist is consulted. They bear the cost of interpretive indeterminacy directly and cannot appeal past the interpretive apparatus.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, litigants_seeking_definitive_rulings, payer,
    powerless, immediate, trapped, local).

% Scholars whose authority and school tradition rest on chronological abrogation (naskh) as settled doctrine — a later verse definitively cancels an earlier one, closing the legal question. The contextual harmonization reading undermines their claim to have definitively settled which rulings currently apply, forcing re-litigation of matters their tradition treated as resolved centuries ago. Their institutional and professional identity is bound to the abrogation framework, making exit from their position costly to their standing within their own school.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, classical_school_jurists, payer,
    powerful, civilizational, identity_locked, global).

% Everyday practitioners who want to know simply 'what does Islam say about X' and instead receive contextually qualified answers requiring specialized knowledge of revelation circumstances to apply. They pay in cognitive and practical burden — needing scholarly mediation for questions the abrogation model would have answered directly.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, lay_believers_needing_clear_guidance, payer,
    powerless, biographical, trapped, local).

% Hold the sibling reading that revelation progressively restricted permissions as pedagogy rather than either cancelling verses or merely contextualizing them. Within a contextual-harmonization-dominated discourse their pedagogical-progression account is treated as a variant of contextualism rather than a distinct claim, so their specific structural argument about directional divine pedagogy is rarely engaged on its own terms.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, progressive_restriction_theologians, excluded,
    organized, generational, constrained, global).

% Study how different Islamic legal traditions handle textual tension without themselves being bound by any reading. They document how contextual harmonization functions institutionally — who gains interpretive authority, who loses definitive closure — without adjudicating the theological question.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__contextual_harmonization, diffuse).
narrative_ontology:fixing_cost_class(naskh_principle__contextual_harmonization, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a method for holding the entire Quranic corpus as simultaneously and permanently valid, resolving apparent legal contradictions by specifying the circumstances each verse addresses rather than declaring any verse cancelled — this coordinates the community's need for both textual completeness (nothing in scripture is void) and situational applicability (rulings can differ by context) within one interpretive framework.
% TRANSFER_FUNCTION: Moves interpretive authority and the power to declare 'which context governs this case' from classical chronological-abrogation jurists (who could close a question definitively by citing revelation order) to contextualist jurists (who must be consulted anew for each situational specification), while moving the burden of interpretive labor from institutions onto individual litigants and lay believers who need case-specific rulings.
% ABSENT_VOICES: Progressive-restriction theologians would object that collapsing their pedagogical-progression account into generic 'contextualism' erases a distinct claim about directional divine intent. Ordinary petitioners with urgent practical questions are rarely represented in the scholarly debate about method, even though they bear most of the cost of interpretive indeterminacy.
% DISAPPEARANCE_RATIONALE: If contextual harmonization vanished as an accepted interpretive method, classical abrogation would become the uncontested default for resolving apparent contradictions, definitive rulings would be reachable faster through chronological priority, but reformist theological arguments that rely on situational limitation of harsh verses would lose their primary textual mechanism, and communities that have built modern jurisprudence on contextual specification would need to re-derive their positions through either abrogation or progressive-restriction frameworks instead.
% FOUNDING_PROBLEM: Early jurists faced verses that appeared to contradict one another on the same legal topic (e.g., wine, warfare, inheritance shares) and needed a principled way to determine which ruling applies without declaring the Quran internally inconsistent or divinely mistaken.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars and historians of Islamic law, external to any single school's authority claims, corroborate that apparent contradiction among revealed rulings is a genuine textual feature requiring some resolution mechanism — the problem itself is not manufactured by any one interpretive camp, though which mechanism should resolve it remains disputed among the schools themselves rather than settled by outside attestation.
narrative_ontology:disappearance_verdict(naskh_principle__contextual_harmonization, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__contextual_harmonization, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__contextual_harmonization, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42 at interval end) rather than high: the coordination function is genuine — the community's need to hold scripture as fully valid is a real problem this method solves, not merely cover. But extraction rises over the interval as the specialist apparatus needed to perform contextual specification becomes more elaborate and more necessary, shifting cost from institutions onto individual petitioners who cannot resolve their own cases without mediation. Suppression is lower than extraction (0.38) because contextual harmonization does not typically use coercive enforcement against dissenting schools so much as institutional gatekeeping over which fatwa councils and legal systems recognize contextualist reasoning as authoritative. Theater ratio is low-moderate (0.22): most of the interpretive labor is substantively engaged with texts and circumstances, though a growing share involves performative citation of asbab al-nuzul to reach predetermined conclusions.
 *
 * PERSPECTIVAL GAP:
 *   From the contextualist jurist seat, this is a rope: a genuine, low-coercion solution to the problem of apparent scriptural contradiction, serving theological coherence. From the litigant seat facing a specific unresolved dispute, the same structure operates as extraction — indeterminate process, specialist gatekeeping, and delay where a definitive rule would have served just as well or better. From the classical jurist seat, it appears almost as a snare on their own authority: their capacity to close questions is displaced without their consent, and enforcement (institutional non-recognition of their abrogation rulings in contextualist-dominated bodies) is real even though it is soft. The engine should register these as genuinely different computed types from the same structural data, not as disagreement to be averaged away.
 *
 * DIRECTIONALITY LOGIC:
 *   Contextualist jurists sit near the beneficiary end: they administer the method, their authority depends on it remaining the accepted framework, and they can move across jurisdictions that recognize contextualist reasoning (arbitrage exit). Reformist theologians and communities seeking flexibility are moderate beneficiaries — real benefit, but dependent on jurists to do the specification work, so directionality is not as favorable as the jurists' own. Litigants and lay believers are near the full-target end: powerless, trapped or immediate-horizon, they bear the indeterminacy cost directly and cannot exit the interpretive apparatus to get a faster answer. Classical school jurists are payers despite high nominal power — their power is institutional and civilizational in horizon but their exit is identity-locked: abandoning the abrogation framework would cost their own school's coherence and their personal standing within it, so their high power does not translate into low directionality the way it does for the contextualist jurists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling apparently contradictory revealed rulings — remains live; this is not a case of an arrangement outliving its function. What has shifted is who performs the reconciling work and at what cost to whom. Classifying this as tangled_rope rather than pure rope or pure snare prevents two errors: treating the method as costless pure coordination (ignoring the real burden shifted onto litigants and the real authority loss to classical jurists) and treating it as pure extraction with no coordination content (ignoring that the underlying problem of textual coherence is genuine and unresolved by fiat). The tangled_rope classification holds both facts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_vs_ad_hoc_rescue,
    'Is the contextual specification method a principled hermeneutic (each verse genuinely addresses a distinguishable historical circumstance) or an ad hoc device deployed whenever a contradiction needs resolving in a preferred direction?',
    'Historical-critical review of asbab al-nuzul literature for internal consistency: does the specification criterion apply symmetrically across cases, or only when the desired ruling requires it? A pattern of asymmetric application would indicate ad hoc rescue rather than principled method.',
    'If principled, the coordination function is robust and extraction is closer to necessary interpretive cost; if ad hoc, the method functions primarily as cover for jurist discretion, and the story would sit closer to snare than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_vs_ad_hoc_rescue, empirical, 'Whether contextual specification is applied consistently or opportunistically.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the contextual_harmonization reading diverge from progressive_restriction — is the difference substantive (directional pedagogy vs. static contextual scope) or merely terminological (both describe the same interpretive moves with different vocabulary)?',
    'Close comparison of specific case rulings (e.g., wine consumption, slavery, warfare permissions) under each reading: if the readings produce different legal outcomes for the same verse-pairs, the divergence is substantive; if they produce identical outcomes via different narrative framing, the divergence is largely rhetorical.',
    'A substantive divergence supports treating these as genuinely distinct sibling constraints with different beneficiary/victim structures, as done here; a merely rhetorical divergence would suggest the two readings should be merged or the distinction is not doing real classificatory work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether contextual_harmonization and progressive_restriction are structurally distinct or notationally distinct.').

omega_variable(
    authority_displacement_measurement,
    'How much actual jurisdictional and institutional authority has shifted from classical abrogation-based schools to contextualist bodies over the last century, versus how much is claimed shift without corresponding institutional reality?',
    'Track which national and transnational fatwa councils, sharia courts, and educational institutions formally adopt contextualist reasoning as primary method versus retain abrogation-based instruction, over a defined period.',
    'High measured institutional shift supports the rising extraction and suppression trajectories authored in the measurements; low measured shift would suggest the temporal drift is overstated and the constraint is more stable than authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_displacement_measurement, empirical, 'Whether authority displacement from classical to contextualist jurists is institutionally real or rhetorically asserted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__contextual_harmonization, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__contextual_harmonization, theater_ratio, 0, 0.12).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__contextual_harmonization, theater_ratio, 20, 0.14).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__contextual_harmonization, theater_ratio, 40, 0.16).
narrative_ontology:measurement(nask_tr_t60, naskh_principle__contextual_harmonization, theater_ratio, 60, 0.18).
narrative_ontology:measurement(nask_tr_t80, naskh_principle__contextual_harmonization, theater_ratio, 80, 0.2).
narrative_ontology:measurement(nask_tr_t100, naskh_principle__contextual_harmonization, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__contextual_harmonization, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(nask_be_t20, naskh_principle__contextual_harmonization, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(nask_be_t40, naskh_principle__contextual_harmonization, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(nask_be_t60, naskh_principle__contextual_harmonization, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(nask_be_t80, naskh_principle__contextual_harmonization, base_extractiveness, 80, 0.41).
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
% This story is one of three sibling readings of the naskh_principle kernel. classical_abrogation authors high definitiveness/high textual-cancellation cost; contextual_harmonization (this file) authors moderate extraction with full textual vitality but interpretive indeterminacy cost; progressive_restriction authors a directional-pedagogy account this reading's proponents tend to subsume. Each carries its own ε, beneficiaries, and victims per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
