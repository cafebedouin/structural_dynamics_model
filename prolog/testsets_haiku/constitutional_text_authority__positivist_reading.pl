% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Constitutional Validity via Procedural Enactment (Positivist Reading)
 *   domain: constitutional_law/legal_theory
 *
 * SUMMARY:
 *   The positivist reading of constitutional authority claims that
 *   constitutional validity derives from formal enactment procedures
 *   (ratification, amendment mechanisms) and institutional sources
 *   (legislatures, courts functioning within their proper roles), not from
 *   moral content or justice principles. The law/morality distinction is
 *   maintained as a foundational divide: what makes something constitutional
 *   law is its source in formal procedure, not whether it is morally good.
 *   This reading competes with originalism (which anchors authority in
 *   historical public understanding at ratification) and living
 *   constitutionalism (which allows meaning to evolve with contemporary moral
 *   understanding). The positivist reading is instantiated here as a single
 *   constraint story capturing this one interpretive frame in isolation. The
 *   sibling readings are separate constraints linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - professional_judiciary: agenda-setter, institutional power — enforces the positivist frame by declaring moral arguments extra-constitutional
 *   - formalist_legal_doctrine: non-agent beneficiary — methodology that depends on text-and-procedure-centered interpretation
 *   - moral_reasoning_constituencies: payer, organized power — civil rights advocates, ethicists, reform movements excluded from authoritative constitutional voice
 *   - originalist_judges: observer seat with secondary benefit — collaborate on text-fidelity but diverge on grounding
 *   - living_constitutionalist_judges: excluded, institutional power — would argue for moral-content evolution but are kept from appellate authority
 *   - legislatures: observer with mobile exit — remain the legitimate forum for moral arguments
 *   - legal_academia: observer with mobile exit — teaches and critiques the positivist orthodoxy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.42).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.38).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Validity via Procedural Enactment (Positivist Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "constitutional_law/legal_theory").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, 'cf0b96e9-283c-4074-ba52-2b30a42d6a3a').
narrative_ontology:cs_kernel_codification('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', formalized).
narrative_ontology:cs_authority_grounding('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', extraction).
narrative_ontology:cs_interpretation_layer_present('cf0b96e9-283c-4074-ba52-2b30a42d6a3a').
narrative_ontology:cs_reading_relation('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', foundational, legal_positivism_supremacy).
narrative_ontology:cs_axiom_status(legal_positivism_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', legal_positivism_supremacy, conventional).
narrative_ontology:cs_axiom('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', foundational, law_morality_distinction_irreducible).
narrative_ontology:cs_axiom_status(law_morality_distinction_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', law_morality_distinction_irreducible, deontological).
narrative_ontology:cs_reference_frame('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', formal_enactment_procedure_authority).
narrative_ontology:cs_drift_state('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', contemporary_moral_critique_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cf0b96e9-283c-4074-ba52-2b30a42d6a3a', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, professional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, formalist_legal_doctrine).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, moral_reasoning_constituencies).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, extra_textual_reform_movements).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint benefits institutional judges and formalist doctrine without producing obviously unfair outcomes—the extraction is the relocation of moral argument from courts to legislatures, a subtle institutional shift rather than overt coercion. Suppression is lower than extraction (0.38) because moral-reasoning constituencies retain legislative access and academic spaces; courts are not the only interpretive venue. Theater ratio is low (0.28) because the formal procedure claim is genuinely functional—positivist interpretation does produce predictable, rule-governed outcomes. Accessibility collapse is high (0.71) because once the positivist frame is established in law schools and courts, alternatives (moral reasoning, natural law, living constitutionalism) appear outside constitutional law proper—they seem unavailable within the legal frame. Resistance is substantial (0.62) because moral advocates, living constitutionalists, and scholars mount continuous, organized opposition to the positivist constraint. The measurement series show slow growth in extractiveness and suppression over 75 years: as the positivist frame became institutionalized, enforcement tightened and the cost to moral-reasoning advocates rose. Theater ratio climbed alongside, indicating increasing performativity in maintaining the law/morality distinction against sustained critique.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary seat and the moral-reasoning-constituencies seat compute radically differently. From the judiciary's position (institutional, constrained, beneficiary), the positivist constraint is a genuine coordination solution—it makes law predictable and shields judges from accusations of imposing their morality. From the moral-constituency position (organized, constrained, payer), the same constraint is extractive suppression—their reasoning is declared illegitimate and relocated to a forum where their voice is weaker. The originalist-judges observer seat shows a third perspective: they benefit from the text-fidelity enforcement but reject the positivist grounding, diverging on whether historical understanding or formal procedure is the true source. The engine computes each seat's type from the structural data (power, exit, beneficiary/victim role); the divergence between seats is the story's signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (judiciary, formalist doctrine) sit at d near the beneficiary end because they gain institutional authority and methodological stability from the constraint without bearing its costs. Victims (moral-reasoning constituencies) sit at d near the target end because they are suppressed (kept from authoritative voice in courts) and constrained (legislative access is their only resort, and legislatures have different incentives). Originalist judges occupy an unusual seat: they are beneficiary-proximate on text-fidelity (d~0.25, beneficiary-ish) but diverge on grounding—the engine will compute their d from the structural data (institutional power, constrained exit, observer role, secondary beneficiary designation) and may differ from the pure-beneficiary judiciary seat. Legislatures have mobile exit and benefit from moral-argument receptiveness, so they sit near symmetric or beneficiary (d~0.3). Living constitutionalist judges are excluded, which keeps them from seat-specific d assignment but marks them in the narrative as the would-be opposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was judicial discretion under ambiguous constitutional text. The positivist solution claims to eliminate discretion by anchoring authority in formal procedure rather than moral judgment. However, the constraint may exhibit mandatrophy dynamics: as the positivist frame became entrenched (1950–2025), it acquired defensive institutional interests (judges, law schools, professional doctrine) that benefit from its maintenance regardless of whether it solves the original discretion problem. The measured theater_ratio climb (0.08 to 0.28) suggests increasing performativity in maintaining the law/morality distinction. The suppression_requirement growth (0.20 to 0.38) indicates that enforcement machinery must intensify to keep alternative interpretive frames out as internal critique mounts. The founding problem may be dead (courts are now predictable, perhaps overly so), but the constraint persists because formalist institutional interests are vested in it. This is a candidate for mandatrophy: the constraint solved a real problem and persists by institutional inertia after the problem's salience faded. Commentary notes this; the engine will compute the type from structural data and metrics independently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_vs_enforced_orthodoxy,
    'Is the positivist constraint a genuinely neutral procedural frame, or is the claim of neutrality itself a moral/political choice that favors institutional actors who benefit from formal procedure?',
    'Comparative jurisprudence: examine whether courts following non-positivist frameworks (natural law, living constitutionalism) exhibit less predictable or less fair outcomes. Philosophical analysis: articulate what work the law/morality distinction does and whom it serves.',
    'If neutrality is genuine, the constraint solves judicial discretion without extractive cost. If the neutrality claim is itself contestable, the constraint is a moral-grounded institutional choice defended as procedure, elevating extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_vs_enforced_orthodoxy, conceptual, 'Whether positivism is a discovered neutral procedure or an enforced moral choice about authority.').

omega_variable(
    natural_law_vs_procedural_authority,
    'Does the positivist constraint foreclose natural law grounding of constitutional authority, or do the two coexist in practice as different rationales for the same text?',
    'Doctrinal analysis: trace whether originalist judges (who accept positivism on text-fidelity) also invoke natural law or higher law reasoning. Institutional history: examine whether the constraint''s enforcement ever requires judges to explicitly reject natural law foundations.',
    'If foreclosed: the positivist and originalist readings represent genuine logical alternatives. If coexisting: the readings differ in philosophical grounding but permit substantial convergence in practice, reducing the foreclosure relation to a rhetorical distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_procedural_authority, empirical, 'Whether positivism forecloses or coexists with natural law constitutional reasoning.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of moral-reasoning constituencies structural (they are kept from courts by procedure and institutional capture) or internalized (they come to believe moral arguments are improper in constitutional law)?',
    'Track whether moral-reasoning advocates in academia and activism maintain their voice undiminished outside the judiciary. Observe whether law-school graduates trained in positivism resist moral arguments reflexively or only when constrained by institutional rules.',
    'If structural, the constraint is an institutional gate; if internalized, the suppression persists even when the external barrier is removed, indicating deeper capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether moral-argument suppression is structural or internalized in legal professionals.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the positivist reading logically foreclose living constitutionalism within a single framework, or do the two coexist as different parties'' incompatible but simultaneously-held positions?',
    'Philosophical: examine whether a single judge or legal system could coherently hold both positivist and living-constitutionalist commitments. Institutional: trace whether the two interpretive schools occupy distinct court seats or represent genuine internal contradiction.',
    'If foreclosed: the sibling reading_relations entry is ''forecloses''. If coexisting: the entry is ''coexists_with''. This determines the kernel''s internal logical structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether positivism and living constitutionalism are logically incompatible or simultaneously-held positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1950, constitutional_text_authority__positivist_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(cons_tr_t1975, constitutional_text_authority__positivist_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(cons_tr_t1990, constitutional_text_authority__positivist_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(cons_tr_t2005, constitutional_text_authority__positivist_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(cons_tr_t2015, constitutional_text_authority__positivist_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(cons_tr_t2025, constitutional_text_authority__positivist_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_text_authority__positivist_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(cons_be_t1975, constitutional_text_authority__positivist_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(cons_be_t1990, constitutional_text_authority__positivist_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(cons_be_t2005, constitutional_text_authority__positivist_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(cons_be_t2015, constitutional_text_authority__positivist_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(cons_be_t2025, constitutional_text_authority__positivist_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_text_authority__positivist_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(cons_su_t1975, constitutional_text_authority__positivist_reading, suppression_requirement, 1975, 0.28).
narrative_ontology:measurement(cons_su_t1990, constitutional_text_authority__positivist_reading, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement(cons_su_t2005, constitutional_text_authority__positivist_reading, suppression_requirement, 2005, 0.36).
narrative_ontology:measurement(cons_su_t2015, constitutional_text_authority__positivist_reading, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(cons_su_t2025, constitutional_text_authority__positivist_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__positivist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The kernel 'constitutional_text_authority' decomposes into three readings, each a distinct constraint with different ε values and structural properties. The positivist reading claims authority derives from formal procedure (procedural constraint, moderate extraction). The originalist reading claims authority derives from historical public understanding at ratification (historical-evidential constraint, low extraction if correctly applied, higher if used as pretense for preferred outcomes). The living-constitutionalist reading allows authority to track contemporary moral understanding (moral-reasoning constraint, higher extraction potential because moral reasoning is contestable). All three share the same kernel (the Constitution), but their ε values differ substantially: positivism relies on procedure stability (moderate extraction if the procedure is actually neutral), originalism relies on historical accuracy (extraction risk if historical evidence is contested or selective), living constitutionalism relies on moral consensus (high extraction risk because consensus is unstable). These are NOT the same constraint viewed from different angles; they are three structurally distinct claims about what grounds constitutional validity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__positivist_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
