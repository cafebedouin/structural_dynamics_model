% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Precedent as Binding Backward Constraint
 *   domain: legal_theory/jurisprudence
 *
 * SUMMARY:
 *   Strict stare decisis is one reading of the common law precedent corpus:
 *   the principle that precedent binds as a backward constraint and departure
 *   requires extraordinary justification. Under this reading, judges are
 *   understood as constrained by accumulated holdings; litigants face narrow
 *   pathways to challenge established doctrine; and the accumulation of
 *   precedent acts as an increasingly rigid skeleton constraining legal
 *   evolution. This is contrasted with two sibling readings: the
 *   evolutionary_framework (which treats precedent as providing an adaptive
 *   template within which contemporary normative development occurs) and the
 *   pluralist_balancing reading (which weights precedent variably by domain
 *   and context rather than applying a uniform constraint). The
 *   strict_stare_decisis reading instantiates maximum rigidity: it treats
 *   precedent as a backward-reaching constraint whose overruling is rare,
 *   contested, and requires an extraordinarily high threshold of
 *   justification. This constraint story models the strict reading's
 *   structural properties and the extraction it creates for litigants and
 *   groups locked by adverse precedent.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: controls the threshold for extraordinary justification and thus monopolizes decisions to overrule — powerful institutional seat with mobile exit options
 *   - litigants_challenging_precedent: bear the cost of narrow pathways; constrained exit — moderate power
 *   - marginalized_groups_locked_by_adverse_precedent: identity-locked to the legal system; depend on others to mount overruling challenges — powerless, highest extraction
 *   - legal_stability_beneficiaries: commercial and institutional actors who rely on precedential stability for planning and contracting — organized, broad exit
 *   - conservative_legal_scholars: defend stare decisis as rule-of-law requirement; influential in legal discourse — powerful institutional influence
 *   - progressive_legal_scholars: critique stare decisis as entrenching historical injustices; less structurally advantaged by the constraint — powerful but constrained by the constraint itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.68).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.72).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Precedent as Binding Backward Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal_theory/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, '9947428e-b2b9-4eab-bc5d-a0b6febd3da3').
narrative_ontology:cs_kernel_codification('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', distributed).
narrative_ontology:cs_authority_grounding('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', lineage).
narrative_ontology:cs_interpretation_layer_present('9947428e-b2b9-4eab-bc5d-a0b6febd3da3').
narrative_ontology:cs_reading_relation('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', foundational, precedent_binds_as_backward_constraint).
narrative_ontology:cs_axiom_status(precedent_binds_as_backward_constraint, holdable).
narrative_ontology:cs_axiom_grounding('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', precedent_binds_as_backward_constraint, conventional).
narrative_ontology:cs_axiom('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', foundational, extraordinary_justification_required_for_overruling).
narrative_ontology:cs_axiom_status(extraordinary_justification_required_for_overruling, holdable).
narrative_ontology:cs_axiom_grounding('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', extraordinary_justification_required_for_overruling, deontological).
narrative_ontology:cs_reference_frame('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', stable_precedential_hierarchy).
narrative_ontology:cs_drift_state('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', contemporary_reform_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9947428e-b2b9-4eab-bc5d-a0b6febd3da3', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, institutional_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, doctrine_stability_vindication).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_challenging_precedent).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, marginalized_groups_locked_by_adverse_precedent).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, legal_stability_beneficiaries).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, conservative_legal_scholars).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, progressive_legal_scholars).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, law_reform_advocates).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_requires_stability).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_constraint_prevents_arbitrary_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies precedent; sets the threshold for 'extraordinary justification' for overruling; controls the doctrinal machinery (distinguishing, narrowing, anchoring) through which precedent is managed. Benefits institutionally from the constraint's stability — it provides a framework within which judges can claim to be rule-bound rather than will-driven, and it monopolizes control over doctrinal change. Exit is constrained by professional norms and institutional role: judges cannot simply declare themselves unbound by precedent without delegitimizing judicial authority itself.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost of narrow pathways to doctrinal challenge. Must argue within the constraint's framework: changed circumstances, new evidence, reliance erosion, or fundamental error. Each ground is subject to appellate discretion and faces a high bar. If they fail to meet the threshold, they must accept the adverse precedent or pursue legislative reform. Their exit is constrained: they can abandon litigation but cannot escape the law's application; they can lobby legislatively but that is slower and more resource-intensive than litigation.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_challenging_precedent, payer,
    moderate, biographical, constrained, national).

% Systematically disadvantaged by precedent formed under historical conditions of unequal representation or now-repudiated normative assumptions. Example: precedent denying a group categorical protection, or interpreting constitutional rights narrowly in ways that exclude them. Their constraint-specific exit is identity-locked: they cannot opt out of legal subjection; they remain subject to the precedent's rule regardless of their preferences. They depend on finding litigants willing to mount overruling challenges and judges willing to meet those challenges with less than the standard extraordinary-justification bar.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, marginalized_groups_locked_by_adverse_precedent, payer,
    powerless, generational, identity_locked, national).

% Rely on stable, predictable law for planning and contracting: settled commercial parties, property owners, repeat institutional players. They benefit from the certainty the constraint provides and from rapid settlement of questions around established precedent. Their exit is mobile: they can relocate operations, restructure transactions to suit precedent, hedge against legal risk, or engage politically to defend favorable precedent.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_stability_beneficiaries, beneficiary,
    organized, generational, mobile, national).

% Defend stare decisis as essential to rule of law and judicial constraint. They argue that departing from precedent ad hoc permits courts to become vehicles for ideological preference and undermines the binding force of law. Their institutional position is strong: they shape legal education, publish in prestigious journals, and testify about constitutional principles. They benefit from the constraint's maintenance because it supports their jurisprudential vision. Exit is mobile: they can shift positions, publish in different forums, or move between institutional roles.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, conservative_legal_scholars, beneficiary,
    powerful, generational, mobile, national).

% Critique stare decisis as entrenching historical injustices and preventing adaptive jurisprudence that responds to evolved moral understanding. They argue that rigid precedent locks in errors made under different constitutional frameworks and prevents law from reflecting contemporary values. Their institutional position is strong but constitutively disadvantaged by the constraint: their arguments for doctrinal change must clear a higher bar than arguments for stability. Exit is mobile: they can shift advocacy strategies, work in legislative reform, or pivot to other professional roles.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, progressive_legal_scholars, payer,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, progressive_legal_scholars, observer).

% Seek to change law through litigation or political pressure. Litigation is hampered by stare decisis: they must demonstrate extraordinary justification even when underlying policy has shifted. Legislative reform is available but slower and more resource-intensive. Their voices in litigation are structurally constrained: they can attempt to challenge precedent, but the appellate court sets a high threshold, and many arguments lose at the threshold stage before reaching the merits. Exit is constrained: they must either accept the legislative path or abandon reform efforts.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, law_reform_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, law_reform_advocates, excluded).

% The normative proposition that law must be internally consistent and that systematic departures from established doctrine undermine the coherence and integrity of legal systems. This is not an agent but a claim vindicated by the constraint's operation: stare decisis doctrine is justified partly as protecting doctrinal integrity against ad hoc erosion. The constraint's persistence vindicates this principle in legal discourse.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, doctrine_stability_vindication, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(common_law_precedent_corpus__strict_stare_decisis, doctrine_stability_vindication).

% The constitutional principle that law is binding, predictable, and constrains arbitrary power. Strict stare decisis claims to instantiate this principle by making precedent binding and preventing judicial will from dominating outcomes. The constraint's legitimacy rests partly on its claimed connection to rule-of-law values. This is an analytical observer seat, not an agent that collects or pays.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_doctrine, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a procedural coordination problem: without treating prior holdings as binding, each litigant reargues settled questions, each judge re-decides closed disputes, and courts face endless relitigational burden and resource drain. Stare decisis permits courts to treat some questions as decided, reducing transaction costs and allowing parties to plan around stable law. It coordinates expectations about what law is across time and across courts.
% TRANSFER_FUNCTION: Transfers authority to determine when law can change from litigants and contemporary moral reasoning to appellate judges and historical holdings. It also transfers the costs of legal stability (inability to reform unjust precedent) from the institutional judiciary to litigants and marginalized groups burdened by adverse precedent. The constraint moves power over doctrinal evolution from dispersed political and moral argument to the judiciary's monopoly on extraordinary-justification determinations.
% ABSENT_VOICES: Litigants in lower courts, who are told that challenges to precedent are off-limits; marginalized groups locked by adverse precedent, whose legal subjection is sustained by holdings made when they lacked representation in the legal system; future generations, whose interests in legal reform are discounted by backward-reaching constraint on doctrinal change. These voices are structurally excluded because the constraint operates backward in time — it grants weight to prior decisions made by a differently composed judiciary and under different normative assumptions.
% DISAPPEARANCE_RATIONALE: If strict stare decisis disappeared and precedent became easily rebuttable on grounds of contemporary justice or changed circumstances, litigation dockets would flood with overruling challenges, settled expectations would unravel, and litigants would no longer be able to rely on stable law. Commercial parties, property owners, and repeat institutional players would face uncertainty. Conversely, many constraints that depend on now-repudiated precedent (e.g., precedent denying equal protection to specific groups) would lose their backward-reaching legitimacy and be immediately vulnerable to challenge. The legal system would reorganize around a different stability principle — perhaps requiring legislative codification of major doctrinal shifts, or super-majority override, or enhanced procedural requirements for overruling that preserve some stability without requiring extraordinary justification.
% FOUNDING_PROBLEM: In 18th- and 19th-century English common law, courts faced procedural instability: without treating prior decisions as binding, each judge relitigated settled questions, leading to inconsistent law, unpredictable outcomes, and heavy relitigation burden on courts. Stare decisis emerged as the principle that judges should respect prior holdings and depart only for compelling reasons, permitting law to become stable and predictable.
% FOUNDING_PROBLEM_CORROBORATION: Conservative legal scholars and the appellate judiciary affirm the founding problem is live: law must remain stable, and departing ad hoc undermines rule of law and creates procedural instability. Progressive legal scholars, law reform advocates, and empirical legal researchers attest the founding problem is substantially solved: modern appellate screening, docket management, written opinions, legislative codification, and precedent summaries make relitigation rare, and the constraint now persists primarily to insulate historical holdings from contemporary moral challenge. Independent historical and empirical scholarship supports the atrophy diagnosis: overruling is rare (fewer than 0.1% of decided cases), and the constraint tightens over time as precedent accumulates, suggesting the original procedural-stability function has been superseded by institutional inertia.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.68) reflects that the constraint transfers power from litigants to judges and from contemporary moral reasoning to historical holdings. It is not zero because stare decisis does generate real coordination value (stability, predictability, reduced relitigation). But that value is exceeded by the power asymmetry: judges set the threshold that determines who can challenge; litigants must clear that threshold; and the threshold itself is applied more stringently to challenges than to stability arguments. Suppression (0.72) is high because the constraint's persistence depends on active enforcement: judges must actively distinguish and narrow adverse precedent to prevent it from applying; they must actively defend the 'extraordinary justification' standard against erosion; and they must suppress alternative framings (e.g., that law should adapt to contemporary understanding) by treating them as insufficient grounds for overruling. Theater ratio (0.41) reflects that a growing share of the machinery serves to maintain constraint rigidity rather than the original coordination function: elaborate doctrinal scaffolding (distinction, anchoring, narrowing) exists primarily to prevent overruling, not to solve relitigation. Accessibility_collapse (0.79) is high: once a litigant understands that they face an extraordinary-justification burden, their alternatives collapse — litigation becomes an uphill fight, legislative routes become necessary, and identity-locked groups face near-zero exit. Resistance (0.58) is moderate: there is substantial scholarly and litigant resistance to strict stare decisis, but the constraint persists because it is embedded in institutional practice and judicial self-understanding. The measurement series show increasing extractiveness and suppression over the interval: as the precedent corpus accumulates, the constraint tightens, and the judicial machinery devoted to suppressing overruling becomes more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and stability-beneficiaries (conservative scholars, institutional actors) experience this constraint as a genuine and valuable coordination mechanism — it produces the predictability they depend on. Litigants, law reformers, and marginalized groups experience it as a rigid backward constraint that insulates historical injustices from challenge. From the judicial seat, stare decisis is a self-imposed rule that constrains judicial power and prevents arbitrary decision-making. From the litigant seat, it is a structure that denies them access to legal change even when circumstances have shifted dramatically. The engine computes these divergences from the structural data: judges have mobile exit (they can choose to depart, constrained by professional norms but not by material survival), while marginalized groups have identity-locked exit (they cannot opt out of legal subjection, only wait for others to mount overruling challenges). This structural divergence should produce different computed types across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The appellate judiciary sits at d near 0.0 (beneficiary): they benefit from the stability, control the rules, face professional-norm constraints but not material constraints on exit. Legal stability beneficiaries sit at d near 0.15-0.25 (slight beneficiary): they benefit substantially but cannot directly shape the constraint. Conservative scholars sit near 0.20 (slight beneficiary): they benefit from the constraint's persistence but face some professional-norm pressure to consider arguments for change. Litigants challenging precedent sit at d near 0.65 (partial target): they bear costs (procedural burden, likelihood of loss) and have constrained exit (they can abandon litigation or pursue legislative routes, but cannot simply ignore adverse precedent). Marginalized groups sit at d near 0.85 (strong target): they bear high costs, their exit is identity-locked, and they depend on others' willingness to mount overruling challenges. Progressive scholars sit at d near 0.50 (symmetric): they benefit from the constraint when defending their own preferred precedent but bear costs when attacking adverse precedent, and they have mobile exit (they can leave the legal profession, publish elsewhere, work in legislative reform).
 *
 * MANDATROPHY ANALYSIS:
 *   The strict_stare_decisis reading faces a strong mandatrophy hypothesis: the founding problem (re-relitigating settled questions leading to procedural instability) is substantially solved by modern docket management, written opinions, legislative codification, and appellate screening mechanisms. The constraint persists not because the founding problem is live but because it has become institutionalized and serves the judiciary's interest in constraining litigant access to doctrinal revision. This is a classic case where the coordination function has atrophied but the extraction mechanism (control over who can challenge precedent) remains highly functional. The theater ratio rising over the interval (from 0.18 to 0.41) is the key signal: the elaborate doctrinal machinery (distinguishing, anchoring, narrowing) exists primarily to suppress overruling, not to solve the original procedural-stability problem. A piton classification would be plausible if the constraint were entirely performative, but the extraction metrics are too high and suppression is too active for piton status. The constraint is better modeled as tangled_rope: it does generate real coordination value (stability is genuinely useful), but that value is incommensurate with the extraction imposed on litigants and marginalized groups, and the constraint persists through active suppression of alternatives (the high suppression score reflects judges actively maintaining the extraordinary-justification standard against erosion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_atrophy,
    'Is the founding problem (procedural instability from re-relitigating settled questions) still live, or has it been substantially solved by modern docket management and legislative codification, leaving the constraint''s persistence to depend on extractive institutional interest rather than coordination necessity?',
    'Empirical analysis of appellate docket patterns, burden on courts from doctrinal challenges, and comparison with jurisdictions using weaker stare decisis or legislative precedent codification. If jurisdictions with weaker stare decisis show no significant increase in procedural instability, the founding problem is substantially solved.',
    'If the founding problem is substantially solved, the classification shifts from tangled_rope (coordination + extraction) toward piton (atrophied coordination function, persistence through inertia and institutional interest). The theater ratio would become more diagnostic of atrophied function. Remedies would shift from defending the coordination principle toward reforming the suppression mechanism (lowering the extraordinary-justification bar).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_atrophy, empirical, 'Whether strict stare decisis solves an ongoing coordination problem or persists as institutional inertia.').

omega_variable(
    identity_lock_vs_constrained_exit,
    'For marginalized groups burdened by adverse precedent, is their constraint-specific exit truly identity-locked (they cannot exit subjection to the law) or is it constrained (they can theoretically exit through legislative change, institutional migration, or social movement organizing)?',
    'Tracing exit pathways for historically marginalized groups: How many have successfully used legislative reform to overturn adverse precedent? How much institutional/organizational power must be mobilized? Does social movement organizing reduce the time to legislative victory? The boundary between identity_locked and constrained depends on whether exit is theoretically available (constrained) or structurally impossible (identity_locked).',
    'If identity-locked: the extraction on marginalized groups is higher and the constraint''s typology shifts toward pure snare at that seat (structural coercion with near-zero exit). If constrained: extraction is lower (exit is costly but possible), and the constraint remains tangled_rope at that seat (coordination + constrained extraction). The directionality of marginalized groups would shift upward (higher d) if identity-locked, further amplifying the per-seat divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Whether marginalized groups face constrained or identity-locked exit from adverse precedent.').

omega_variable(
    judicial_autonomy_vs_doctrine_instrumentalization,
    'Do judges experience stare decisis as a genuine constraint on their power (a self-imposed rule that prevents arbitrary decision-making) or as an instrument they control (a doctrine they invoke when defending preferred precedent and relax when overruling serves their policy preferences)?',
    'Comparative study of overruling patterns: Do judges apply the extraordinary-justification standard equally to challenges from all ideological directions? Do they distinguish or narrow precedent differently depending on whether the precedent aligns with contemporary judicial preferences? If the bar is applied asymmetrically, the constraint is instrumentalized rather than genuinely constraining.',
    'If genuinely constraining: judges are truth-telling about their structural position, and the constraint merits classification as a coordination mechanism that constrains power. If instrumentalized: the constraint is a cover story for selective doctrinal authority, extraction is higher (judges extract discretion while claiming constraint), and the classification shifts toward snare (coordination frame conceals selective extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_autonomy_vs_doctrine_instrumentalization, empirical, 'Whether stare decisis constrains judges or serves as an instrument for selective authority.').

omega_variable(
    kernel_reading_underspecification,
    'Within the strict_stare_decisis reading itself, what counts as ''extraordinary justification'' for overruling? How does the judiciary operationalize this standard, and does the standard itself remain stable or does it drift over time?',
    'Systematic analysis of overruling decisions: What reasons do courts give? How often do courts identify changed circumstances, new evidence, or reliance-interest erosion as sufficient? Does the standard''s stringency vary by topic (constitutional vs. statutory precedent, civil rights vs. commercial law) or over time periods? If the standard drifts or varies, it is underspecified within the reading itself.',
    'If the standard is drifting toward leniency: the strict reading is eroding from within, and the constraint is transitioning toward the pluralist_balancing reading (precedential weight varies by domain and context). If the standard is stable, the strict reading is coherent. If the standard is drifting toward stringency: the constraint is tightening and theater ratio is rising (increasing effort to suppress overruling), confirming mandatrophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underspecification, empirical, 'Whether the strict_stare_decisis reading''s central standard (extraordinary justification) remains stable or drifts.').

omega_variable(
    committer_reading_contest,
    'Is the strict_stare_decisis reading genuinely contestable with the evolutionary_framework and pluralist_balancing readings, or does the strict reading have an institutional advantage that forecloses the others in practice?',
    'Institutional analysis of which reading dominates appellate jurisprudence, legal education, and bar admission standards. If the strict reading is taught as canonical and overruling is presented as an exceptional deviation, the other readings face suppression that goes beyond normal academic debate. Surveying law schools, comparing constitutional law textbooks, analyzing Supreme Court framing of precedent.',
    'If the strict reading has foreclosed the others institutionally (not logically): the kernel contest is not really a three-way live debate but a suppressed contest where one reading is naturalized and others are marginalized. This would shift the classification of the constraint itself: it is not merely a coordination mechanism but an enforcement mechanism maintaining one reading''s dominance over contested alternatives. Classification consequences: the constraint becomes more snare-like (enforced through institutional suppression of alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_contest, conceptual, 'Whether strict_stare_decisis is one live reading among three or has achieved institutional suppression of the other readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t8, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(comm_tr_t8, observed).
narrative_ontology:measurement(comm_tr_t16, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 16, 0.3).
narrative_ontology:measurement_basis(comm_tr_t16, observed).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(comm_tr_t24, observed).
narrative_ontology:measurement(comm_tr_t32, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 32, 0.39).
narrative_ontology:measurement_basis(comm_tr_t32, observed).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(comm_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t8, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(comm_be_t8, observed).
narrative_ontology:measurement(comm_be_t16, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(comm_be_t16, observed).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(comm_be_t24, observed).
narrative_ontology:measurement(comm_be_t32, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(comm_be_t32, observed).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comm_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t8, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(comm_su_t8, observed).
narrative_ontology:measurement(comm_su_t16, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(comm_su_t16, observed).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(comm_su_t24, observed).
narrative_ontology:measurement(comm_su_t32, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(comm_su_t32, observed).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(comm_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__strict_stare_decisis, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, constitutional_originalism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, legal_stare_decisis_erosion).

% DUAL FORMULATION NOTE:
% Strict_stare_decisis is one reading of the common_law_precedent_corpus kernel. The evolutionary_framework reading (sibling story) treats precedent as permitting adaptive reinterpretation within a stable template; the pluralist_balancing reading weights precedential rigidity variably by domain. All three stories instantiate different ε values, beneficiary/victim structures, and computed types from the SAME kernel authority structure. The strict reading produces the highest rigidity and extraction; the evolutionary reading distributes rigidity across interpretation layers; the pluralist reading allows context-specific variation. Readers analyzing this kernel should consume all three stories and the network connections to understand the structure of the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__strict_stare_decisis, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
