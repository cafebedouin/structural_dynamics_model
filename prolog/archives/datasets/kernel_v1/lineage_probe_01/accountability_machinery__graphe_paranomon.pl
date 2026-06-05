% ============================================================================
% CONSTRAINT STORY: accountability_machinery__graphe_paranomon
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_accountability_machinery__graphe_paranomon, []).

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
 *   constraint_id: accountability_machinery__graphe_paranomon
 *   human_readable: Graphe Paranomon: Athenian Accountability Through Proposer Liability
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The graphe paranomon (action for proposing unlawful measures) was Athens'
 *   mechanism for binding the democracy to its own laws through individual
 *   proposer liability. An orator who persuaded the Assembly to pass a
 *   measure later judged unlawful faced prosecution, with personal liability
 *   rather than collective punishment. This constraint exemplifies how
 *   democracy self-binds through its orators — the system makes persuaders
 *   individually accountable for the legality of their own advocacy. The
 *   mechanism is a reading of the broader kernel of accountability machinery;
 *   it contrasts with euthynai (universal auditing of officials) and
 *   ostracism (blunt exile votes) as alternative constitutional approaches to
 *   the same problem: how to constrain Assembly power while preserving
 *   democratic decision-making. The graphe is neither pure coordination
 *   (rope) nor pure extraction (snare) but a hybrid that genuinely
 *   coordinates legal stability while extracting a suppressive cost from bold
 *   initiative. Theater has risen over time as prosecution became
 *   ritualistic, while the foundational rule remained formally enforceable.
 *
 * KEY AGENTS:
 *   - Bold Initiators / Ambitious Orators: Primary victims (powerless/trapped or moderate/constrained) — bear legal jeopardy for persuasion that later proves unlawful; their exit options are limited by career stakes
 *   - Assembly Collective: Primary beneficiary (institutional/arbitrage) — experiences the constraint as coordination that ensures legality of its own decisions; net beneficiary of the stability the constraint provides
 *   - Decisional Stability / Rule of Law: Abstract beneficiary (institutional/arbitrage) — the systemic benefit of holding individual persuaders accountable for legal compliance
 *   - Established Political Dynasties: Secondary beneficiary (powerful/mobile) — can bear prosecution risk better due to superior legal resources and reputation; experience the constraint as manageable
 *   - Democratic Reformers / Young Politicians: Secondary victim (moderate/constrained) — disproportionately targeted if prosecution is biased; bear asymmetric suppression if mechanisms concentrate against political challengers
 *   - Prosecutors / Dikastai: Institutional actors (institutional/arbitrage) — operate the mechanism; have incentive to maintain the ritual even as substantive enforcement decays
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing proposer liability as a principle of justice when it is actually a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(accountability_machinery__graphe_paranomon, 0.38).
domain_priors:suppression_score(accountability_machinery__graphe_paranomon, 0.52).
domain_priors:theater_ratio(accountability_machinery__graphe_paranomon, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(accountability_machinery__graphe_paranomon, extractiveness, 0.38).
narrative_ontology:constraint_metric(accountability_machinery__graphe_paranomon, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(accountability_machinery__graphe_paranomon, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(accountability_machinery__graphe_paranomon, tangled_rope).
narrative_ontology:human_readable(accountability_machinery__graphe_paranomon, "Graphe Paranomon: Athenian Accountability Through Proposer Liability").
narrative_ontology:topic_domain(accountability_machinery__graphe_paranomon, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(accountability_machinery__graphe_paranomon).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(accountability_machinery__graphe_paranomon, '8e0e3d1a-950f-4dc6-88d8-4916f9d984bd').
narrative_ontology:cs_kernel_codification('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', formalized).
narrative_ontology:cs_authority_grounding('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', lineage).
narrative_ontology:cs_interpretation_layer_present('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd').
narrative_ontology:cs_reading_relation('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', accountability_machinery__euthynai_audit, coexists_with).
narrative_ontology:cs_reading_relation('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', accountability_machinery__ostracism_institution, influences).
narrative_ontology:cs_axiom('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', foundational, proposer_bears_accountability_cost).
narrative_ontology:cs_axiom_status(proposer_bears_accountability_cost, holdable).
narrative_ontology:cs_axiom_grounding('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', proposer_bears_accountability_cost, deontological).
narrative_ontology:cs_axiom('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', foundational, legality_reviewable_ex_post).
narrative_ontology:cs_axiom_status(legality_reviewable_ex_post, holdable).
narrative_ontology:cs_axiom_grounding('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', legality_reviewable_ex_post, conventional).
narrative_ontology:cs_reference_frame('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', proposer_individual_accountability_doctrine).
narrative_ontology:cs_drift_state('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', late_classical_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8e0e3d1a-950f-4dc6-88d8-4916f9d984bd', '').
narrative_ontology:cs_kernel_id(accountability_machinery__graphe_paranomon, accountability_machinery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(accountability_machinery__graphe_paranomon, decisional_stability).
narrative_ontology:constraint_beneficiary(accountability_machinery__graphe_paranomon, assembly_collective).
narrative_ontology:constraint_victim(accountability_machinery__graphe_paranomon, bold_initiators).
narrative_ontology:constraint_victim(accountability_machinery__graphe_paranomon, ambitious_orators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNPOPULAR PROPOSER (SNARE) — An orator who genuinely believes a proposal serves the state but Assembly opinion shifts after passage faces prosecution without recourse. Their only exit is silence. The mechanism suppresses dissent and bold advocacy through personal legal jeopardy. Maximum experienced extraction because the proposer bears full cost of a collective decision that turned out unlawful.
constraint_indexing:constraint_classification(accountability_machinery__graphe_paranomon, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AMBITIOUS ORATOR (TANGLED ROPE) — A politician with genuine policy proposals experiences the constraint as mixed: they benefit from the platform and collective legitimacy the Assembly provides, but face career-ending risk if persuasion later proves unlawful. Suppression is real (liability deters recklessness) but not total — skillful advocacy still advances careers. Extraction is moderate because the orator retains some agency and some benefit from the political system.
constraint_indexing:constraint_classification(accountability_machinery__graphe_paranomon, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: ASSEMBLY COLLECTIVE (ROPE) — The Assembly experiences the graphe as pure coordination: it enables collective decision-making by holding proposers accountable for the legality of their own persuasion. The mechanism solves the collective action problem of ensuring proposals conform to law. Net beneficiary — extraction runs toward institutional stability, not away from the Assembly's interests.
constraint_indexing:constraint_classification(accountability_machinery__graphe_paranomon, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: ESTABLISHED POLITICAL DYNASTY (TANGLED ROPE) — Families with generations of successful advocacy experience the graphe as manageable risk: their networks, legal expertise, and reputation for sound judgment reduce prosecution risk. They benefit from the system's legitimacy while bearing minimal suppression. This is a mixed experience — they coordinate through the Assembly while extracting advantage through superior defensive position.
constraint_indexing:constraint_classification(accountability_machinery__graphe_paranomon, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: DEMOCRATIC STABILIZATION PROJECT (SCAFFOLD) — Organizers of the graphe mechanism see it as temporary theater of accountability, with sunset logic inherent: as legal norms crystallize and become predictable, the graphe becomes less necessary. The mechanism has low theater because the legal liability is genuine and unperformative. This perspective sees the constraint as a bridge mechanism toward mature rule of law.
constraint_indexing:constraint_classification(accountability_machinery__graphe_paranomon, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: PROSECUTORIAL RITUAL (PITON) — Over centuries, the graphe hardens into ritual: prosecutors routinely file, colleagues defend in stale formulas, acquittals dominate. The mechanism persists through institutional inertia — the threat is performative even though the rule is formally enforced. Theater ratio rises as the threat decouples from actual conviction risk. The piton marks the constraint's degradation into ceremony.
constraint_indexing:constraint_classification(accountability_machinery__graphe_paranomon, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some accountability mechanism is inherent to any legal order: someone must bear the cost of false persuasion, and assigning it to the persuader rather than the collective is a natural principle of individual responsibility. This perspective risks naturalizing what is actually a contingent institutional choice. The engine will identify this as a false summit candidate.
constraint_indexing:constraint_classification(accountability_machinery__graphe_paranomon, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(accountability_machinery__graphe_paranomon_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(accountability_machinery__graphe_paranomon, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(accountability_machinery__graphe_paranomon, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(accountability_machinery__graphe_paranomon, TR),
    TR >= 0.70.

:- end_tests(accountability_machinery__graphe_paranomon_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The graphe creates genuine suppression (bold advocates face prosecution risk) but is not maximally extractive because: (1) proposers are not trapped — they can choose silence or align with consensus; (2) Assembly legitimacy rests partly on believing proposer accountability ensures legality, so extreme extraction would undermine the mechanism's own legitimacy; (3) skilled advocacy still succeeds and advances careers. The value reflects moderate suppression with real but not insurmountable barriers. Suppression (0.52): Moderate-high. Barriers to bold advocacy include prosecution risk, legal fees, reputational damage from association with unlawful proposals, and career jeopardy. But suppression is not total — some risk-takers do emerge, and Assembly debate was evidently vigorous. Theater ratio (0.48): Moderate. The mechanism begins with genuine legal force — proposals are actually reviewed and some genuinely violate law. But over time, as prosecution becomes ritualistic and collegial acquittals dominate, the theater rises. By t=50, the ritual persists (0.48) despite weakening enforcement, indicating partial degradation toward piton status. Claimed type (tangled_rope): The constraint meets the gates — beneficiaries exist (Assembly, stability), victims exist (orators, initiators), active enforcement is real (prosecution mechanism), and both coordination and extraction are genuine. The mechanism coordinates Assembly self-binding (coordination function) while suppressing bold advocacy (extraction function). The asymmetry is real and measurable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why deferential realism requires multiple perspectives. The Assembly sees rope (coordination solving the problem of ensuring legal proposals). The bold initiator sees snare (legal jeopardy with no exit). The ambitious orator sees tangled rope (mixed benefit and burden, constrained but not trapped). The established dynasty sees tangled rope too but with far lower suppression (mobile exit, better defensive position). The prosecutorial ritual sees piton (the mechanism persists through ceremony, not function). The analytical observer risks mountain (naturalizing proposer liability as justice principle) but actually faces false summit detection (the choice to bind proposers rather than Assembly is contingent). The same structural data yields six different classifications because observational position determines which extraction and coordination flows are salient.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply across agents. Bold initiators who lack established networks face high d (0.85–0.95) — they are full targets of the mechanism, experiencing extraction. Established dynasties face low d (0.15–0.25) — they are partial beneficiaries with defensive capacity. The Assembly faces d ~0.05 (full beneficiary of coordination). The analytical observer faces d ~0.72 (observational position with some extraction). The directionality derivation is from beneficiary/victim declarations plus exit options: victims with trapped or constrained exit get high d; beneficiaries with arbitrage exit get low d. The perspectival gap reflects these divergent positions — what appears as pure coordination (rope) to the Assembly appears as pure extraction (snare) to a powerless proposer, and as manageable risk (tangled_rope) to a politically skilled orator with networks.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legality_determination_authority,
    'Who determines whether a proposal is ''unlawful''? Is it the courts, a subsequent assembly vote, or a constitutional review process?',
    'Historical analysis of graphe cases: which bodies had authority to overturn proposals; correlation between initial Assembly votes and subsequent legal determinations; evidence of courts overruling Assembly judgments',
    'If courts had binding authority: proposer liability enforces judicial supremacy over Assembly (extraction mechanism favoring judicial power). If Assembly could overturn its own decisions through meta-deliberation: liability enforces Assembly self-correction (coordination mechanism). If authority was contested: extractiveness and suppression values must be higher due to ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legality_determination_authority, empirical, 'Authority structure for determining unlawfulness').

omega_variable(
    prosecution_selectivity_and_bias,
    'Were graphe paranomon prosecutions evenly distributed across political factions or concentrated against disfavored groups?',
    'Prosopographic analysis of defendants; correlation between political alignment, prominence, and prosecution rates; comparison with contemporaneous ostracism exile lists and euthynai accusations',
    'If evenly distributed: constraint functions as described — neutral accountability. If biased: mechanism is weaponized political extraction (snare amplified). If concentrated against democratic reformers: suppression value must increase to reflect asymmetric targeting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prosecution_selectivity_and_bias, empirical, 'Distribution of prosecutions across political groups').

omega_variable(
    interaction_with_euthynai_and_ostracism,
    'How does the graphe paranomon relate structurally to the euthynai audit and ostracism institution? Do they reinforce or substitute for each other?',
    'Chronological analysis of accountability actions; evidence of whether the same agents faced multiple accountability mechanisms or whether types segregated by offender category; analysis of legal precedent integrating all three',
    'If mechanisms reinforce (all three deploy against same agents): combined suppression is severe, transforming the system into a snare. If they substitute (different mechanisms for different scenarios): each has lower extractiveness. If temporal sequence shows progression (graphe first, then euthynai, then ostracism): suppression accumulates across the reading''s timeline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interaction_with_euthynai_and_ostracism, empirical, 'Structural relationship between graphe, euthynai, and ostracism accountability mechanisms').

omega_variable(
    reading_selection_among_siblings,
    'Which accountability reading best captures the democracy''s actual mechanism: individual proposer liability (graphe), universal official auditing (euthynai), or supraconstitutional exile (ostracism)?',
    'Historical frequency analysis: which mechanism was actually deployed most often? Which generated fear or compliance in the historical record? Textual analysis of rhetorical appeals to each mechanism.',
    'If graphe dominates practice: this reading''s classification holds and the democracy''s self-binding is through orators. If euthynai dominates: the system emphasizes collective accountability via audit, not individual proposal liability. If ostracism dominates: the system relies on brute-force exclusion, not legal mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_among_siblings, empirical, 'Empirical dominance of graphe paranomon among accountability mechanisms').

omega_variable(
    kernel_reading_contrast_graphe_vs_euthynai,
    'Is the kernel''s accountability principle centered on individual proposer liability (graphe) or on universal auditing of all officials (euthynai)? These are readings of the same commitment to accountability but with different victim sets and extraction mechanisms.',
    'Conceptual analysis: euthynai targets all officials post-tenure; graphe targets specific proposers ex post facto. They instantiate different doctrinal commitments to ''who bears accountability cost''. Sibling comparison study.',
    'This omega documents the committer-frame ambiguity. Graphe emphasizes suppression of bold advocacy; euthynai emphasizes comprehensive oversight. The kernel is contested — both readings are live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contrast_graphe_vs_euthynai, conceptual, 'Kernel-level distinction between proposer liability (graphe) and official auditing (euthynai) readings').

omega_variable(
    false_summit_natural_law_risk,
    'Does the analytical perspective''s classification as ''mountain'' (individual responsibility for persuasion is a natural principle) naturalize what is actually a contingent institutional choice?',
    'Comparative constitutional analysis: do other democracies with different legal traditions assign proposer liability? Is the assignment to the persuader universal or historically specific? Do alternative accountability models work?',
    'If naturalizing: the mountain classification is a false summit; the constraint should reclassify to tangled_rope (mixed coordination and extraction). If principled: the natural law framing holds and reflects a deep constitutional commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether proposer liability is a natural principle or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(accountability_machinery__graphe_paranomon, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(graphe_tr_t0, accountability_machinery__graphe_paranomon, theater_ratio, 0, 0.32).
narrative_ontology:measurement(graphe_tr_t25, accountability_machinery__graphe_paranomon, theater_ratio, 25, 0.42).
narrative_ontology:measurement(graphe_tr_t50, accountability_machinery__graphe_paranomon, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(graphe_be_t0, accountability_machinery__graphe_paranomon, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(graphe_be_t25, accountability_machinery__graphe_paranomon, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(graphe_be_t50, accountability_machinery__graphe_paranomon, base_extractiveness, 50, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(accountability_machinery__graphe_paranomon, enforcement_mechanism).
narrative_ontology:affects_constraint(accountability_machinery__graphe_paranomon, accountability_machinery__euthynai_audit).
narrative_ontology:affects_constraint(accountability_machinery__graphe_paranomon, accountability_machinery__ostracism_institution).

% DUAL FORMULATION NOTE:
% The accountability_machinery kernel has three structural readings. This story models the graphe paranomon reading (individual proposer liability). The euthynai reading models universal official auditing (different victim set, different enforcement structure, different extractiveness). The ostracism reading models blunt exile votes (no legal process, maximum suppression but no individual accountability). All three are linked as siblings in the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
