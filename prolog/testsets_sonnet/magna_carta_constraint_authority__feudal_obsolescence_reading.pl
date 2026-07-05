% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta as Obsolete Feudal Compact (Executive Discretion Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Magna Carta kernel:
 *   the feudal-obsolescence reading, which holds that the 1215 charter was a
 *   narrow baronial compact resolving specific grievances between King John
 *   and his barons — forest law, feudal relief, wardship — and carries no
 *   logical or binding authority over modern sovereignty structures. Under
 *   this reading, invocations of Magna Carta in contemporary due-process or
 *   executive-restraint arguments are treated as anachronistic sentiment
 *   rather than live constraint. This is NOT a story about whether Magna
 *   Carta 'really' constrains modern government — that contested question is
 *   decomposed across three sibling readings (this one, the
 *   living-constitutionalism reading, and the parliamentary-sovereignty
 *   reading), each its own constraint story with its own epsilon,
 *   beneficiaries, and type. This reading's structural signature: the
 *   obsolescence claim is used less as sober historiography and more as an
 *   extraction-enabling move — it clears interpretive ground for executive
 *   and security actors to act without answering due-process objections,
 *   while imposing costs on advocates, judicial-restraint proponents, and
 *   directly on detained persons who cannot invoke a charter-derived norm the
 *   reading has declared void of modern force.
 *
 * KEY AGENTS:
 *   - executive_branch_officials: beneficiary/agenda_setter (institutional/arbitrage) — invokes obsolescence to expand discretion
 *   - national_security_apparatus: beneficiary (institutional/arbitrage) — relies on the reading to resist due-process claims
 *   - popular_constitutionalism_advocates: payer (moderate/constrained) — loses a rhetorical and doctrinal resource
 *   - judicial_restraint_proponents: payer (moderate/constrained) — doctrinal citations weakened
 *   - detained_persons_without_due_process_recourse: payer (powerless/trapped) — bears direct cost of foreclosed argument
 *   - constitutional_historians: observer (analytical) — assesses the historical claim independent of its use
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.58).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta as Obsolete Feudal Compact (Executive Discretion Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'b05c00ea-3bd8-4734-831b-1cd503a17be9').
narrative_ontology:cs_kernel_codification('b05c00ea-3bd8-4734-831b-1cd503a17be9', distributed).
narrative_ontology:cs_authority_grounding('b05c00ea-3bd8-4734-831b-1cd503a17be9', distributed).
narrative_ontology:cs_reading_relation('b05c00ea-3bd8-4734-831b-1cd503a17be9', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('b05c00ea-3bd8-4734-831b-1cd503a17be9', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('b05c00ea-3bd8-4734-831b-1cd503a17be9', foundational, historical_particularity_bars_modern_extension).
narrative_ontology:cs_axiom_status(historical_particularity_bars_modern_extension, holdable).
narrative_ontology:cs_axiom_grounding('b05c00ea-3bd8-4734-831b-1cd503a17be9', historical_particularity_bars_modern_extension, empirically_contingent).
narrative_ontology:cs_axiom('b05c00ea-3bd8-4734-831b-1cd503a17be9', secondary, sovereign_discretion_unconstrained_absent_positive_enactment).
narrative_ontology:cs_axiom_status(sovereign_discretion_unconstrained_absent_positive_enactment, holdable).
narrative_ontology:cs_axiom_grounding('b05c00ea-3bd8-4734-831b-1cd503a17be9', sovereign_discretion_unconstrained_absent_positive_enactment, conventional).
narrative_ontology:cs_reference_frame('b05c00ea-3bd8-4734-831b-1cd503a17be9', feudal_baronial_settlement_1215).
narrative_ontology:cs_drift_state('b05c00ea-3bd8-4734-831b-1cd503a17be9', post_war_on_terror_executive_expansion, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('b05c00ea-3bd8-4734-831b-1cd503a17be9', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch_officials).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, national_security_apparatus).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, judicial_restraint_proponents).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, detained_persons_without_due_process_recourse).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, sovereign_discretion_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, historical_particularity_of_feudal_charters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the argument that Magna Carta was a narrow settlement between King John and rebellious barons over specific 13th-century feudal levies, with no direct textual or logical bridge to modern executive power, in order to resist claims that clauses like 39 and 40 constrain contemporary detention, due process, or emergency action. This reading widens the space in which they can act without citing the charter as a limit.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch_officials, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch_officials, agenda_setter).

% Relies on the obsolescence framing to justify detention, surveillance, and emergency-power practices without engaging arguments that such practices violate inherited due-process guarantees. Treats appeals to Magna Carta in litigation or public argument as historically confused category errors rather than live constraints.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, national_security_apparatus, beneficiary,
    institutional, immediate, arbitrage, national).

% Argue that Magna Carta's symbolic and doctrinal weight, however textually narrow at origin, constituted a founding claim that subjects possess rights against arbitrary sovereign power — a claim later constitutions and case law built upon. Under the obsolescence reading, their argument is dismissed as anachronistic sentiment rather than engaged as a genealogical claim, foreclosing a rhetorical and legal resource they rely on in rights litigation and public advocacy.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates, payer,
    moderate, civilizational, constrained, national).

% Judges and legal scholars who cite Magna Carta as interpretive background for due-process and habeas corpus doctrine find their citations treated as decorative rather than binding once the obsolescence reading dominates judicial or executive argument, weakening the doctrinal scaffolding they use to check executive overreach.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, judicial_restraint_proponents, payer,
    moderate, generational, constrained, national).

% Individuals subject to executive detention or process-limiting action bear the direct cost when courts or officials accept the feudal-obsolescence framing to decline treating Magna Carta-derived due-process norms as live constraints on their treatment. Their exit options are essentially nonexistent — they cannot litigate their way around a doctrine that has already foreclosed the argument.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, detained_persons_without_due_process_recourse, payer,
    powerless, immediate, trapped, national).

% Study the actual 1215 text and its immediate feudal context, noting that most of the charter's original 63 clauses addressed forest law, feudal relief, and baronial privilege — narrow historical grievances — while a small number of clauses (39, 40) were later read expansively by 17th-century jurists like Coke for reasons contingent on their own political struggles, not textual necessity.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% Nations whose legal systems inherited Magna Carta-derived doctrine through imperial transmission are rarely consulted on how the charter's authority should be read in either direction; the obsolescence-vs-living debate is conducted almost entirely within the originating jurisdiction's institutions, even though the outcome affects inherited due-process doctrine across many former colonies.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, colonial_and_postcolonial_polities, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine at the level this reading operates: the obsolescence claim does not coordinate collective action among any group of beneficiaries toward a shared good; it functions as an interpretive move that clears doctrinal ground for executive action by denying binding force to an inherited restraint.
% TRANSFER_FUNCTION: Moves interpretive leverage and practical discretion from courts, advocates, and detained individuals who would otherwise invoke Magna Carta-derived due-process norms, to executive and security institutions freed from having to answer those invocations as live constraints.
% ABSENT_VOICES: Colonial and postcolonial jurisdictions that inherited Magna Carta-derived common-law doctrine through imperial legal transmission have no seat in the debate over whether the charter's authority survives into modern sovereignty structures, despite bearing downstream doctrinal consequences. Detained persons asserting due-process claims are structurally unable to contest the historical-obsolescence argument on its own terms — it is adjudicated by judges and scholars, not by those it is invoked against.
% DISAPPEARANCE_RATIONALE: If the feudal-obsolescence reading vanished overnight, executive and security actors would lose a ready argument for dismissing Magna Carta-derived due-process claims, and courts would need to engage substantively with genealogical continuity arguments they currently can bypass. Whether this constitutes the world 'rearranging' is disputed: proponents of the obsolescence reading maintain the charter was never doing real constraining work in modern practice (so nothing would actually change), while critics maintain its absence would remove a load-bearing dismissal mechanism that currently protects a meaningful volume of executive discretion.
% FOUNDING_PROBLEM: The historical claim was constructed to solve a jurisprudential problem: reconcile the symbolic weight attached to Magna Carta in constitutional rhetoric with the text's actual narrow, feudal, and historically contingent content, resisting anachronistic over-reading of a 1215 baronial settlement as a modern rights charter.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside any beneficiary group (e.g., scholars of medieval English law with no stake in contemporary executive-power litigation) substantially corroborate the narrow textual-historical claim — the 1215 charter was indeed a feudal settlement addressing specific baronial grievances. However, the SAME historians frequently reject the further inference that this historical narrowness settles the question of the charter's doctrinal authority today, noting that legal traditions routinely build binding doctrine atop texts whose original scope was narrower than their later application (common law generally). The historical premise is corroborated from outside; the extractive inference drawn from it is not.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (moderate-high) because the reading's practical function in contemporary use is not neutral historiography but a lever that redistributes discretion toward executive and security actors at the direct expense of due-process claimants who have the least capacity to contest it. Suppression is moderate (0.58): the reading does not physically prevent advocates from making genealogical arguments, but it structurally forecloses those arguments from succeeding in courts and public argument that have accepted the obsolescence premise as settled, which functions as a soft suppression of the alternative reading's uptake. Theater ratio is high and rising (0.71 by 2025, from near-zero in 1215) because the historiographical apparatus increasingly performs rigor — citing the narrow textual scope, the feudal specificity of most clauses — while the deployment of that apparatus in policy and litigation contexts functions less to advance historical understanding than to authorize discretion that the underlying historical claim does not, by itself, entail (the is/ought gap the corroboration section documents). Accessibility collapse is moderate (0.42) — the living-constitutionalism and parliamentary-sovereignty readings remain fully articulable and are actively argued by other parties, so alternatives have not collapsed, only been out-competed in specific institutional venues. Resistance is moderate (0.55): historians, civil liberties advocates, and courts in some jurisdictions actively contest the extractive use of the obsolescence claim even while often accepting its narrow historical premise.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive and security beneficiaries sit near the full-beneficiary end of directionality: they gain discretion and are not exposed to the reading's costs. Detained persons sit at the full-target end: trapped exit, powerless, and the reading is deployed specifically against arguments that would protect them. Popular-constitutionalism advocates and judicial-restraint proponents are targets but at moderate power with constrained (not trapped) exit — they can continue making the argument in other venues, coalitions, or jurisdictions, which the engine should read as some capacity to route around the extraction, though at real cost. Colonial and postcolonial polities are excluded rather than positioned as targets or beneficiaries in this specific reading's operation — the debate over the kernel does not include them as parties even though its resolution affects doctrine they inherited.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading addresses — resisting anachronistic over-reading of a historically narrow feudal text — is genuinely live as a historiographical matter (constitutional historians outside any beneficiary group corroborate the narrow textual claim). But the reading's CONTEMPORARY institutional use has drifted from correcting anachronism toward manufacturing executive latitude: the founding problem (correcting historical over-reading) is substantially solved by ordinary historical scholarship and does not require an ongoing extractive apparatus to keep solving it. What persists past the solved problem is the doctrine's repeated invocation in venues where its practical effect is not historical correction but discretion expansion — a piton-like atrophy of a legitimate historiographical corrective into a standing tool of extraction, with rising theater_ratio marking the growing gap between the apparatus's stated historical-corrective function and its actual policy-shaping use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_accuracy_vs_extractive_use,
    'Is the feudal-obsolescence reading''s core historical claim (Magna Carta was narrowly feudal in original scope) separable from its contemporary extractive deployment (using that historical narrowness to foreclose due-process claims)?',
    'Compare invocations of the obsolescence reading in pure historiographical scholarship versus in litigation briefs, executive legal opinions, and security-policy justification documents; measure whether the historical claim is doing genuine argumentative work in the latter or functioning as rhetorical cover for outcomes reached on other grounds.',
    'If separable, the historical claim itself remains a mountain-like historiographical fact while only its deployment is extractive (supporting a piton or snare classification for the deployment pattern specifically, distinct from the underlying history). If inseparable — if the historical claim was always selected and amplified because of its extractive utility — the entire reading is better classified as snare from origin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_accuracy_vs_extractive_use, conceptual, 'Whether the narrow-historical-scope claim and its executive-discretion-expanding use can be structurally disentangled.').

omega_variable(
    kernel_reading_selection_bias,
    'Why does the feudal-obsolescence reading dominate in executive and security institutional contexts specifically, while the living-constitutionalism reading dominates in civil-rights litigation and the parliamentary-sovereignty reading dominates in UK constitutional scholarship — is this selection driven by genuine institutional expertise differences or by which reading best serves each institution''s structural interest?',
    'Track which reading each institutional actor invokes across multiple unrelated disputes over time; consistent institutional preference for the reading that maximizes that institution''s own discretion, regardless of the specific legal question, would indicate interest-driven selection rather than principled interpretation.',
    'If institutional reading-selection tracks self-interest rather than principled interpretive method, all three kernel readings should be understood partly as tools rather than purely as competing historical/legal theories — reweighting how much independent evidentiary weight any single reading''s institutional dominance should carry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, empirical, 'Whether reading-selection across institutions correlates with institutional self-interest rather than principled interpretation.').

omega_variable(
    postcolonial_doctrinal_inheritance_gap,
    'Should postcolonial jurisdictions that inherited Magna Carta-derived common-law doctrine have independent standing in resolving which kernel reading governs, given that the resolution affects their inherited doctrine but they were not party to the originating jurisdiction''s debate?',
    'Survey postcolonial constitutional courts'' own treatment of Magna Carta-derived doctrine to determine whether they have already developed independent readings that diverge from all three metropolitan readings, which would suggest the kernel itself has already fragmented further than this three-reading decomposition captures.',
    'If postcolonial jurisdictions have developed genuinely independent readings, the kernel decomposition should be expanded beyond three readings, and this story''s excluded-party framing for colonial_and_postcolonial_polities would need to be revised to a fourth sibling reading rather than an excluded voice within this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(postcolonial_doctrinal_inheritance_gap, conceptual, 'Whether the three-reading kernel decomposition is complete or omits independently-evolved postcolonial readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1215, 0.05).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1628, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1628, 0.1).
narrative_ontology:measurement_basis(magn_tr_t1628, observed).
narrative_ontology:measurement(magn_tr_t1789, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1789, 0.2).
narrative_ontology:measurement_basis(magn_tr_t1789, observed).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement_basis(magn_tr_t1900, observed).
narrative_ontology:measurement(magn_tr_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1950, 0.45).
narrative_ontology:measurement_basis(magn_tr_t1950, observed).
narrative_ontology:measurement(magn_tr_t1975, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1975, 0.53).
narrative_ontology:measurement_basis(magn_tr_t1975, observed).
narrative_ontology:measurement(magn_tr_t2001, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2001, 0.62).
narrative_ontology:measurement_basis(magn_tr_t2001, observed).
narrative_ontology:measurement(magn_tr_t2015, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2015, 0.67).
narrative_ontology:measurement_basis(magn_tr_t2015, observed).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2025, 0.71).
narrative_ontology:measurement_basis(magn_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1215, 0.1).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1628, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1628, 0.15).
narrative_ontology:measurement_basis(magn_be_t1628, observed).
narrative_ontology:measurement(magn_be_t1789, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1789, 0.2).
narrative_ontology:measurement_basis(magn_be_t1789, observed).
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement_basis(magn_be_t1900, observed).
narrative_ontology:measurement(magn_be_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement_basis(magn_be_t1950, observed).
narrative_ontology:measurement(magn_be_t1975, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement_basis(magn_be_t1975, observed).
narrative_ontology:measurement(magn_be_t2001, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement_basis(magn_be_t2001, observed).
narrative_ontology:measurement(magn_be_t2015, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(magn_be_t2015, observed).
narrative_ontology:measurement(magn_be_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(magn_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_constraint_authority__feudal_obsolescence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the magna_carta_constraint_authority kernel, decomposed per the ε-invariance principle: the natural-language claim 'does Magna Carta bind modern sovereignty' conflates structurally distinct claims with different beneficiary/victim structures and different epsilon values. feudal_obsolescence_reading (this story) computes as extractive/piton-adjacent because its institutional deployment concentrates benefit in executive/security actors and cost in due-process claimants. living_constitutionalism_reading and parliamentary_sovereignty_reading are separate files with their own metrics — do not average or reconcile epsilon across the three; each reading is a complete, independent constraint linked here only for network/contamination-propagation purposes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
