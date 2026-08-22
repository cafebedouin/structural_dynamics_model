% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: Constitutional Positivism: Text-Plus-Amendment Reading of Constitutional Authority
 *   domain: Constitutional Law / Legal Theory / Political Philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the positivist reading of the US
 *   Constitution's kernel: constitutional meaning is exhausted by the
 *   ratified text plus formally ratified amendments, and judicial
 *   interpretation is bound to that textual corpus without recourse to either
 *   framers' subjective intent (the originalist reading) or evolving societal
 *   values (the living reading). This is a moderate-constraint reading — it
 *   is text-bound but not historically bound, meaning it permits
 *   interpretation of what the text's words mean today without needing to
 *   reconstruct 1787 or 1868 intent, but it forecloses judicial updating of
 *   meaning absent formal amendment. The amendment process (Article V)
 *   becomes the sole legitimate democratic mechanism for constitutional
 *   change, which places enormous practical weight on a mechanism that has
 *   produced only 27 amendments in over two centuries and none since 1992
 *   outside a delayed ratification technicality. Positivism's coordination
 *   function is real: it gives courts, legislators, and citizens a stable,
 *   publicly ascertainable interpretive anchor. Its extraction is structural
 *   rather than acute: interpretive silences and unamendable minorities bear
 *   the cost of a system that treats textual gaps as an absence of right
 *   rather than an invitation to construct one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.32).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.38).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "Constitutional Positivism: Text-Plus-Amendment Reading of Constitutional Authority").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "Constitutional Law / Legal Theory / Political Philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '250fcbf6-c090-4708-95d7-d3e030aa66c2').
narrative_ontology:cs_kernel_codification('250fcbf6-c090-4708-95d7-d3e030aa66c2', fixed_text).
narrative_ontology:cs_authority_grounding('250fcbf6-c090-4708-95d7-d3e030aa66c2', lineage).
narrative_ontology:cs_interpretation_layer_present('250fcbf6-c090-4708-95d7-d3e030aa66c2').
narrative_ontology:cs_reading_relation('250fcbf6-c090-4708-95d7-d3e030aa66c2', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('250fcbf6-c090-4708-95d7-d3e030aa66c2', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('250fcbf6-c090-4708-95d7-d3e030aa66c2', foundational, textual_meaning_exhausts_constitutional_content).
narrative_ontology:cs_axiom_status(textual_meaning_exhausts_constitutional_content, holdable).
narrative_ontology:cs_axiom_grounding('250fcbf6-c090-4708-95d7-d3e030aa66c2', textual_meaning_exhausts_constitutional_content, conventional).
narrative_ontology:cs_axiom('250fcbf6-c090-4708-95d7-d3e030aa66c2', foundational, amendment_process_is_exclusive_legitimate_change_channel).
narrative_ontology:cs_axiom_status(amendment_process_is_exclusive_legitimate_change_channel, holdable).
narrative_ontology:cs_axiom_grounding('250fcbf6-c090-4708-95d7-d3e030aa66c2', amendment_process_is_exclusive_legitimate_change_channel, conventional).
narrative_ontology:cs_reference_frame('250fcbf6-c090-4708-95d7-d3e030aa66c2', textual_positivist_supremacy).
narrative_ontology:cs_drift_state('250fcbf6-c090-4708-95d7-d3e030aa66c2', post_1971_amendment_dormancy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('250fcbf6-c090-4708-95d7-d3e030aa66c2', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, textualist_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, amendment_capable_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, legislative_branch).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, constitutionally_unamendable_minorities).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, litigants_seeking_doctrinal_evolution).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, communities_with_textual_gaps).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, rule_of_law_predictability_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, democratic_legitimacy_through_amendment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates constitutional questions by treating the ratified text plus formally ratified amendments as the exclusive interpretive material, refusing to import evolving social values or extra-textual purpose. This judiciary sets the interpretive methodology that governs which arguments litigants may successfully make, and its members' professional legitimacy is bound up in maintaining textual discipline as the standard against which their rulings are judged.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, textualist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Supermajority coalitions capable of clearing the Article V threshold get a predictable, singular channel for changing constitutional meaning — amendment rather than litigation or judicial reinterpretation. This gives them assurance that a win, once amended into text, cannot be eroded by later judicial drift; the cost is that assembling such coalitions is expensive and rare.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, amendment_capable_majorities, beneficiary,
    organized, generational, mobile, national).

% Positivist reading pushes contested normative questions back toward legislative and amendment processes rather than letting courts resolve them by reinterpretation, which preserves the legislature's role as the primary site of value-laden lawmaking within its textually permitted scope.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legislative_branch, beneficiary,
    institutional, generational, constrained, national).

% Groups whose claims depend on constitutional protections not enumerated in text or subsequent amendments (e.g., emergent rights claims not yet codified) have no path to relief through litigation under this reading; their only recourse is the amendment process, which requires supermajority coalition-building that dispersed or politically weak groups structurally cannot assemble. The text becomes a ceiling on their claims regardless of the depth of harm.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutionally_unamendable_minorities, payer,
    powerless, biographical, trapped, national).

% Parties whose cases would benefit from evolving doctrinal interpretation (e.g., applying old text to new technology or social conditions in ways not textually anticipated) find their arguments foreclosed because the positivist judiciary declines to reason beyond text-plus-amendment. They bear the cost of a legal system that cannot adapt to circumstances the text-writers did not foresee, without the numbers to force an amendment.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, litigants_seeking_doctrinal_evolution, payer,
    moderate, biographical, constrained, national).

% Communities whose lived harms fall into interpretive silences in the text (areas the framers or amenders never addressed) have no textual hook for relief. Positivism treats silence as absence of right rather than as an invitation to construct one, leaving these harms unaddressed until a supermajority chooses to act.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, communities_with_textual_gaps, payer,
    powerless, generational, trapped, regional).

% Advocates for evolving interpretation are excluded from the positivist judiciary's accepted interpretive toolkit; their arguments about societal evolution and living values are treated as illegitimate methodology rather than substantive claims to be weighed, so they must either persuade under textualist terms or pursue amendment instead.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalists, excluded,
    organized, generational, constrained, national).

% Study how positivist doctrine performs against originalist and living readings over time, tracking whether text-plus-amendment produces more stable, predictable, or more brittle constitutional law, and whether the amendment channel functions as a genuine safety valve or a structurally blocked pressure vessel.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, publicly ascertainable source of constitutional meaning — ratified text plus formally ratified amendments — so that judges, legislators, and citizens can predict what the Constitution requires without relitigating its content case by case, and channels normative disagreement into a defined, democratically accountable amendment process rather than diffuse judicial discretion.
% TRANSFER_FUNCTION: Moves interpretive authority away from courts exercising open-ended judgment and toward text-drafters and amendment-ratifying supermajorities; correspondingly moves the cost of unaddressed harms onto groups whose claims fall in textual gaps or require doctrinal evolution the text does not license, since their only remedy is the high-threshold amendment process rather than litigation.
% ABSENT_VOICES: Living constitutionalists and dispersed minority groups whose claims depend on interpretive evolution are structurally outside the accepted interpretive conversation — their substantive arguments are recast as methodological errors (judicial activism) rather than engaged on the merits, and they lack the numbers to pursue the amendment channel this reading treats as their proper remedy.
% DISAPPEARANCE_RATIONALE: If positivist interpretive discipline vanished overnight, courts would face immediate pressure to fill it with either originalist historical inquiry or living-constitutionalist evolving-values reasoning; litigation strategy, judicial confirmation politics, and legislative drafting all currently orient around text-plus-amendment as the operative standard, and its removal would immediately reopen currently-foreclosed doctrinal arguments and destabilize settled precedent built on textual discipline.
% FOUNDING_PROBLEM: The problem of judicial discretion without a fixed anchor: without a rule that judges are bound by ratified text and only formally ratified changes to it, constitutional meaning risks becoming whatever judges believe is normatively desirable at a given moment, undermining rule-of-law predictability and democratic accountability for constitutional change.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivists and textualist judges attest the problem remains fully live — unconstrained judicial discretion is an ongoing risk evidenced by contested rulings under competing interpretive methods. Critical legal scholars and living-constitutionalist academics, situated outside the beneficiary coalition, attest that the 'discretion problem' framing understates how much discretion textualism itself smuggles in through interpretive choices about what counts as 'the text,' and that the amendment-channel remedy is largely theoretical given Article V's near-total post-1971 dormancy — corroboration exists but the parties dispute what it shows.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) is moderate and rising slowly — the positivist reading is genuinely less extractive than either the originalist reading (which imports contestable historical claims as binding) or a captured living reading (which can be manipulated by whichever judicial coalition controls 'evolving values'), but it still imposes real costs on textually-gapped claims over time as social conditions generate harms the text never contemplated and the amendment channel fails to absorb. Suppression (0.38) reflects the judiciary's active refusal to entertain non-textual arguments as a matter of accepted methodology — this is a real interpretive constraint enforced through case selection, doctrine, and appellate reversal, not merely a preference. Theater ratio (0.22) is low-to-moderate: the textualist enterprise does substantial genuine interpretive work, but a growing minority of theater lies in selective invocations of 'plain text' that themselves involve contestable interpretive choices dressed as mechanical readings.
 *
 * PERSPECTIVAL GAP:
 *   From the textualist judiciary's seat, this reading is coordination: it prevents judges from becoming an unaccountable third amendment process and preserves rule-of-law predictability. From the seat of communities with textual gaps or unamendable minorities, the identical structure operates as enforced extraction — their claims are foreclosed not because they lack merit but because the interpretive rules exclude the kind of argument that could vindicate them. The engine should compute a tangled-rope classification precisely because both descriptions are structurally accurate from their respective seats: genuine coordination (predictable, publicly ascertainable meaning) coexists with asymmetric extraction (foreclosure of textually-unanchored claims) through the same interpretive apparatus, requiring active judicial enforcement of the textualist discipline to hold.
 *
 * DIRECTIONALITY LOGIC:
 *   The textualist judiciary and amendment-capable majorities sit near the beneficiary end: the judiciary gains professional and institutional legitimacy from methodological discipline, and organized supermajorities gain a predictable, durable channel for locking in constitutional change once achieved. Constitutionally unamendable minorities and communities with textual gaps sit near the full-target end: they are trapped by an interpretive framework that treats their unaddressed harms as constitutionally silent rather than constitutionally cognizable, and they structurally lack the numbers to clear the Article V threshold. Litigants seeking doctrinal evolution occupy an intermediate position — moderate power, constrained exit — since some can eventually succeed through incremental textual argument even where broad reinterpretation is foreclosed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unconstrained judicial discretion undermining rule-of-law predictability — remains partly live (contested framings of what 'discretion' means persist) but the remedy (text-plus-amendment exclusivity) has arguably outlived proportionality to the problem given Article V's near-total dormancy since 1971. This is not straightforward mandatrophy (the founding problem is not simply dead) but a drift condition: the amendment channel that was meant to be the live democratic safety valve has become increasingly theoretical, while the disciplinary function on judges remains fully operative. This asymmetry — active constraint, inactive escape valve — is exactly the pattern that risks converting a genuine coordination mechanism into a one-way ratchet, which the classification as tangled_rope rather than pure rope is meant to flag for ongoing scrutiny.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_reading_framing_choice,
    'Is positivism best understood as a genuinely distinct interpretive methodology, or is ''text-plus-amendment'' itself parasitic on background originalist or evolving-meaning assumptions about what words in the text mean (making it a disguised hybrid rather than a third independent reading)?',
    'Comparative doctrinal analysis: identify cases where positivist judges diverge in outcome from both originalist and living-constitutionalist judges on the same facts. If systematic divergence exists across enough cases, positivism is a genuinely distinct reading; if positivist rulings track one of the other two readings'' outcomes closely, positivism functions as a rhetorical veneer over one of its siblings.',
    'If positivism collapses into a disguised hybrid, its distinct ε and stakeholder structure would need re-derivation from whichever sibling it actually tracks, and this story''s claim to independent constraint status would weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_reading_framing_choice, conceptual, 'Whether positivism is a genuinely independent reading or a disguised hybrid of its siblings.').

omega_variable(
    article_v_safety_valve_functionality,
    'Does the Article V amendment process still function as a genuine, accessible democratic mechanism for constitutional change, or has it become effectively dead due to modern polarization and supermajority requirements, converting positivism''s coordination story into pure judicial gatekeeping?',
    'Track amendment proposal and ratification rates against historical baselines, and model the coalition sizes realistically achievable under current partisan geography; compare to eras when amendments passed more frequently.',
    'If Article V is effectively dead, the extraction component of this reading is understated in the current metrics and should rise substantially, since the claimed remedy for constitutional gaps is unavailable in practice; this would push the classification further from rope-like coordination toward snare-adjacent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_v_safety_valve_functionality, empirical, 'Whether the amendment process remains a functioning democratic remedy or has become inert.').

omega_variable(
    textualism_beneficiary_naturalization,
    'Does the textualist judiciary present its methodology as neutral and mechanical partly because doing so obscures the discretionary choices embedded in determining what ''the text says'' (e.g., choosing among semantic, structural, or purposive readings of ambiguous language)?',
    'Analyze split decisions among textualist judges on identical text; persistent disagreement among judges purportedly using the same mechanical method would indicate discretion is present but rhetorically minimized.',
    'If textualism smuggles in substantial hidden discretion, the theater_ratio for this reading is understated and the judiciary''s beneficiary status (professional legitimacy from claimed neutrality) would be better characterized as partly cover for ordinary judicial policy-making.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textualism_beneficiary_naturalization, conceptual, 'Whether claimed textual neutrality conceals ordinary judicial discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__positivist_reading, theater_ratio, 1787, 0.1).
narrative_ontology:measurement(us_c_tr_t1850, us_constitution_1787__positivist_reading, theater_ratio, 1850, 0.13).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_1787__positivist_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(us_c_tr_t1971, us_constitution_1787__positivist_reading, theater_ratio, 1971, 0.17).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__positivist_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__positivist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__positivist_reading, base_extractiveness, 1787, 0.18).
narrative_ontology:measurement(us_c_be_t1850, us_constitution_1787__positivist_reading, base_extractiveness, 1850, 0.22).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_1787__positivist_reading, base_extractiveness, 1937, 0.25).
narrative_ontology:measurement(us_c_be_t1971, us_constitution_1787__positivist_reading, base_extractiveness, 1971, 0.27).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__positivist_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__positivist_reading, base_extractiveness, 2024, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__positivist_reading, suppression_requirement, 1787, 0.25).
narrative_ontology:measurement(us_c_su_t1850, us_constitution_1787__positivist_reading, suppression_requirement, 1850, 0.28).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_1787__positivist_reading, suppression_requirement, 1937, 0.3).
narrative_ontology:measurement(us_c_su_t1971, us_constitution_1787__positivist_reading, suppression_requirement, 1971, 0.32).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__positivist_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__positivist_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'constitutional interpretation methodology' under the us_constitution_1787 kernel, per the ε-invariance principle. The originalist_reading (framers' intent binding, historically anchored) has a different beneficiary/victim structure — historical elites' presumed intent is privileged, contemporary claims not anticipated by 1787/1868 drafters are foreclosed more severely. The living_reading (evolving societal values, aspirational text) has a substantially different ε — it privileges judicial and contemporary-majority discretion, benefiting groups who can currently mobilize interpretive sympathy but exposing the reading to charges of judicial policy-making without democratic authorization. Positivism sits structurally between the two: less discretion-generating than living constitutionalism, less historically contestable than originalism, but still tangled by its dependence on an amendment channel that may not function as advertised. All three stories share the same underlying kernel (the ratified constitutional text) but instantiate materially different constraints with different ε values, different victim sets, and different classifications, linked here rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
