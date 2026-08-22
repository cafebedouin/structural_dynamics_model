% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Constitutional Validity as Formal-Pedigree Legal Positivism
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint models the positivist reading of constitutional validity:
 *   validity is a function of formal enactment procedure (bicameralism,
 *   presentment, Article V ratification) and recognized institutional
 *   authority, and moral rightness or wrongness of a legal outcome plays no
 *   role in whether the outcome is constitutionally valid. This is a distinct
 *   constraint from the originalist reading (which fixes MEANING at
 *   ratification, a hermeneutic claim) and the living-constitutionalist
 *   reading (which allows evolving APPLICATION of enduring principles, a
 *   normative-evolution claim) — this reading concerns the SOURCE of validity
 *   itself (procedure vs. morality), not how to read a fixed text or whether
 *   meaning can evolve. The rising extractiveness over the interval (0.42 to
 *   0.58) reflects the accumulating gap between what the amendment process
 *   can formally ratify (essentially frozen since 1971's 26th Amendment for
 *   anything contested) and the growing set of substantive claims that lack
 *   clean textual anchoring, especially as social consensus about rights has
 *   moved faster than the supermajority-gated amendment channel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.58).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.62).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Constitutional Validity as Formal-Pedigree Legal Positivism").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '33e4cffc-c554-4ce1-a4b1-54fea68567b4').
narrative_ontology:cs_kernel_codification('33e4cffc-c554-4ce1-a4b1-54fea68567b4', fixed_text).
narrative_ontology:cs_authority_grounding('33e4cffc-c554-4ce1-a4b1-54fea68567b4', lineage).
narrative_ontology:cs_interpretation_layer_present('33e4cffc-c554-4ce1-a4b1-54fea68567b4').
narrative_ontology:cs_reading_relation('33e4cffc-c554-4ce1-a4b1-54fea68567b4', us_constitution_meaning__originalist_reading, influences).
narrative_ontology:cs_reading_relation('33e4cffc-c554-4ce1-a4b1-54fea68567b4', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('33e4cffc-c554-4ce1-a4b1-54fea68567b4', foundational, validity_is_procedural_not_moral).
narrative_ontology:cs_axiom_status(validity_is_procedural_not_moral, holdable).
narrative_ontology:cs_axiom_grounding('33e4cffc-c554-4ce1-a4b1-54fea68567b4', validity_is_procedural_not_moral, conventional).
narrative_ontology:cs_axiom('33e4cffc-c554-4ce1-a4b1-54fea68567b4', secondary, amendment_process_is_exclusive_channel_for_change).
narrative_ontology:cs_axiom_status(amendment_process_is_exclusive_channel_for_change, holdable).
narrative_ontology:cs_axiom_grounding('33e4cffc-c554-4ce1-a4b1-54fea68567b4', amendment_process_is_exclusive_channel_for_change, conventional).
narrative_ontology:cs_reference_frame('33e4cffc-c554-4ce1-a4b1-54fea68567b4', procedural_pedigree_validity).
narrative_ontology:cs_drift_state('33e4cffc-c554-4ce1-a4b1-54fea68567b4', post_1971_amendment_gridlock_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33e4cffc-c554-4ce1-a4b1-54fea68567b4', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, procedural_legitimacy_apparatus).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, sitting_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, legislative_supermajority_incumbents).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, unenumerated_rights_holders).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, amendment_blocked_minorities).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, rule_of_recognition_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, separation_of_powers_formalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies the pedigree test (was this rule enacted through the recognized formal procedure — bicameralism, presentment, Article V) to determine validity, and declines to import contested moral premises into that determination. This insulates judges from charges of imposing personal values and gives them a stable, publicly defensible criterion for every hard case. When the formal text is silent or the amendment process is gridlocked, the reading collapses toward searching for the closest available enacted meaning rather than reasoning from justice directly.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, sitting_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, sitting_judiciary, beneficiary).

% The doctrine of rule-following legitimacy (courts bound by pedigree, not by contested first-order moral theories) is not an actor but the standing institutional posture that benefits whenever validity questions are resolved by procedure rather than substance — it collects deference and predictability without ever being named as a party in a dispute.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, procedural_legitimacy_apparatus, beneficiary,
    institutional, civilizational, analytical, national).

% Whoever currently controls the supermajorities required by Article V benefits doubly: their enacted preferences carry full validity regardless of moral content, and the extreme difficulty of the amendment process locks in the current textual settlement against future majorities who lack the same supermajority position.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legislative_supermajority_incumbents, beneficiary,
    organized, generational, mobile, national).

% Bring claims that a law or practice is unjust or violates a moral principle not clearly reducible to enacted text — anti-discrimination claims resting on evolving moral consensus, dignity claims, claims about proportionality. Under this reading the claim fails at the threshold: the court asks only whether the text or its recognized incorporation doctrine supports the claim, not whether the claim is right. Their only remedy is legislative or Article V amendment, both of which require supermajorities they typically lack.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, substantive_justice_claimants, payer,
    moderate, biographical, constrained, national).

% Hold interests or claims to protection that were never reduced to enacted constitutional text and cannot be traced to a recognized formal source of law. Under strict formal-pedigree validity, their claims are legally invisible until captured by statute or amendment; they cannot exit the jurisdiction's authority structure and cannot force textual capture of their interest without political power they do not hold.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, unenumerated_rights_holders, payer,
    powerless, biographical, trapped, national).

% Groups whose substantive interests would require a constitutional amendment to formally vindicate, but who lack the political coalition size to clear Article V's supermajority thresholds. The formal-validity framework treats this blockage as legitimate — the process worked as designed — even though the practical effect is permanent exclusion from constitutional protection regardless of the merits of their claim.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, amendment_blocked_minorities, payer,
    powerless, generational, trapped, national).

% Argue that constitutional interpretation cannot be fully separated from moral reasoning because concepts like 'cruel,' 'unusual,' 'due process,' and 'equal protection' are themselves morally loaded terms that formal pedigree alone cannot resolve. Their critique is well-developed in academic literature but has no formal channel into validity determinations under this reading — courts may cite it in dicta but it does not bear on the pedigree test itself.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, moral_philosophy_scholars, excluded,
    moderate, civilizational, analytical, national).

% Study how the positivist reading interacts with originalist and living-constitutionalist readings, tracking when courts profess pedigree-based reasoning but import interpretive discretion through doctrines like incorporation, penumbras, or evolving standards — and documenting when the positivist frame collapses into originalism as a fallback disambiguation rule.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives courts, legislators, and citizens a shared, publicly verifiable test for what counts as valid law — was it enacted through the recognized procedure? — so that legal validity does not depend on resolving deep and permanent moral disagreement case by case.
% TRANSFER_FUNCTION: Moves the burden of moral argument out of adjudication and into the amendment process, which requires supermajority coalitions; this systematically favors groups who can assemble supermajorities and disfavors groups whose substantive claims lack, or cannot obtain, formal textual anchoring.
% ABSENT_VOICES: Unenumerated rights holders and amendment-blocked minorities have no forum in which the substantive merit of their claim is dispositive; moral philosophy scholars document the gap but their arguments do not enter the validity test itself. They are present in the academic conversation but structurally absent from the operative legal one.
% DISAPPEARANCE_RATIONALE: If courts abandoned formal-pedigree validity overnight, defenders of the reading argue the entire system of predictable, apolitical adjudication would collapse into ad hoc moral reasoning by unelected judges — a genuine coordination loss. Critics argue the practical effect would mainly be that substantive justice claims currently barred at the threshold would finally get a hearing on the merits, and that the 'collapse' feared is actually the loss of a shield that currently protects enacted injustice from moral scrutiny. The parties do not agree on which of these is the more accurate description of what would rearrange.
% FOUNDING_PROBLEM: The felt need, especially post-Lochner and in reaction to charges of judicial policy-making, for a theory of constitutional validity that does not require judges to adjudicate contested first-order moral questions — giving courts a legitimacy basis independent of whether their moral conclusions are shared by the public or by later generations.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivists and formalist judges attest the problem (judicial moral overreach) remains live and the framework is still doing necessary work. Critical legal studies scholars and civil rights historians, writing from outside the judiciary and outside the beneficiary set, attest that the 'neutral procedure' framing has itself functioned historically to insulate substantively unjust arrangements (e.g., pre-Reconstruction era slavery protections, later Jim Crow-adjacent doctrines) from moral challenge, and that the founding problem as originally stated was already serving an extractive function for incumbent power even at inception — no source entirely outside both camps corroborates a single clean genealogy.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, contested).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial but not extreme (0.58): the coordination function — a stable, verifiable validity test independent of judges' personal moral views — is real and valuable, which is why this is authored as tangled_rope rather than snare. But the same structure systematically transfers cost to claimants whose interests were never or cannot be textually captured, and that transfer requires active judicial enforcement (courts must actively decline to consider moral arguments that fall outside recognized doctrinal categories) — hence requires_active_enforcement: true. Suppression (0.62) is higher than extraction because the doctrine's exclusionary force (moral reasoning is inadmissible to the validity question, full stop) is a harder, more totalizing constraint than the variable transfer amount. Accessibility collapse (0.60) is moderate: political and doctrinal workarounds exist (incorporation doctrine, penumbral reasoning, statutory remedies) but they require capturing institutional access that many claimant groups lack.
 *
 * DIRECTIONALITY LOGIC:
 *   The sitting judiciary and procedural-legitimacy apparatus sit near the beneficiary end: they collect insulation from moral controversy and a stable, defensible decision rule. Legislative supermajority incumbents benefit because their formally enacted preferences are unconditionally valid while rivals' informally-supported claims are not. Substantive justice claimants, unenumerated rights holders, and amendment-blocked minorities sit near the target end: trapped or constrained exit, no channel to have the merits of their claim heard on its own terms, systematically structural rather than incidental disadvantage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing judges from smuggling contested personal morality into constitutional adjudication under the guise of interpretation — remains partially live: judicial legitimacy concerns about unelected moral policymaking have not disappeared. But the amendment channel that was supposed to be the release valve for legitimate substantive change has become practically inoperable (no successful contested amendment since 1971), which means the reading has drifted from 'procedure channels moral change through supermajority consensus' toward 'procedure freezes moral change indefinitely for any claim lacking existing textual anchor.' This is exactly the mandatrophy pattern: a coordination mechanism (channeling disagreement through legitimate procedure) persisting and hardening after its release valve seized up, becoming pure exclusion for claims that cannot clear the now much higher effective bar.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_originalist_collapse_dynamic,
    'When the formal text underdetermines an outcome and Article V amendment is gridlocked, does the positivist reading remain a distinct constraint, or does it functionally collapse into the originalist reading (since original historical meaning becomes the only remaining formally-anchored disambiguator)?',
    'Track cases where courts explicitly invoke pedigree-based reasoning but resolve textual underdetermination by resort to historical original meaning rather than any other formal source (statutory gloss, structural inference); a high rate of collapse would indicate the two readings are not independently operative in practice even though they rest on different foundational premises.',
    'If collapse is frequent and systematic, the positivist reading''s independent ε may be partly an artifact of doctrinal labeling rather than a genuinely distinct operative constraint from the originalist reading in the majority of hard cases — though the two remain conceptually and axiomatically distinct (source-of-validity vs. meaning-fixing-moment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_originalist_collapse_dynamic, conceptual, 'Whether positivism functionally reduces to originalism under amendment gridlock.').

omega_variable(
    morally_loaded_constitutional_terms,
    'Can terms like ''cruel and unusual,'' ''due process,'' and ''equal protection'' actually be resolved by pure formal pedigree, or do they smuggle moral reasoning back in under the guise of textual interpretation, making the positivist/moral-reasoning boundary illusory at the margin?',
    'Doctrinal analysis of whether courts purporting to apply pure pedigree-based reasoning to open-textured moral terms produce outcomes indistinguishable from courts explicitly reasoning from moral principle, controlling for case difficulty.',
    'If the boundary is illusory for open-textured clauses, the positivist reading''s claim to have excluded moral reasoning from validity determination is itself contestable, which would push some of the reading''s coordination claim toward the theater_ratio rather than genuine function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morally_loaded_constitutional_terms, conceptual, 'Whether formal-pedigree reasoning can actually exclude moral content from open-textured constitutional clauses.').

omega_variable(
    amendment_channel_functional_status,
    'Is the Article V amendment channel a genuinely available release valve that claimants have simply not yet organized sufficient supermajorities to use, or has it become structurally inoperable for any substantively contested question given modern polarization and supermajority thresholds?',
    'Historical base-rate analysis of contested (non-technical) amendment attempts since 1971 relative to prior eras, controlling for population and polarization measures.',
    'If structurally inoperable, the ''coordination function'' claim (moral change is channeled through legitimate supermajority process) becomes largely nominal for contested claims, strengthening the case that the reading has drifted toward pure extraction for amendment-blocked minorities; if merely underused, the coordination claim retains more force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_channel_functional_status, empirical, 'Whether the amendment process is a live coordination channel or a frozen formality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 1937, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_meaning__positivist_reading, theater_ratio, 1937, 0.18).
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_meaning__positivist_reading, theater_ratio, 1954, 0.2).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_meaning__positivist_reading, theater_ratio, 1973, 0.22).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_meaning__positivist_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(us_c_tr_t2008, us_constitution_meaning__positivist_reading, theater_ratio, 2008, 0.26).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_meaning__positivist_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1937, us_constitution_meaning__positivist_reading, base_extractiveness, 1937, 0.42).
narrative_ontology:measurement(us_c_be_t1954, us_constitution_meaning__positivist_reading, base_extractiveness, 1954, 0.4).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_meaning__positivist_reading, base_extractiveness, 1973, 0.45).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_meaning__positivist_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(us_c_be_t2008, us_constitution_meaning__positivist_reading, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_meaning__positivist_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1937, us_constitution_meaning__positivist_reading, suppression_requirement, 1937, 0.48).
narrative_ontology:measurement(us_c_su_t1954, us_constitution_meaning__positivist_reading, suppression_requirement, 1954, 0.5).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_meaning__positivist_reading, suppression_requirement, 1973, 0.52).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_meaning__positivist_reading, suppression_requirement, 1990, 0.56).
narrative_ontology:measurement(us_c_su_t2008, us_constitution_meaning__positivist_reading, suppression_requirement, 2008, 0.59).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_meaning__positivist_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the meaning of the Constitution' per the ε-invariance principle: the positivist reading (this file, addressing the SOURCE of validity — procedure vs. morality), the originalist reading (addressing the FIXING of meaning at ratification, a hermeneutic claim), and the living-constitutionalist reading (addressing the EVOLUTION of application under enduring principles, a normative-evolution claim). Each has a distinct ε, distinct beneficiary/victim structure, and distinct classification; they are linked here rather than merged because measuring 'the Constitution's meaning' by these three different lenses produces materially different extraction profiles — exactly the decomposition trigger the framework requires rather than folding observer-dependence into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
