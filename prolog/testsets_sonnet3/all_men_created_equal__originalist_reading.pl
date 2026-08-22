% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Equality Clause Bounded by 18th-Century Social Taxonomy (Originalist Reading)
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the originalist reading — of the
 *   contested 'all men are created equal' kernel. Under this reading, the
 *   equality clause's scope is fixed at ratification and bounded by the
 *   founders' actual 18th-century social taxonomy (propertied white men),
 *   regardless of the clause's universal grammatical form. This is a narrow,
 *   high-extraction reading: it treats the founders' intent as dispositive,
 *   which means the historically excluded categories (enslaved persons,
 *   indigenous nations, women, non-propertied men) remain outside the
 *   clause's original protective scope unless brought in by subsequent
 *   amendment rather than reinterpretation. This is NOT the universalist
 *   reading (which treats the same text as requiring iterative expansion) or
 *   the textualist-paradox reading (which treats the universal language and
 *   restricted application as an exposed contradiction) — those are separate
 *   constraints, linked here by network reference, each with their own ε and
 *   stakeholder structure.
 *
 * KEY AGENTS:
 *   - propertied_white_male_founders_and_descendants: beneficiary of default-category status inherited from the founding taxonomy
 *   - slaveholding_planter_class_descendants: direct beneficiary of the taxonomy that excluded enslaved persons from personhood
 *   - originalist_judicial_tradition: agenda-setter administering fixed-meaning interpretive methodology
 *   - enslaved_persons_and_descendants: primary historical victim of the bounded taxonomy
 *   - indigenous_nations: excluded from the category entirely, not merely subordinated within it
 *   - women_excluded_from_founding_polity: excluded from civic personhood under the operative 18th-century taxonomy
 *   - constitutional_historians: analytical observers whose findings are selectively invoked by the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.81).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.72).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Equality Clause Bounded by 18th-Century Social Taxonomy (Originalist Reading)").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '2d7ce301-4964-41cb-a962-013afdbc8c96').
narrative_ontology:cs_kernel_codification('2d7ce301-4964-41cb-a962-013afdbc8c96', fixed_text).
narrative_ontology:cs_authority_grounding('2d7ce301-4964-41cb-a962-013afdbc8c96', lineage).
narrative_ontology:cs_interpretation_layer_present('2d7ce301-4964-41cb-a962-013afdbc8c96').
narrative_ontology:cs_reading_relation('2d7ce301-4964-41cb-a962-013afdbc8c96', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d7ce301-4964-41cb-a962-013afdbc8c96', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('2d7ce301-4964-41cb-a962-013afdbc8c96', foundational, founders_intent_is_dispositive_for_scope).
narrative_ontology:cs_axiom_status(founders_intent_is_dispositive_for_scope, holdable).
narrative_ontology:cs_axiom_grounding('2d7ce301-4964-41cb-a962-013afdbc8c96', founders_intent_is_dispositive_for_scope, conventional).
narrative_ontology:cs_axiom('2d7ce301-4964-41cb-a962-013afdbc8c96', foundational, clause_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(clause_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2d7ce301-4964-41cb-a962-013afdbc8c96', clause_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('2d7ce301-4964-41cb-a962-013afdbc8c96', secondary, expansion_requires_amendment_not_reinterpretation).
narrative_ontology:cs_axiom_status(expansion_requires_amendment_not_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('2d7ce301-4964-41cb-a962-013afdbc8c96', expansion_requires_amendment_not_reinterpretation, instrumental).
narrative_ontology:cs_reference_frame('2d7ce301-4964-41cb-a962-013afdbc8c96', founding_era_social_taxonomy).
narrative_ontology:cs_drift_state('2d7ce301-4964-41cb-a962-013afdbc8c96', post_civil_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2d7ce301-4964-41cb-a962-013afdbc8c96', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, propertied_white_male_founders_and_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, slaveholding_planter_class_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_judicial_tradition).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_persons_and_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_nations).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women_excluded_from_founding_polity).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_propertied_white_men_at_founding).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, non_propertied_white_men_at_founding).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, founders_intent_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, fixed_meaning_at_ratification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupied the category the founding document's 'men' actually denoted — propertied, white, male heads of household. Their descendants inherit the presumption that constitutional equality was always meant to include people like them, and inherit the political, legal, and economic capital that flowed from being the unmarked default category. They administer the reading through appointment to judicial and academic posts that treat the founders' 18th-century social taxonomy as the fixed reference point.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, propertied_white_male_founders_and_descendants, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__originalist_reading, propertied_white_male_founders_and_descendants, agenda_setter).

% Directly benefited from a founding taxonomy that categorized enslaved persons as property rather than persons within 'all men.' Descendants retain wealth, land, and institutional standing built on that exclusion, and the originalist reading retroactively legitimates the founding arrangement as the document's true and fixed meaning rather than a defect to be corrected.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, slaveholding_planter_class_descendants, beneficiary,
    organized, civilizational, arbitrage, national).

% Sets the interpretive methodology: constitutional meaning is fixed at ratification and recoverable through founders' intent and 18th-century usage. Administers this through judicial appointment, legal scholarship, and precedent. Could adopt a different interpretive frame but has strong professional and ideological investment in the fixed-meaning approach; the cost of abandoning it is career and institutional, not merely intellectual.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_judicial_tradition, agenda_setter,
    institutional, generational, arbitrage, national).

% Categorically excluded from 'all men' at the founding by the very social taxonomy this reading holds authoritative. Descendants inherited generations of legal, economic, and civic exclusion justified by appeal to what the founders meant. Exit from the constraint's reach required either full abolition of the taxonomy (Reconstruction Amendments) or reinterpretation the originalist reading resists as illegitimate expansion beyond original scope.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_persons_and_descendants, payer,
    powerless, civilizational, trapped, national).

% Excluded from the founding polity's conception of 'men' as citizens or rights-bearers within the constitutional order; treated as external nations or domestic dependents rather than as constituents of 'all men' at all. The originalist reading offers no path to inclusion because the founders' taxonomy did not contemplate indigenous nations as candidates for the category.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_nations, payer,
    powerless, civilizational, trapped, national).

% Fell outside the founders' operative definition of 'men' as a civic and legal category, regardless of the word's occasional generic usage. Under the originalist reading, women's exclusion from suffrage and civic equality reflects the document's actual original scope, not a later betrayal of it — meaning inclusion required constitutional amendment rather than reinterpretation of the existing clause.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women_excluded_from_founding_polity, payer,
    powerless, civilizational, constrained, national).

% Nominally within the racial and sex category the founders used but excluded by property and status qualifications from full civic participation at the founding. Benefited relative to enslaved and indigenous people and women, but bore a lesser version of the same taxonomic exclusion — property requirements narrowed even the 'men' the founders had in mind.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, non_propertied_white_men_at_founding, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__originalist_reading, non_propertied_white_men_at_founding, beneficiary).

% Study what the founders actually believed and meant by 'all men,' drawing on convention debates, contemporary usage, and the coexistence of slavery with the equality clause. Their findings are cited by both the originalist tradition and its critics, but the originalist reading selectively treats their historical reconstruction as dispositive for legal meaning rather than as one input among several.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, constitutional_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, propertied_white_male_founders_and_descendants).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides judicial and political actors a determinate, historically anchored standard for resolving equality claims — fixed meaning at ratification avoids indefinitely open-ended reinterpretation and gives the clause stable, predictable legal content.
% TRANSFER_FUNCTION: Moves the burden of proving inclusion onto excluded groups (who must secure constitutional amendment or reinterpretation) while the founding elite's descendants retain default beneficiary status without having to argue for it — political, legal, and reputational capital flows toward those the taxonomy already included.
% ABSENT_VOICES: Enslaved persons, indigenous nations, and women had no voice in the constitutional convention that fixed the taxonomy this reading treats as authoritative; their descendants and successor advocacy movements object to the reading today but are not positioned to change what 'the founders intended' since intent is fixed at a moment they were excluded from.
% DISAPPEARANCE_RATIONALE: If the originalist reading disappeared as the governing interpretive frame, courts would lose the fixed-meaning anchor currently used to cabin equal-protection claims to founding-era categories, and pending doctrinal fights over the scope of constitutional equality would shift decisively toward reasoning from the universalist or textualist-paradox readings — a substantial swath of current equal-protection jurisprudence would need to be reargued on different interpretive premises.
% FOUNDING_PROBLEM: The founders needed a unifying revolutionary and governing principle that could justify separation from Britain and legitimate the new political order, without disturbing the existing economic and social hierarchy (slavery, coverture, property qualifications) that many founders' own wealth depended on.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the beneficiary set (drawing on convention records, contemporaneous correspondence, and the documented coexistence of the equality clause with slavery and coverture in the same founding generation) attest that the founders' operative taxonomy was deliberately narrower than the clause's universal language — corroboration exists independent of both originalist defenders and critics, though its implications for present legal meaning remain disputed between the readings.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because this reading's function is precisely to cabin the clause's protective scope to the founders' actual social categories, systematically excluding groups the founders did not include — the exclusion is the constraint's operative content, not an incidental side effect. Suppression is high (0.72) because maintaining this reading against the plain universal language of the text requires active interpretive and sometimes coercive enforcement (case law, doctrine, historically: violence and law enforcing the excluded categories' subordination). Accessibility collapse is moderate (0.4), not high, because unlike a mountain, alternative readings of the same text are visibly available and contested — the originalist reading has never achieved the kind of totalizing closure a genuine natural law would. Resistance is high (0.75) reflecting the sustained, organized challenge this reading has faced from abolitionist, suffragist, civil rights, and indigenous rights movements across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (originalist judicial tradition), this reading is a principled constraint on judicial overreach — a rope solving the coordination problem of preventing courts from reading their own values into the text. From the payer seats, the identical structure operates as an enforced tangled rope: real coordination value (predictability, restraint on judicial discretion) bundled with asymmetric extraction (perpetual exclusion absent formal amendment). The engine's per-seat computation should reflect this divergence without either seat's view being privileged in the authored data.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white male founders and their descendants sit at the beneficiary end: the taxonomy the reading enforces is the taxonomy that always already included them, so they bear none of the burden of proving inclusion and inherit accumulated capital from that default status. Enslaved persons, indigenous nations, and women sit at the target end: the reading's entire function, from their structural position, is to hold the clause's protection at bay by appeal to a taxonomy that excluded them by design. Non-propertied white men occupy an intermediate position — nominally within the racial/sex category but excluded by property qualification, giving them a lesser but real version of the same exclusion mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing a unifying principle for revolutionary legitimacy without disturbing the existing hierarchy — is largely dead as a live problem (the revolutionary moment has passed, the hierarchy it protected has been formally dismantled by amendment), yet the originalist reading persists as an active interpretive constraint on how far the clause's protection extends today. This mismatch (dead founding problem, world_rearranges disappearance verdict) is exactly the capture/zombie signature the R5 genealogy interview is designed to surface: the reading's stated justification (fidelity to fixed meaning) increasingly diverges from what the reading now functions to do (bound the pace and scope of equality's application).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_reading_vs_sibling_readings,
    'Is the originalist reading''s fixed-meaning methodology itself neutral interpretive practice, or is methodological commitment to ''founders'' intent'' selected because it happens to produce outcomes favorable to the founding elite''s descendants?',
    'Compare originalist outcomes across issue areas where founder intent is genuinely ambiguous or contested — if the reading consistently resolves ambiguity toward outcomes favoring the historically included category, that is evidence the methodology functions as motivated reasoning rather than neutral historical recovery.',
    'If the methodology is neutral, the high extraction is a byproduct of accurately recovering an admittedly narrow founding intent (defensible as historical fact even if morally troubling). If motivated, the extraction is doing interpretive work disguised as historical fidelity, strengthening the tangled_rope/snare reading over any coordination-function defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_reading_vs_sibling_readings, conceptual, 'Whether originalist methodology is neutral historical recovery or motivated selection of a favorable interpretive frame.').

omega_variable(
    founders_intent_recoverability,
    'Is a single, determinate ''founders'' intent'' regarding the scope of ''all men'' actually recoverable, given documented disagreement among the founders themselves (e.g., over slavery, property qualifications, and women''s civic status)?',
    'Systematic review of convention records, ratification debates, and founders'' private correspondence for evidence of a consensus versus contested or absent intent on the clause''s scope.',
    'If founders'' intent was itself contested or indeterminate at the time, the originalist reading''s claim to recover a single fixed meaning is undermined at the root, and the reading''s authority rests on selecting one faction''s intent as authoritative rather than on genuine historical recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founders_intent_recoverability, empirical, 'Whether the founders held a single determinate intent regarding the equality clause''s scope, or only a contested and divided one.').

omega_variable(
    reading_selection_as_political_choice,
    'Given that all three readings (originalist, universalist, textualist-paradox) are available and internally coherent, is the selection among them itself a political act rather than a purely interpretive or historical one?',
    'Track which institutional actors adopt which reading and whether adoption correlates with material interest in the outcome (e.g., whether courts favoring originalism disproportionately rule against claims by historically excluded groups).',
    'If reading-selection correlates strongly with material interest, the kernel-contest itself is better modeled as a site of ongoing political struggle rather than a purely interpretive disagreement, which would support treating cross-reading network effects as central to any full account of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_political_choice, conceptual, 'Whether choice among sibling kernel readings tracks interpretive merit or material interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1787, all_men_created_equal__originalist_reading, theater_ratio, 1787, 0.1).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__originalist_reading, theater_ratio, 1865, 0.15).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__originalist_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__originalist_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(all__tr_t1990, all_men_created_equal__originalist_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__originalist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(all__be_t1787, all_men_created_equal__originalist_reading, base_extractiveness, 1787, 0.92).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__originalist_reading, base_extractiveness, 1865, 0.86).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__originalist_reading, base_extractiveness, 1920, 0.78).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__originalist_reading, base_extractiveness, 1965, 0.68).
narrative_ontology:measurement(all__be_t1990, all_men_created_equal__originalist_reading, base_extractiveness, 1990, 0.74).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__originalist_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1787, all_men_created_equal__originalist_reading, suppression_requirement, 1787, 0.95).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__originalist_reading, suppression_requirement, 1865, 0.88).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__originalist_reading, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement(all__su_t1965, all_men_created_equal__originalist_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(all__su_t1990, all_men_created_equal__originalist_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__originalist_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__originalist_reading, 0.1).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'the equality clause' per the ε-invariance principle. The originalist reading (this file) authors high extraction (0.81) and a narrow victim set derived from founding-era social taxonomy. The universalist_reading authors substantially lower extraction and a much broader beneficiary set (all persons brought within the clause's scope over time). The textualist_paradox_reading authors a distinct structural claim about the text's internal contradiction rather than about correct interpretive method, with its own beneficiary/victim structure (beneficiaries: those who profit from unresolved ambiguity; victims: those harmed by the gap between stated and applied principle). All three share the same underlying kernel text but are structurally distinct constraints with different ε, different stakeholders, and different classifications — they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
