% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive-Originalist Reading of the Equality Clause (Propertied White Male Political Actors)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint captures the restrictive-originalist reading of the
 *   equality clause: the claim that the clause's political-actor guarantee
 *   applies, as a matter of original public meaning, to propertied white
 *   males situated as parties to the 18th-century social contract, and that
 *   its scope cannot be judicially expanded to reach other groups absent
 *   formal constitutional amendment. This is one of three sibling readings of
 *   a single contested kernel (equality_clause_scope) — the
 *   expansive_universalist reading treats the clause as self-evidently
 *   applying to all humans regardless of the framers' exclusions, and the
 *   progressive_textualist reading treats the text as containing a genuine
 *   equality principle whose application scope properly expands through the
 *   amendment process rather than judicial reinterpretation. Each reading is
 *   a structurally distinct constraint with its own ε, beneficiary set, and
 *   enforcement mechanism; this file models only the restrictive-originalist
 *   reading.
 *
 * KEY AGENTS:
 *   - propertied_white_male_citizens: primary beneficiary (powerful/arbitrage) — retains full original-scope political-actor status
 *   - founding_era_political_establishment: agenda_setter (institutional/analytical) — fixed the original scope through drafting and ratification
 *   - originalist_judicial_interpreters: agenda_setter (institutional/analytical) — administers the fixed-meaning doctrine and channels expansion demands to the amendment process
 *   - enslaved_persons, free_black_citizens, women_of_all_classes, unpropertied_white_men, indigenous_peoples: excluded/victim classes — categorically outside original scope, dependent on subsequent amendment for recognition
 *   - constitutional_historians: analytical observer — reconstructs original public meaning from drafting and ratification record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.71).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.78).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.71).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive-Originalist Reading of the Equality Clause (Propertied White Male Political Actors)").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '2e2569a0-44de-4a34-a052-531d47a8aff2').
narrative_ontology:cs_kernel_codification('2e2569a0-44de-4a34-a052-531d47a8aff2', fixed_text).
narrative_ontology:cs_authority_grounding('2e2569a0-44de-4a34-a052-531d47a8aff2', lineage).
narrative_ontology:cs_interpretation_layer_present('2e2569a0-44de-4a34-a052-531d47a8aff2').
narrative_ontology:cs_reading_relation('2e2569a0-44de-4a34-a052-531d47a8aff2', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('2e2569a0-44de-4a34-a052-531d47a8aff2', equality_clause_scope__progressive_textualist, influences).
narrative_ontology:cs_axiom('2e2569a0-44de-4a34-a052-531d47a8aff2', foundational, equality_scope_fixed_at_ratification).
narrative_ontology:cs_axiom_status(equality_scope_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2e2569a0-44de-4a34-a052-531d47a8aff2', equality_scope_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('2e2569a0-44de-4a34-a052-531d47a8aff2', foundational, political_actor_status_requires_original_social_contract_membership).
narrative_ontology:cs_axiom_status(political_actor_status_requires_original_social_contract_membership, holdable).
narrative_ontology:cs_axiom_grounding('2e2569a0-44de-4a34-a052-531d47a8aff2', political_actor_status_requires_original_social_contract_membership, conventional).
narrative_ontology:cs_axiom('2e2569a0-44de-4a34-a052-531d47a8aff2', secondary, scope_expansion_requires_formal_amendment_not_reinterpretation).
narrative_ontology:cs_axiom_status(scope_expansion_requires_formal_amendment_not_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('2e2569a0-44de-4a34-a052-531d47a8aff2', scope_expansion_requires_formal_amendment_not_reinterpretation, instrumental).
narrative_ontology:cs_reference_frame('2e2569a0-44de-4a34-a052-531d47a8aff2', founding_era_social_contract_political_actor_class).
narrative_ontology:cs_drift_state('2e2569a0-44de-4a34-a052-531d47a8aff2', contemporary_constitutional_practice, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2e2569a0-44de-4a34-a052-531d47a8aff2', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, founding_era_political_establishment).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, originalist_judicial_interpreters).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, free_black_citizens).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women_of_all_classes).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, unpropertied_white_men).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, indigenous_peoples).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, social_contract_political_actor_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the franchise, stand for office, own property that qualifies them as political actors under the original social contract framing, and enjoy the full protection and application of the equality clause as written and originally understood. They benefit from a reading that treats their enfranchisement as the settled, complete scope of the constitutional guarantee.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens, beneficiary,
    powerful, generational, arbitrage, national).

% Drafted and ratified the founding text, embedding a specific historically-situated conception of who counts as a political actor. Their drafting choices are treated by this reading as fixing the clause's scope permanently absent formal amendment; they set the interpretive terms that later originalist courts and scholars administer.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, founding_era_political_establishment, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, founding_era_political_establishment, beneficiary).

% Apply the doctrine that the clause's meaning is fixed at ratification and that any expansion of its beneficiary class requires a new constitutional amendment rather than judicial reinterpretation. They administer the boundary between what counts as legitimate constitutional change and what counts as illegitimate judicial expansion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_judicial_interpreters, agenda_setter,
    institutional, civilizational, analytical, national).

% Categorically excluded from political-actor status under the original framework; the equality clause as originally applied does not reach them at all. Under this reading, their exclusion is not a violation of the clause but outside its intended scope entirely, requiring the later Thirteenth and Fourteenth Amendments to address, not reinterpretation of the original text.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_persons, excluded,
    powerless, biographical, trapped, national).

% Even where legally free, treated as outside the original political-actor class in most founding-era jurisdictions. Under this reading, their claims to equal citizenship depend entirely on subsequent amendment, not on the original clause's self-executing force.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, free_black_citizens, excluded,
    powerless, biographical, trapped, national).

% Excluded from the franchise and from political-actor status regardless of property ownership, race, or class. This reading treats that exclusion as consistent with the clause's original meaning, requiring the Nineteenth Amendment rather than judicial reinterpretation to establish suffrage.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women_of_all_classes, excluded,
    powerless, generational, constrained, national).

% Share the race and sex of the favored class but lack the property qualification that originally defined political actorhood, and so are excluded under the strict original scope until state-level and later federal reforms removed property qualifications for voting.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, unpropertied_white_men, excluded,
    powerless, biographical, constrained, national).

% Treated as outside the political community altogether under the founding framework — neither citizens nor subject to ordinary constitutional protections in the original design. This reading locates their exclusion as a foundational premise of the social contract as drafted, not an oversight subject to reinterpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, indigenous_peoples, excluded,
    powerless, civilizational, trapped, national).

% Seek judicial recognition of expanded equality protections through interpretation of existing text. Under this reading, their claims are properly directed at the amendment process, not at courts reinterpreting the original clause; they experience the restrictive reading as a procedural wall between them and the outcomes they seek.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, modern_civil_rights_claimants, excluded,
    moderate, generational, constrained, national).

% Study the drafting record, ratification debates, and contemporaneous usage to establish what the framers understood the clause to mean. Their scholarship is invoked by originalist interpreters as evidence for original public meaning, and contested by scholars favoring other interpretive methods.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, ascertainable standard for what the equality clause originally guaranteed and to whom, allowing courts to distinguish legitimate constitutional interpretation from judicial policymaking, and channeling demands for expanded rights into the amendment process where they can claim durable democratic legitimacy.
% TRANSFER_FUNCTION: Preserves the political, legal, and social advantages of the original propertied white male beneficiary class by treating their historically-conferred status as the textually complete scope of the equality guarantee, while placing the burden of formal constitutional amendment on every excluded group seeking recognition as political actors.
% ABSENT_VOICES: Enslaved persons, free Black citizens, women, unpropertied men, and Indigenous peoples had no voice in the drafting or ratification of the original text whose 18th-century scope this reading treats as authoritative and fixed; their absence from the founding conversation is treated by this reading as a fact about original meaning rather than a defect requiring correction through reinterpretation.
% DISAPPEARANCE_RATIONALE: If the restrictive-originalist reading disappeared as the operative interpretive framework, courts would be free to treat the equality clause as self-executing against later-recognized exclusions without requiring formal amendment, dramatically expanding the pool of claims cognizable through litigation rather than legislation — a substantial reallocation of institutional power from legislatures and amendment processes to courts, and a substantial expansion of who counts as holding enforceable equality claims under the original text.
% FOUNDING_PROBLEM: The framers needed to establish which persons counted as political actors capable of holding, exercising, and being bound by the social contract's reciprocal obligations, given the property, race, and sex-based political theory dominant in their era, while creating durable, judicially administrable criteria for citizenship and political participation.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars and some constitutional historians attest that the founding problem — defining a stable, textually fixed political-actor class — remains the doctrine's operative function today. Civil rights historians, comparative constitutional scholars, and the drafters and beneficiaries of the Thirteenth, Fourteenth, Fifteenth, and Nineteenth Amendments attest from outside the originalist interpretive community that the founding-era scope was itself the injustice requiring correction, and that treating it as the fixed baseline for interpretation preserves rather than resolves the original problem of exclusion.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.71 at 2024) because the reading's practical effect is to preserve the founding-era beneficiary class's relative advantage by treating later exclusions as constitutionally exogenous rather than as violations requiring correction through the same interpretive mechanism that established the clause. Suppression is high (0.78) because the doctrine's persistence depends on actively foreclosing judicial avenues for redress, forcing every excluded group through the much higher-cost amendment process. Theater ratio is moderate and rising (0.42) because a substantial share of contemporary originalist argument is now devoted to defending the interpretive method itself against charges of result-orientation, rather than to the underlying historical inquiry. The temporal series shows extraction and suppression declining through the amendment era (1868-1965) as the Reconstruction and suffrage amendments formally expanded scope, then rising again post-1990 as originalism reasserted a narrower baseline against subsequent statutory and doctrinal expansions — the U-shaped curve reflects contestation over whether amendment-driven expansion settles the question permanently or whether the restrictive reading reasserts itself as the default absent continuous amendment pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary and agenda-setter seats, this reading is a Rope-like coordination device: a stable, judicially administrable standard preventing courts from imposing contested contemporary values under the guise of interpretation. From the excluded seats, the identical structure operates as a Tangled Rope at best — a genuine coordination function (interpretive stability) fused with an asymmetric extraction (the perpetuation of founding-era exclusions until a supermajoritarian process removes them) — and the requirement of active judicial enforcement to maintain the restrictive scope against expansive claims is what makes it Tangled Rope rather than Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white male citizens sit at the beneficiary end: the clause as originally scoped confers on them the full, uncontested status of political actors, and this reading treats that status as complete and self-justifying rather than as one historically contingent allocation among possible ones. Excluded groups sit at the target end with trapped or constrained exit: their only path to recognition under this reading is the amendment process, which requires supermajoritarian political mobilization they historically lacked the franchise to exercise. The institutional agenda-setters (founding-era establishment, originalist interpreters) occupy an analytical exit position because their role is administering the doctrine's boundaries, not living inside its consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing a stable political-actor class — is genuinely contested as to its current status. Under this reading, the problem was legitimately solved in 1787 for the class it was designed to cover, and later exclusions are properly addressed by amendment (a claim the doctrine treats as still live and structurally sound). From outside the originalist tradition, the founding problem as originally framed was itself the injustice, and treating the founding-era scope as the interpretive baseline preserves rather than resolves it — the amendment process becomes a legitimacy-laundering mechanism that makes the original exclusion look like a solved problem rather than an ongoing one requiring continuous political struggle to overcome. This is precisely the seat-divergence the engine is built to surface: the same doctrine reads as principled restraint from the agenda-setter seat and as an entrenchment mechanism from the excluded seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_vs_constructed_exclusion,
    'Is the restrictive-originalist reading a genuine recovery of the framers'' actual understanding, or a retroactively constructed interpretive method that happens to preserve the advantages of the historically enfranchised class?',
    'Comparative analysis of founding-era drafting records, ratification debates, and contemporaneous legal commentary against the historical development of originalism as a named interpretive method (which emerged primarily in the late 20th century in response to expansive mid-century jurisprudence) — if the method''s rise correlates more closely with resistance to specific expansive rulings than with independent historiographical developments, this supports the constructed-method reading.',
    'If genuinely a recovery of original meaning, the doctrine functions closer to a principled (if exclusionary) coordination mechanism; if substantially constructed to resist 20th-century expansion, the doctrine functions closer to a Snare wearing historical-recovery framing as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_vs_constructed_exclusion, conceptual, 'Whether originalism is genuine historical recovery or a constructed defensive doctrine.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the equality_clause_scope kernel supports at least three coherent readings (restrictive_originalist, progressive_textualist, expansive_universalist) with materially different beneficiary sets and legitimacy conditions, what determines which reading a given court or political actor adopts, and is that selection itself principled or outcome-driven?',
    'Track correlation between judges'' or scholars'' substantive policy preferences and their choice of interpretive reading across a large sample of cases; if reading choice correlates strongly with predicted substantive outcome rather than with independent methodological commitment, this suggests reading selection is itself instrumentalized.',
    'If reading selection is outcome-driven, no single reading (including this one) can claim privileged legitimacy on originalist grounds alone, and the entire kernel contest becomes a proxy for underlying political conflict rather than a genuine interpretive dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the choice among sibling readings of the equality kernel is principled or outcome-selected.').

omega_variable(
    amendment_process_accessibility,
    'Is the constitutional amendment process this reading directs excluded groups toward realistically accessible to groups who, by definition, lacked the franchise and political power the original scope reserved to propertied white males?',
    'Historical analysis of the time elapsed and political mobilization required for each expansion amendment (13th, 14th, 15th, 19th) relative to the population share affected and their contemporaneous political power, compared to a counterfactual judicial-reinterpretation pathway.',
    'If the amendment process is empirically inaccessible to the groups this reading excludes without decades or centuries of prior political mobilization, the reading''s procedural fairness claim (excluded groups have a legitimate path, just not through courts) is substantially weakened, supporting a Snare-adjacent reading of the doctrine''s practical effect despite its Tangled Rope structural form.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_process_accessibility, empirical, 'Whether the amendment pathway this reading prescribes is realistically available to excluded groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1787, equality_clause_scope__restrictive_originalist, theater_ratio, 1787, 0.15).
narrative_ontology:measurement_basis(equa_tr_t1787, observed).
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__restrictive_originalist, theater_ratio, 1868, 0.25).
narrative_ontology:measurement_basis(equa_tr_t1868, observed).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__restrictive_originalist, theater_ratio, 1920, 0.3).
narrative_ontology:measurement_basis(equa_tr_t1920, observed).
narrative_ontology:measurement(equa_tr_t1965, equality_clause_scope__restrictive_originalist, theater_ratio, 1965, 0.35).
narrative_ontology:measurement_basis(equa_tr_t1965, observed).
narrative_ontology:measurement(equa_tr_t1990, equality_clause_scope__restrictive_originalist, theater_ratio, 1990, 0.38).
narrative_ontology:measurement_basis(equa_tr_t1990, observed).
narrative_ontology:measurement(equa_tr_t2024, equality_clause_scope__restrictive_originalist, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(equa_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 1787, 0.85).
narrative_ontology:measurement_basis(equa_be_t1787, observed).
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__restrictive_originalist, base_extractiveness, 1868, 0.68).
narrative_ontology:measurement_basis(equa_be_t1868, observed).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__restrictive_originalist, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement_basis(equa_be_t1920, observed).
narrative_ontology:measurement(equa_be_t1965, equality_clause_scope__restrictive_originalist, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement_basis(equa_be_t1965, observed).
narrative_ontology:measurement(equa_be_t1990, equality_clause_scope__restrictive_originalist, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement_basis(equa_be_t1990, observed).
narrative_ontology:measurement(equa_be_t2024, equality_clause_scope__restrictive_originalist, base_extractiveness, 2024, 0.71).
narrative_ontology:measurement_basis(equa_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 1787, 0.92).
narrative_ontology:measurement_basis(equa_su_t1787, observed).
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__restrictive_originalist, suppression_requirement, 1868, 0.8).
narrative_ontology:measurement_basis(equa_su_t1868, observed).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__restrictive_originalist, suppression_requirement, 1920, 0.72).
narrative_ontology:measurement_basis(equa_su_t1920, observed).
narrative_ontology:measurement(equa_su_t1965, equality_clause_scope__restrictive_originalist, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement_basis(equa_su_t1965, observed).
narrative_ontology:measurement(equa_su_t1990, equality_clause_scope__restrictive_originalist, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement_basis(equa_su_t1990, observed).
narrative_ontology:measurement(equa_su_t2024, equality_clause_scope__restrictive_originalist, suppression_requirement, 2024, 0.78).
narrative_ontology:measurement_basis(equa_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equality_clause_scope kernel, each authored as a separate constraint story per the ε-invariance principle. restrictive_originalist (this file) has the narrowest beneficiary set and highest measured extraction/suppression; progressive_textualist shares this reading's commitment to amendment-based expansion but rejects the narrow original-scope premise, occupying an intermediate ε; expansive_universalist treats the clause as self-evidently universal, with the lowest ε and broadest beneficiary class. All three are linked bidirectionally via affects_constraints to preserve the kernel-family structure for contamination and drift analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
