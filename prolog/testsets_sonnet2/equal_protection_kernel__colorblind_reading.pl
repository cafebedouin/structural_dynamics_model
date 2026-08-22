% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__colorblind_reading, []).

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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Equal Protection Clause — Colorblind (Anticlassification) Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the colorblind (anticlassification) reading of
 *   the Equal Protection Clause: the clause is read as categorically
 *   forbidding state use of racial classifications regardless of the state's
 *   purpose, benign or invidious. This reading has moved from a minority
 *   position (Justice Harlan's dissent in Plessy, then a growing strand of
 *   doctrine through the late twentieth century) to the controlling reading
 *   in the most recent line of cases striking down race-conscious admissions.
 *   Sibling readings — remedial and antisubordination — are separate
 *   constraints, not alternative measurements of this one; each has its own
 *   ε, its own beneficiary/victim structure, and its own classification. This
 *   file authors ε, suppression, and the stakeholder structure ONLY as they
 *   operate under the colorblind reading's own terms.
 *
 * KEY AGENTS:
 *   - federal_judiciary_majority: sets and enforces the categorical bar (institutional/analytical)
 *   - underrepresented_minority_applicants: bears the loss of the remedial admissions pathway (powerless/trapped)
 *   - selective_institutions_seeking_liability_shield: administers admissions under the new bright-line rule (institutional/constrained)
 *   - colorblind_legal_movement: organized advocacy movement that achieved doctrinal capture (organized/mobile)
 *   - constitutional_scholars_observer: analytical seat tracking the doctrinal contest across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.58).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.62).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection Clause — Colorblind (Anticlassification) Reading").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '31aaeea0-c90f-4565-ba9e-9fa2297ea45c').
narrative_ontology:cs_kernel_codification('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', fixed_text).
narrative_ontology:cs_authority_grounding('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', lineage).
narrative_ontology:cs_interpretation_layer_present('31aaeea0-c90f-4565-ba9e-9fa2297ea45c').
narrative_ontology:cs_reading_relation('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', foundational, racial_classification_categorically_impermissible).
narrative_ontology:cs_axiom_status(racial_classification_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', racial_classification_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', foundational, purpose_irrelevant_to_classification_analysis).
narrative_ontology:cs_axiom_status(purpose_irrelevant_to_classification_analysis, holdable).
narrative_ontology:cs_axiom_grounding('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', purpose_irrelevant_to_classification_analysis, conventional).
narrative_ontology:cs_reference_frame('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', textual_formal_equality_framework).
narrative_ontology:cs_drift_state('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', post_2023_admissions_rulings, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('31aaeea0-c90f-4565-ba9e-9fa2297ea45c', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, incumbent_advantaged_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, selective_institutions_seeking_liability_shield).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, colorblind_legal_movement).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_communities).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, diversity_dependent_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the clause as a categorical bar on racial classification by state actors, strikes down race-conscious admissions frameworks, and enforces strict scrutiny that in practice becomes near-fatal-in-fact for any racial classification, regardless of the state's remedial or diversity justification.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, federal_judiciary_majority, agenda_setter,
    institutional, generational, analytical, national).

% Compete for selective admissions slots under a formally identical rule set that no longer weighs race as a factor; benefits from the removal of a competing consideration that previously affected relative standing, without needing to organize or litigate to receive the gain.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, incumbent_advantaged_applicants, beneficiary,
    moderate, biographical, mobile, national).

% Lose access to an admissions pathway that previously accounted for the compounding effects of segregation, wealth gaps, and unequal school funding; cannot litigate around the ruling and cannot relocate their way out of a national doctrine that applies uniformly across public and federally-funded private institutions.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, underrepresented_minority_applicants, payer,
    powerless, biographical, trapped, national).

% Bear the accumulated, multi-generational effects of exclusion that the remedial framework existed to address; the colorblind reading forecloses any state acknowledgment of those effects as a legitimate basis for action, leaving the community with no institutional remedy beyond private-sector or philanthropic substitutes.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_communities, payer,
    powerless, generational, trapped, national).

% Administers admissions and can redesign criteria; benefits from a bright-line rule that reduces litigation exposure and administrative discretion over contested racial-preference decisions, even where the institution's own diversity goals are frustrated by the doctrine.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, selective_institutions_seeking_liability_shield, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__colorblind_reading, selective_institutions_seeking_liability_shield, agenda_setter).

% Relies on race-conscious tools to build cohorts it judges pedagogically and institutionally necessary; must now redesign around proxies (geography, income, essays) that are costlier to administer and empirically weaker substitutes, while remaining exposed to litigation testing whether the proxies are themselves disguised classification.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, diversity_dependent_institutions, payer,
    organized, generational, constrained, national).

% Litigates and lobbies for anticlassification as the exclusive correct reading of the clause; achieves doctrinal victory and reputational/professional gains from having its interpretive framework adopted as controlling law, independent of the downstream effects on excluded communities.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, colorblind_legal_movement, beneficiary,
    organized, generational, mobile, national).

% Argue the clause was drafted and ratified to dismantle caste subordination, not to bar remedial classification, and that the colorblind reading inverts the amendment's purpose; their historical and textual evidence is treated by the controlling doctrine as, at most, a losing argument already litigated and settled against them.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% Studies the doctrinal history of the Fourteenth Amendment's Equal Protection Clause across readings, tracks how each reading maps to different empirical outcomes in enrollment and stratification, and documents the contest without holding decision-making power over which reading controls.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, constitutional_scholars_observer, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__colorblind_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_kernel__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable, formally neutral rule — no state actor may classify by race — that is simple to audit, resistant to disguised discrimination in either direction, and forecloses a category of litigation over whether a given racial classification's purpose was benign or invidious.
% TRANSFER_FUNCTION: Moves admission slots, contract awards, and other race-classified allocations away from applicants and entities selected partly on the basis of race-conscious remedial or diversity criteria, toward applicants and entities who rank higher under race-blind formal criteria; moves interpretive and litigation risk away from institutions and toward historically excluded communities who lose their remedial pathway.
% ABSENT_VOICES: Historically excluded communities and the civil rights organizations representing their interests argued extensively in the litigation record that the clause's drafting history targets caste subordination specifically, not classification as such; that argument was heard and rejected by the controlling reading, so their voice was present procedurally but structurally outvoted, not absent from the room — though the communities who will bear the multi-generational effects were not decision-makers in the doctrine's formation.
% DISAPPEARANCE_RATIONALE: If the colorblind reading were displaced overnight by the remedial or antisubordination reading, selective institutions could reintroduce explicit race-conscious criteria, admissions composition at elite institutions would shift measurably within one to two admissions cycles, and the litigation landscape around proxy criteria (geography, income-as-race-proxy) would collapse since the underlying doctrinal bar motivating the proxies would be gone.
% FOUNDING_PROBLEM: The colorblind reading was built to solve the problem of race-conscious state action being used, in its view, as a tool that could as easily entrench racial hierarchy (segregation, exclusion) as dismantle it — and to close what its proponents saw as a loophole where benign-labeled racial classifications could not be reliably distinguished from invidious ones, so a categorical bar was proposed as the only administrable safeguard against both.
% FOUNDING_PROBLEM_CORROBORATION: The colorblind legal movement and the controlling judicial majority attest the founding problem (classification as an inherently dangerous state tool) remains fully live and is solved by the categorical bar. Constitutional historians and the excluded civil rights organizations — outside the reading's own beneficiary set — attest the amendment's original founding problem was caste subordination specifically, that this problem is empirically not dead given persisting stratification, and that the colorblind reading substitutes a different founding problem (fear of state racial classification as such) for the one the amendment was ratified to solve.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 — substantial but not extreme — because the reading's operation transfers a real, contested allocation (admissions slots, contract set-asides) away from historically excluded groups toward incumbent-advantaged applicants, but does so through a facially neutral rule rather than overt targeting, which caps the measured extraction relative to an openly discriminatory regime. Suppression is authored at 0.62 because the doctrine's persistence depends on active enforcement — strict scrutiny functioning as near-fatal-in-fact — that forecloses institutional discretion to reintroduce race-conscious remedy even where the institution itself would prefer to. Theater ratio (0.4) reflects that a real coordination function (an administrable, litigation-resistant rule) coexists with a growing proxy-litigation apparatus (challenges to income/geography criteria as disguised race-consciousness) whose primary function is doctrinal enforcement rather than solving any new coordination problem. Accessibility collapse (0.68) and resistance (0.72) are both authored high for a non-mountain: once the doctrine is understood as controlling, remedial alternatives collapse almost completely for institutions bound by it, and resistance is intense precisely because the doctrine forecloses a previously live policy tool.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary majority and the colorblind legal movement experience this constraint as a coordination achievement — a stable, principled, administrable rule finally applied consistently after decades of doctrinal drift. Underrepresented minority applicants and historically excluded communities experience the same rule as an imposed extraction: a formally neutral mechanism that reallocates a scarce good (admission, contract award) away from them on grounds that do not depend on any finding about their present circumstances. The engine computes these as different seat-level types from the same structural data; the divergence is not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent-advantaged applicants and the colorblind legal movement are declared beneficiaries — they gain admission slots or doctrinal victory without needing to run the enforcement machinery themselves, so their derived directionality sits near the beneficiary end. Underrepresented minority applicants and historically excluded communities are declared victims with trapped/constrained exit at national scope — no relocation or private arrangement lets them route around a controlling constitutional doctrine, so their derived directionality sits near the full-target end. Selective institutions occupy a genuinely dual position (beneficiary of liability shield, payer of lost pedagogical discretion), captured via the secondary_role field rather than an override, since the derivation from declared beneficiary status already captures the liability-shield gain honestly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a live mismatch: the colorblind reading's own account of its founding problem (state racial classification is inherently dangerous regardless of purpose) is corroborated by its own beneficiaries but disputed by outside historians and the communities the amendment was originally ratified to protect, who locate the founding problem elsewhere (caste subordination) and judge it not dead. This mismatch — status contested, verdict world_rearranges — is exactly the signal the R5 genealogy interview exists to surface, and it is reported here as diagnostic data, not resolved by this story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_reading_committer_structure,
    'This constraint is one reading (colorblind_reading) of the contested equal_protection_kernel. What would the sibling readings (remedial_reading, antisubordination_reading) change structurally, and where exactly is the disagreement located?',
    'The disagreement is located at a single interpretive fork: whether the Fourteenth Amendment''s Equal Protection Clause is read as forbidding racial CLASSIFICATION as such (this reading) or forbidding racial SUBORDINATION/hierarchy-entrenchment while permitting classification aimed at dismantling it (antisubordination_reading), with remedial_reading occupying an intermediate position permitting narrowly tailored remedial classification without adopting the full antisubordination premise. Resolution would require either a doctrinal shift by the controlling judiciary or a constitutional amendment; it will not be resolved by additional historical evidence alone, since the drafting history is contested by all three camps using overlapping sources.',
    'Under remedial_reading or antisubordination_reading, the beneficiary/victim structure authored in this file inverts for the specific mechanism of race-conscious admissions: historically_excluded_communities would move from victim to beneficiary of the remedial pathway, and incumbent_advantaged_applicants would bear a real (though narrower) cost. This story does not average across that inversion — each reading is a separate file with its own ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_reading_committer_structure, conceptual, 'Committer-frame declaration: this file is one reading of a kernel with two live sibling readings; the fork is textual/purposive interpretation of ''equal protection,'' not a resolvable empirical question.').

omega_variable(
    false_summit_formal_neutrality,
    'Is the colorblind reading a genuine natural-law-like reading of constitutional text (formal equality as an intrinsic requirement of equal treatment), or a constructed doctrinal choice that happens to benefit incumbent-advantaged applicants and institutions seeking litigation shelter?',
    'Compare the doctrine''s stated purpose (preventing any state use of race, benign or invidious) against its actual distributive effects over the measured interval (declining minority enrollment at institutions bound by the doctrine, rising proxy-criteria litigation). If effects consistently favor already-advantaged groups across jurisdictions and time, the ''neutral principle'' framing is harder to sustain as purely principled.',
    'If constructed rather than naturally required by the text, the declared beneficiaries (incumbent_advantaged_applicants, colorblind_legal_movement) become evidence of an extractive function riding underneath a formal-equality justification, strengthening the tangled_rope classification already claimed; if genuinely required by the text''s plain meaning, the beneficiary pattern is incidental to a correct reading rather than constitutive of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_formal_neutrality, conceptual, 'Whether declaring beneficiaries on this claimed tangled_rope (not mountain) reading reflects genuine incidental effect or doctrinal capture dressed as principle.').

omega_variable(
    proxy_criteria_durability,
    'Will race-neutral proxy criteria (geography, income, essay content) that institutions adopt to preserve diversity outcomes under the colorblind reading themselves be struck down as disguised classification, or will they be permitted to stand?',
    'Track subsequent litigation outcomes challenging proxy criteria; a pattern of striking down proxies as pretextual would indicate the doctrine''s scope is still expanding, while a pattern of upholding facially neutral proxies would indicate a stable boundary has been reached.',
    'If proxies are struck down, effective extraction rises further and suppression intensifies (the doctrine reaches further into institutional discretion than currently measured); if proxies are upheld, current metrics may represent close to the doctrine''s ceiling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_criteria_durability, empirical, 'Open question about the doctrine''s future expansion into proxy-criteria litigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_kernel__colorblind_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__colorblind_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__colorblind_reading, theater_ratio, 2003, 0.28).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_kernel__colorblind_reading, theater_ratio, 2016, 0.32).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__colorblind_reading, theater_ratio, 2023, 0.38).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_kernel__colorblind_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__colorblind_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__colorblind_reading, base_extractiveness, 1978, 0.25).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__colorblind_reading, base_extractiveness, 2003, 0.32).
narrative_ontology:measurement(equa_be_t2016, equal_protection_kernel__colorblind_reading, base_extractiveness, 2016, 0.4).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__colorblind_reading, base_extractiveness, 2023, 0.55).
narrative_ontology:measurement(equa_be_t2024, equal_protection_kernel__colorblind_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__colorblind_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__colorblind_reading, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__colorblind_reading, suppression_requirement, 2003, 0.4).
narrative_ontology:measurement(equa_su_t2016, equal_protection_kernel__colorblind_reading, suppression_requirement, 2016, 0.48).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__colorblind_reading, suppression_requirement, 2023, 0.58).
narrative_ontology:measurement(equa_su_t2024, equal_protection_kernel__colorblind_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language concept 'the Equal Protection Clause's requirement regarding race.' Each reading (colorblind, remedial, antisubordination) is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle — measuring 'the clause' under different interpretive lenses yields structurally different claims, not one claim viewed three ways. All three files link to each other via affects_constraints and share the kernel_id equal_protection_kernel in their respective cs_structure blocks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
