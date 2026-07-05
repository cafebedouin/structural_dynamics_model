% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category — Intersex/DSD Accommodation Reading
 *   domain: political_philosophy/law/bioethics
 *
 * SUMMARY:
 *   This story instantiates the intersex-accommodation reading of the
 *   contested 'woman' category kernel: the claim that biological sex is
 *   better modeled as a non-binary spectrum, and that 'woman' properly
 *   includes both typical female biology and intersex/DSD variations that do
 *   not map onto a male category. This reading was developed to correct real
 *   injustice done by strict binary sex classification to intersex people.
 *   Its ε is low across most policy domains — legal sex markers, healthcare
 *   access, family law — where it functions as pure accommodation with
 *   negligible extraction. But in elite sport, where competitive-advantage
 *   stakes are high and testable, the same spectrum framework is
 *   operationalized by governing bodies (testosterone thresholds, eligibility
 *   panels) in a way that reproduces a NEW exclusionary boundary against the
 *   very population the reading was meant to protect — turning an
 *   inclusion-oriented reading into an instrument that extracts competitive
 *   opportunity from DSD/intersex women for the benefit of typical-biology
 *   competitors and the institutional legitimacy of 'fair sport' claims. This
 *   divergence between domains is why the tangled_rope classification is
 *   claimed here (real coordination function: consistent, humane sex
 *   categorization across most institutions) coexisting with genuine
 *   asymmetric extraction (in the narrow, high-stakes sport domain).
 *
 * KEY AGENTS:
 *   - sports_governing_bodies: agenda_setter (institutional/analytical) — administers thresholds and eligibility
 *   - intersex_and_dsd_athletes: primary target (powerless/trapped) — bears extraction specifically in sport despite being nominally included by the reading
 *   - binary_female_competitors: beneficiary (organized/constrained) — advantaged by exclusionary application
 *   - medical_and_intersex_advocacy_groups: excluded expert voice (moderate/constrained)
 *   - legislators_and_courts: analytical observer (institutional/analytical) — reviews but does not set the category
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.42).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.55).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category — Intersex/DSD Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '33cfa1fd-88b4-4d76-a7c2-e6394d8674fe').
narrative_ontology:cs_kernel_codification('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', distributed).
narrative_ontology:cs_authority_grounding('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', distributed).
narrative_ontology:cs_reading_relation('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', woman_category__sex_biology_reading, influences).
narrative_ontology:cs_reading_relation('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', foundational, sex_is_a_biological_spectrum_not_a_binary).
narrative_ontology:cs_axiom_status(sex_is_a_biological_spectrum_not_a_binary, holdable).
narrative_ontology:cs_axiom_grounding('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', sex_is_a_biological_spectrum_not_a_binary, empirically_contingent).
narrative_ontology:cs_axiom('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', foundational, woman_category_includes_atypical_female_biology).
narrative_ontology:cs_axiom_status(woman_category_includes_atypical_female_biology, holdable).
narrative_ontology:cs_axiom_grounding('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', woman_category_includes_atypical_female_biology, empirically_contingent).
narrative_ontology:cs_reference_frame('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', strict_binary_sex_classification).
narrative_ontology:cs_drift_state('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', post_semenya_caster_litigation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('33cfa1fd-88b4-4d76-a7c2-e6394d8674fe', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, binary_female_competitors).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_and_dsd_athletes).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, women_with_atypical_sex_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% World Athletics and similar bodies set testosterone thresholds and eligibility panels that operationalize the accommodation reading in the narrowest domain where it bites: elite competition. They administer testing, adjudicate borderline cases, and can revise the threshold, but have strong institutional incentive to preserve a clean binary category for competitive fairness optics rather than absorb the spectrum framing fully.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Athletes like Caster Semenya have female-typical gender identity, legal sex, and upbringing but XY chromosomes or elevated endogenous testosterone from a DSD. Under this reading they are women whose biology sits on the spectrum rather than the binary — but the same reading is used by governing bodies to carve them OUT of eligibility on performance-advantage grounds, converting an accommodation framework into a boundary-policing mechanism against them specifically. They cannot change their biology and often cannot compete at the elite level without medical suppression of their own hormones.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_and_dsd_athletes, payer,
    powerless, biographical, trapped, global).

% Athletes with typical XX female biology benefit from eligibility rules that exclude or medically restrict higher-testosterone competitors, preserving what they experience as a level competitive field. They did not design the intersex-accommodation framework but are structurally advantaged by how it gets applied in sport, even though the same framework is meant to include, not exclude, women with atypical biology.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, binary_female_competitors, beneficiary,
    organized, biographical, constrained, global).

% Endocrinologists, intersex advocacy organizations, and bioethicists who articulate the underlying biological complexity are consulted for expert testimony but do not sit on eligibility panels or set policy. They argue the spectrum reading, applied consistently, should expand inclusion rather than create a new exclusionary sub-boundary within it.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, medical_and_intersex_advocacy_groups, excluded,
    moderate, generational, constrained, global).

% National courts and sports arbitration bodies (CAS, Swiss Federal Tribunal) review challenges to eligibility rules built on this reading, weighing human rights claims against governing-body autonomy. Their rulings can force revision of how the spectrum reading is operationalized without adjudicating the underlying category question itself.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, legislators_and_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a category framework capable of including people whose biology does not fit a strict binary — avoiding the false-negative problem of excluding female-typical, female-identified, female-raised people purely because a chromosomal or hormonal marker deviates from the typical case.
% TRANSFER_FUNCTION: In most policy domains (legal sex markers, healthcare, family law) the reading transfers essentially nothing extractive — it just widens who is correctly counted as a woman. In elite sport specifically, it is inverted: eligibility panels use spectrum-biology reasoning to draw a NEW boundary that transfers competitive opportunity, prize money, and career eligibility away from intersex/DSD women toward binary-typical women.
% ABSENT_VOICES: Intersex and DSD athletes themselves are rarely voting members of the panels that set the testosterone thresholds applied to them; medical experts on intersex variation are consulted but do not hold decision authority. Their objection — that the reading which was supposed to include them is instead used to exclude them from the one domain where it has teeth — is documented in court filings and advocacy statements but does not control policy outcomes.
% DISAPPEARANCE_RATIONALE: In most legal and administrative contexts, if this reading vanished, little would change day to day — most people's sex categorization is untouched by spectrum edge cases. In elite sport, its disappearance (replaced by either pure chromosomal or pure self-identification readings) would immediately reopen the eligibility question for every current DSD athlete, materially changing who competes and who is excluded — sports governing bodies dispute which replacement reading should fill the gap, which is why the verdict is contested rather than settled.
% FOUNDING_PROBLEM: Legal, medical, and administrative systems built around a strict binary sex category produced clear injustices for people with intersex conditions and DSDs whose bodies did not fit either pole cleanly — mismatched IDs, denied medical care, and in sport, ad hoc and sometimes humiliating verification procedures (sex testing scandals of the 20th century).
% FOUNDING_PROBLEM_CORROBORATION: Medical and intersex advocacy organizations attest the founding problem (unjust binary exclusion) remains live and is in fact being reproduced by the specific way sports bodies operationalize the spectrum reading. Sports governing bodies attest a different, narrower founding problem — competitive fairness — is what their version of the reading solves, and treat that problem as still live and correctly addressed by current thresholds. No party outside the governing bodies themselves fully endorses the current threshold-based operationalization as a fair implementation of the accommodation principle.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, contested).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).
:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is authored as a blended, domain-weighted figure: near-zero in most legal/administrative contexts but substantial within elite sport, where it is the dominant lived experience of the reading for the affected population. Suppression (0.55) reflects active enforcement machinery (testosterone testing, medical intervention requirements) that did not exist in this form a generation ago and has hardened over the measured interval — hence the rising suppression_requirement series. Theater ratio (0.28) is moderate: some genuine biological review occurs, but an increasing share of eligibility-panel activity functions to defend a boundary-line optics of fairness rather than to serve the accommodation the reading was meant to provide.
 *
 * PERSPECTIVAL GAP:
 *   From the governing-body seat, the arrangement looks like principled, biology-literate accommodation refined into a workable competitive-fairness rule. From the DSD athlete's seat, the identical rule looks like the binary they were promised protection from, reconstituted one level down as a testosterone-threshold binary. The engine's per-seat computation should reflect this: agenda_setter and beneficiary seats likely compute rope-like or coordination-flavored; the payer seat should compute as tangled_rope or snare-adjacent given trapped exit and concentrated cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Sports governing bodies and binary-typical competitors sit near the beneficiary end: the former administers and derives legitimacy/revenue from clean competitive categories, the latter benefits competitively from exclusion of higher-testosterone competitors. Intersex and DSD athletes sit near the full-target end: trapped by biology, unable to exit the category dispute, and bearing direct career and bodily costs (forced hormone suppression) from how the reading is applied against them specifically — even though the reading's own logic should include rather than exclude them. This is the central irony the story is built to surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unjust exclusion of intersex people from binary sex categories) is only partly resolved: it is resolved in most legal and social contexts, but in elite sport a new, narrower version of the same exclusionary logic has been reconstructed under the accommodation reading's own vocabulary. This is not classic mandatrophy (mandate fully obsolete, arrangement persisting on inertia) — the founding problem is genuinely live in most domains — but it IS a localized mandatrophy-adjacent failure within sport specifically, where the accommodation apparatus has been captured to serve a different function than the one that justified it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_domain_capture,
    'Is the divergence between this reading''s low ε in general policy and high ε in elite sport evidence that the sport-domain application has been captured by a different (biology-strict, exclusionary) logic wearing the accommodation reading''s vocabulary, or is it a legitimate domain-specific refinement of the same principle under genuinely higher stakes?',
    'Compare eligibility-panel composition and threshold-setting rationale across sports federations against the panels/bodies that apply the reading in non-sport domains (passports, healthcare); divergent inclusion criteria under the same nominal reading would support the capture hypothesis.',
    'If captured, the sport-domain instantiation is arguably a distinct constraint entirely (a sex_biology_reading in disguise) rather than a high-stakes application of the accommodation reading, which would argue for decomposing this story further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_domain_capture, conceptual, 'Whether elite-sport application of this reading is genuine refinement or a captured re-instantiation of the sibling biology reading.').

omega_variable(
    spectrum_boundary_line_drawing,
    'Any operationalization of a ''spectrum'' reading in a binary-gated institution (competition eligibility, single-sex facilities) requires SOME cutoff. Is that cutoff-drawing itself extractive by necessity, or only extractive in its current (testosterone-threshold) form?',
    'Model alternative cutoff mechanisms (e.g., performance-based banding, open categories) and assess whether extraction from DSD athletes persists under any workable binary-gated implementation.',
    'If extraction is intrinsic to any binary implementation of a spectrum principle, the tangled_rope classification in sport is structurally durable rather than a fixable administrative defect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spectrum_boundary_line_drawing, conceptual, 'Whether cutoff-drawing under a spectrum principle is inherently extractive in binary-gated institutions.').

omega_variable(
    kernel_reading_dominance_by_domain,
    'Which of the three sibling readings (biology, identity, intersex-accommodation) is institutionally dominant in which domain, and is that distribution itself contested or settled?',
    'Survey legal codes, sports federation rules, and healthcare guidelines across jurisdictions for which reading each domain has adopted as its operative definition.',
    'Establishes whether this reading is a genuine plurality position or a minority accommodation carved into specific high-conflict domains like sport.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance_by_domain, empirical, 'Cross-domain mapping of which kernel reading actually governs in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t4, woman_category__intersex_accommodation_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(woma_tr_t8, woman_category__intersex_accommodation_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(woma_tr_t12, woman_category__intersex_accommodation_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(woma_tr_t16, woman_category__intersex_accommodation_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(woma_tr_t20, woman_category__intersex_accommodation_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(woma_tr_t24, woman_category__intersex_accommodation_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(woma_be_t4, woman_category__intersex_accommodation_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(woma_be_t8, woman_category__intersex_accommodation_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(woma_be_t12, woman_category__intersex_accommodation_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(woma_be_t16, woman_category__intersex_accommodation_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(woma_be_t20, woman_category__intersex_accommodation_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(woma_be_t24, woman_category__intersex_accommodation_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(woma_su_t4, woman_category__intersex_accommodation_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(woma_su_t8, woman_category__intersex_accommodation_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(woma_su_t12, woman_category__intersex_accommodation_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(woma_su_t16, woman_category__intersex_accommodation_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(woma_su_t20, woman_category__intersex_accommodation_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(woma_su_t24, woman_category__intersex_accommodation_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__intersex_accommodation_reading, 0.1).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the woman_category kernel. sex_biology_reading holds the strict binary chromosomal/anatomical definition; gender_identity_reading holds the self-identification definition; this story (intersex_accommodation_reading) holds a spectrum-biology definition that partially overlaps with and partially contests both siblings. This reading INFLUENCES the sex_biology_reading by undermining its binary premise with documented biological variation, without fully foreclosing it (biology-reading advocates can and do retreat to 'typical case' framings). It COEXISTS with the gender_identity_reading because both are non-strict-binary responses to the same founding problem, held by different, sometimes overlapping, advocacy coalitions, without either logically ruling out the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
