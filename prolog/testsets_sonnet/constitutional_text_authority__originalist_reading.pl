% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Text Authority
 *   domain: legal/political/institutional
 *
 * SUMMARY:
 *   Originalism emerged as a jurisprudential movement asserting that
 *   constitutional provisions carry the meaning they had at the moment of
 *   ratification, fixed and recoverable through historical evidence, and that
 *   judges exceed their legitimate authority when they depart from that
 *   meaning in response to contemporary values. The claimed type here is
 *   tangled_rope: there is a genuine coordination function (constraining
 *   judicial discretion, anchoring authority in a democratic act) but it
 *   operates alongside asymmetric extraction — the historical-evidence gate
 *   systematically disadvantages claimants whose interests were unrepresented
 *   in the ratifying public, and its administration has become entangled with
 *   an organized legal-professional movement whose institutional fortunes
 *   rise and fall with the doctrine's dominance.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: agenda_setter, sets and administers the evidentiary standard for what counts as authoritative historical meaning
 *   - federalist_legal_movement: beneficiary, organized professional network whose institutional position depends on originalism's doctrinal dominance
 *   - settled_property_and_contract_interests: beneficiary, favors interpretive stability regardless of historical determinacy
 *   - groups_seeking_unenumerated_rights_recognition: payer, faces structurally higher evidentiary bar for claims outside the historical record
 *   - historically_excluded_populations_absent_from_ratification_public: excluded, their absence from the ratifying public is treated as a fixed interpretive fact rather than a historical injustice to be corrected
 *   - constitutional_law_scholars: observer, disputes the determinacy of the historical reconstruction the doctrine depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.42).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.51).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Reading of Constitutional Text Authority").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "legal/political/institutional").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'e6f4def4-d5b9-4220-acba-76ba35090650').
narrative_ontology:cs_kernel_codification('e6f4def4-d5b9-4220-acba-76ba35090650', fixed_text).
narrative_ontology:cs_authority_grounding('e6f4def4-d5b9-4220-acba-76ba35090650', lineage).
narrative_ontology:cs_interpretation_layer_present('e6f4def4-d5b9-4220-acba-76ba35090650').
narrative_ontology:cs_reading_relation('e6f4def4-d5b9-4220-acba-76ba35090650', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('e6f4def4-d5b9-4220-acba-76ba35090650', constitutional_text_authority__positivist_reading, influences).
narrative_ontology:cs_axiom('e6f4def4-d5b9-4220-acba-76ba35090650', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('e6f4def4-d5b9-4220-acba-76ba35090650', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('e6f4def4-d5b9-4220-acba-76ba35090650', foundational, post_ratification_change_requires_article_v).
narrative_ontology:cs_axiom_status(post_ratification_change_requires_article_v, holdable).
narrative_ontology:cs_axiom_grounding('e6f4def4-d5b9-4220-acba-76ba35090650', post_ratification_change_requires_article_v, conventional).
narrative_ontology:cs_axiom('e6f4def4-d5b9-4220-acba-76ba35090650', secondary, historical_public_understanding_is_recoverable).
narrative_ontology:cs_axiom_status(historical_public_understanding_is_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('e6f4def4-d5b9-4220-acba-76ba35090650', historical_public_understanding_is_recoverable, empirically_contingent).
narrative_ontology:cs_reference_frame('e6f4def4-d5b9-4220-acba-76ba35090650', ratification_era_public_understanding).
narrative_ontology:cs_drift_state('e6f4def4-d5b9-4220-acba-76ba35090650', contemporary_originalist_revival, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e6f4def4-d5b9-4220-acba-76ba35090650', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, federalist_legal_movement).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, settled_property_and_contract_interests).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, groups_seeking_unenumerated_rights_recognition).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, historically_excluded_populations_absent_from_ratification_public).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, litigants_dependent_on_evolving_constitutional_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, rule_of_law_predictability_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, democratic_legitimacy_through_ratification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges and justices who adjudicate by reconstructing historical public meaning at ratification, using corpus linguistics, founding-era dictionaries, and historical practice as dispositive evidence. They administer which historical sources count as authoritative and set the evidentiary bar for what counts as 'sufficiently clear' original meaning. Their interpretive authority is enhanced relative to a living-constitutionalist framework because history, not contemporary moral reasoning, becomes the site of expertise they alone are positioned to adjudicate.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% A coordinated legal and academic network that built originalism into a credentialing pipeline: law schools, clerkships, judicial nominations, and funded historical scholarship. The methodology's dominance in appointments and opinion-writing directly advances the movement's institutional position and the substantive outcomes it favors.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, federalist_legal_movement, beneficiary,
    organized, generational, mobile, national).

% Commercial and property-holding actors who benefit from the interpretive stability originalism promises: contracts, titles, and regulatory expectations are less exposed to reinterpretation driven by shifting social consensus. They favor the predictability the reading claims to deliver, whether or not the historical reconstruction is actually determinate.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, settled_property_and_contract_interests, beneficiary,
    powerful, generational, mobile, national).

% Litigants and movements (reproductive rights, LGBTQ+ rights, privacy claimants) whose claims depend on recognizing rights not enumerated in the text and not clearly contemplated by the ratifying public. Under this reading, such claims face a structurally higher evidentiary bar; if no sufficiently specific historical analogue exists, the claim is foreclosed absent formal amendment. Their only formal exit is the Article V amendment process, which requires supermajorities they typically cannot assemble.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, groups_seeking_unenumerated_rights_recognition, payer,
    moderate, biographical, trapped, national).

% Groups who were not part of the enfranchised 'public' whose understanding is treated as authoritative at the relevant ratification moments (women, enslaved and formerly enslaved people, non-property-holders). Their historical exclusion from the meaning-making public is now treated as a fixed interpretive baseline rather than a defect to be corrected; they were not in the room when the meaning being enforced was fixed, and the reading gives them no doctrinal mechanism to say so except after-the-fact amendment.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, historically_excluded_populations_absent_from_ratification_public, excluded,
    powerless, civilizational, trapped, national).

% Parties whose cases were previously supported by doctrine built under living-constitutionalist or precedent-accretion reasoning now face doctrinal instability as courts revisit precedent for originalist fidelity. They must relitigate settled expectations against a historical-evidence standard that did not govern when their reliance interests formed.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, litigants_dependent_on_evolving_constitutional_doctrine, payer,
    moderate, biographical, constrained, national).

% Academic historians and legal scholars who study the historical record independent of litigation outcomes and frequently dispute whether the 'original public meaning' asserted in specific cases is historically well-supported or a reconstruction shaped by present-day preferences.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, text-anchored decision procedure that constrains judicial discretion, gives litigants and legislatures a stable interpretive target, and grounds constitutional authority in a democratically ratified act rather than in judges' contemporary moral judgment.
% TRANSFER_FUNCTION: Moves interpretive authority from evolving democratic and judicial consensus-building toward historians and judges credentialed in historical reconstruction; moves substantive constitutional protection away from claimants whose interests were not represented in the ratifying public and toward interests already reflected in the historical record.
% ABSENT_VOICES: Groups excluded from the franchise or public discourse at the relevant ratification moments (women, enslaved people, other disenfranchised populations) cannot testify to what 'the public' understood, because they were not counted as part of that public; their absence from the historical record is treated as silence rather than exclusion.
% DISAPPEARANCE_RATIONALE: If originalism ceased to be a live interpretive commitment, judicial appointments, doctrinal tests across multiple areas of constitutional law (equal protection, unenumerated rights, federalism, criminal procedure), and decades of legal-movement infrastructure built around historical-evidence litigation would all require reorganization around a different interpretive currency.
% FOUNDING_PROBLEM: Judicial discretion under mid-to-late-20th-century living constitutionalism was perceived by critics as untethered from any fixed text, allowing judges to constitutionalize their own policy preferences under the guise of interpretation; originalism was built to re-anchor interpretive authority in a determinate, democratically-ratified source.
% FOUNDING_PROBLEM_CORROBORATION: Originalist judges and the Federalist Society attest the discretion problem remains live and that historical anchoring is the correct remedy. Independent legal historians outside the originalist movement (including some who are otherwise sympathetic to textualism) attest that the 'historical public meaning' the doctrine claims to recover is frequently underdetermined or contested among historians themselves, and that the doctrine's determinacy claim is not corroborated by the state of the historical record it depends on.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).
:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than extreme: the doctrine genuinely constrains judicial discretion in ways that benefit predictability broadly, but a growing share of that constraint's practical effect falls on claimants seeking new rights recognition, which is why extraction has risen over the measured interval as originalism moved from academic minority position to controlling doctrine in multiple courts. Suppression (0.51) reflects the doctrine's structural foreclosure of certain claim types absent Article V amendment, a genuinely high bar; it is not total because litigants retain the amendment path in principle and dissenting scholarship remains possible. Theater ratio (0.28) is moderate-low: much originalist historical analysis is genuine archival work, but a meaningful share of 'historical public meaning' findings are contested by professional historians as reconstructions shaped by the outcome sought, which is the performative component. Accessibility collapse (0.62) is fairly high because once a court adopts strict originalism, alternative interpretive methodologies become largely unavailable within that court's own doctrine. Resistance (0.58) reflects substantial ongoing pushback from legal scholars, dissenting judges, and rights-claimant litigants.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting judiciary's seat, originalism is coordination: a neutral, democratically-grounded procedure that removes judges' personal preferences from constitutional adjudication. From the seat of a claimant seeking recognition of an unenumerated right, the identical procedure operates as an enforced historical veto — the same textual fixity that promises neutrality also forecloses categories of relief that a living-constitutionalist framework would keep open. The engine's per-seat computation should register this asymmetry without either seat's report being treated as the authoritative one.
 *
 * DIRECTIONALITY LOGIC:
 *   The originalist judiciary and the federalist legal movement sit near the beneficiary end: they set and administer the interpretive rule and their institutional standing is enhanced by its dominance. Settled property and contract interests benefit incidentally through preserved predictability. Groups seeking unenumerated rights recognition and litigants dependent on prior evolving doctrine sit near the target end: the same rule that grants predictability to some forecloses relief to others, and their formal exit (Article V amendment) is structurally almost unreachable for a minority claim. Historically excluded populations are the sharpest case: they are not merely disadvantaged targets but structurally excluded from the very historical record the doctrine treats as authoritative, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — perceived judicial policy-making disguised as interpretation — has a contested status: it may still be live in cases where judges do appear to reach for contemporary values, but originalists themselves have not agreed among each other or with independent historians on what the historical record actually establishes in many contested cases, suggesting the solution has itself become a site of the same indeterminacy it was built to foreclose. This does not resolve mandatrophy either way; it is flagged as contested rather than declared dead or live, consistent with the corroboration record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_meaning_determinacy,
    'Is ''original public meaning'' at ratification generally a determinate, recoverable historical fact, or is it frequently underdetermined such that originalist judges are selecting among plausible historical narratives rather than discovering a fixed answer?',
    'Systematic comparison of professional historians'' independent reconstructions of ratification-era public understanding against judicial originalist findings in a sample of contested cases, checking for convergence or divergence.',
    'If frequently underdetermined, the coordination claim (a neutral, discretion-constraining procedure) is substantially undermined, and the doctrine functions closer to a discretion-laundering mechanism dressed as historical discovery — pushing the classification toward snare. If generally determinate, the tangled_rope classification''s coordination component is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_determinacy, empirical, 'Whether original public meaning is a recoverable historical fact or a contested reconstruction.').

omega_variable(
    excluded_public_baseline_legitimacy,
    'Does grounding constitutional authority in the historical understanding of a ratifying public that structurally excluded large populations (by sex, race, and property status) transmit that exclusion into present-day interpretive authority, or is the exclusion adequately cured by subsequent formal amendments (13th, 14th, 15th, 19th)?',
    'Doctrinal analysis of whether courts applying originalism to amended provisions use the amending generation''s public meaning (curing the exclusion) or import pre-amendment original-public-meaning assumptions into adjacent unamended text (perpetuating it).',
    'If courts default to pre-amendment original meaning outside the amended clauses themselves, the exclusion is not cured and the victim declaration for historically_excluded_populations is directly supported; if courts consistently use the amending generation''s understanding wherever relevant, the exclusion concern is substantially narrower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_public_baseline_legitimacy, conceptual, 'Whether formal amendment cures the exclusion baked into the original ratifying public''s composition.').

omega_variable(
    kernel_reading_selection_basis,
    'What determines which reading of the constitutional_text_authority kernel a given judge, scholar, or institution adopts — is it a genuinely prior methodological commitment, or is it substantially predicted by anticipated substantive outcomes?',
    'Empirical study correlating judges'' declared interpretive methodology with their voting patterns on outcome-salient cases, controlling for case type, to see whether methodology predicts outcome independent of political appointment history.',
    'If reading selection is substantially outcome-driven, all three kernel readings (originalist, living-constitutionalist, positivist) function partly as post-hoc justificatory frames rather than independent methodological commitments, which would reframe the entire kernel contest rather than any single reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether interpretive methodology selection is prior to or downstream of desired substantive outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1980, constitutional_text_authority__originalist_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(cons_tr_t1990, constitutional_text_authority__originalist_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text_authority__originalist_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(cons_tr_t2010, constitutional_text_authority__originalist_reading, theater_ratio, 2010, 0.21).
narrative_ontology:measurement(cons_tr_t2020, constitutional_text_authority__originalist_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(cons_tr_t2025, constitutional_text_authority__originalist_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t1980, constitutional_text_authority__originalist_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(cons_be_t1990, constitutional_text_authority__originalist_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(cons_be_t2000, constitutional_text_authority__originalist_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement(cons_be_t2010, constitutional_text_authority__originalist_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(cons_be_t2020, constitutional_text_authority__originalist_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(cons_be_t2025, constitutional_text_authority__originalist_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1980, constitutional_text_authority__originalist_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(cons_su_t1990, constitutional_text_authority__originalist_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(cons_su_t2000, constitutional_text_authority__originalist_reading, suppression_requirement, 2000, 0.39).
narrative_ontology:measurement(cons_su_t2010, constitutional_text_authority__originalist_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement(cons_su_t2020, constitutional_text_authority__originalist_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement(cons_su_t2025, constitutional_text_authority__originalist_reading, suppression_requirement, 2025, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__originalist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the constitutional_text_authority kernel. Each reading is authored as an independent constraint with its own epsilon, beneficiary/victim structure, and classification, per the ε-invariance principle: the natural-language label 'constitutional interpretation' covers structurally distinct claims about where interpretive authority is grounded (fixed historical meaning vs. evolving contemporary principle vs. formal enactment procedure), and forcing them into one story would average over genuinely different structures. The originalist and living-constitutionalist readings are direct competitors for the same interpretive terrain (hence linked); the positivist reading operates on a partially orthogonal axis (formal validity vs. moral content) but is linked because judges and scholars frequently invoke positivist premises in service of either substantive reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
