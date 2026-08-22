% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__marketplace_reading, []).

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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Free Speech Doctrine — Marketplace-of-Ideas Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the marketplace-of-ideas reading of the speech
 *   protection kernel: the doctrinal claim that speech protection is
 *   justified because unregulated discourse is a truth-discovery process,
 *   with counterspeech as the remedy for false or harmful claims rather than
 *   content-based restriction. This is a distinct constraint from the
 *   absolutist reading (which grounds protection in autonomy rather than
 *   epistemic function and would tolerate protection even where
 *   truth-discovery demonstrably fails), the harm-threshold reading (which
 *   conditions protection on absence of demonstrable victim harm — the
 *   opposite gate), the dignity reading (which asks whether speech functions
 *   as subordination, a question the marketplace frame does not ask), and the
 *   democratic-participation reading (which grounds strength of protection in
 *   self-governance function specifically, not epistemic contest generally).
 *   Each reading would author a different epsilon for the same doctrinal
 *   label 'free speech protection' because each identifies a different
 *   mechanism as doing the justificatory work; this file addresses only the
 *   marketplace mechanism.
 *
 * KEY AGENTS:
 *   - established_media_institutions: institutional beneficiary of scale-neutral doctrine
 *   - well_resourced_speakers: powerful beneficiary who can saturate the marketplace
 *   - targets_of_disinformation_campaigns: moderate-power payer whose reputational harm outpaces correction
 *   - marginalized_group_members_subject_to_hostile_speech: powerless payer for whom counterspeech does not address exclusionary function
 *   - courts_as_doctrine_administrators: institutional observer applying the doctrine case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.42).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.28).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Free Speech Doctrine — Marketplace-of-Ideas Reading").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, 'e346e519-f173-4642-ba0b-7fe6b6477ea6').
narrative_ontology:cs_kernel_codification('e346e519-f173-4642-ba0b-7fe6b6477ea6', fixed_text).
narrative_ontology:cs_authority_grounding('e346e519-f173-4642-ba0b-7fe6b6477ea6', lineage).
narrative_ontology:cs_interpretation_layer_present('e346e519-f173-4642-ba0b-7fe6b6477ea6').
narrative_ontology:cs_reading_relation('e346e519-f173-4642-ba0b-7fe6b6477ea6', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e346e519-f173-4642-ba0b-7fe6b6477ea6', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('e346e519-f173-4642-ba0b-7fe6b6477ea6', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('e346e519-f173-4642-ba0b-7fe6b6477ea6', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('e346e519-f173-4642-ba0b-7fe6b6477ea6', foundational, discourse_is_self_correcting_over_time).
narrative_ontology:cs_axiom_status(discourse_is_self_correcting_over_time, holdable).
narrative_ontology:cs_axiom_grounding('e346e519-f173-4642-ba0b-7fe6b6477ea6', discourse_is_self_correcting_over_time, empirically_contingent).
narrative_ontology:cs_axiom('e346e519-f173-4642-ba0b-7fe6b6477ea6', foundational, counterspeech_is_sufficient_remedy_for_false_or_harmful_speech).
narrative_ontology:cs_axiom_status(counterspeech_is_sufficient_remedy_for_false_or_harmful_speech, holdable).
narrative_ontology:cs_axiom_grounding('e346e519-f173-4642-ba0b-7fe6b6477ea6', counterspeech_is_sufficient_remedy_for_false_or_harmful_speech, empirically_contingent).
narrative_ontology:cs_reference_frame('e346e519-f173-4642-ba0b-7fe6b6477ea6', early_twentieth_century_marketplace_rationale).
narrative_ontology:cs_drift_state('e346e519-f173-4642-ba0b-7fe6b6477ea6', post_algorithmic_amplification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e346e519-f173-4642-ba0b-7fe6b6477ea6', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, established_media_institutions).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, well_resourced_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, academic_and_scientific_institutions).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, incumbent_political_actors).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_disinformation_campaigns).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, marginalized_group_members_subject_to_hostile_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, low_resource_speakers_drowned_out).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, epistemically_vulnerable_audiences).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, truth_emerges_from_unregulated_discourse).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, counterspeech_is_an_adequate_remedy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate large-audience platforms and benefit from a legal doctrine that immunizes broad publication against most content-based liability, provided they compete in the same undifferentiated marketplace as anyone else. Their scale means they set the effective terms of what circulates as 'winning' speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, established_media_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Have the money, platforms, and legal teams to flood the marketplace with volume and repetition. The doctrine treats their speech and a lone objector's speech as formally equal inputs, but only they can actually saturate the discourse space to shape which claims survive contact with 'more speech.'
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, well_resourced_speakers, beneficiary,
    powerful, biographical, mobile, national).

% Rely on strong speech protection to pursue contested inquiry without state interference, and benefit from the doctrine's premise that bad ideas lose out over time through open contest rather than suppression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, academic_and_scientific_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Courts and legislatures that adopted and enforce the marketplace rationale are themselves incumbents of a system whose legitimacy the doctrine helps secure; they administer the doctrine and benefit from the presumption that the existing communicative order is self-correcting rather than in need of intervention.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, incumbent_political_actors, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__marketplace_reading, incumbent_political_actors, agenda_setter).

% Individuals or small groups falsely accused, defamed, or targeted by coordinated disinformation absorb reputational and material harm faster than any 'more speech' correction can propagate; the marketplace remedy assumes a correction speed and audience reach that does not exist for people without platforms.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targets_of_disinformation_campaigns, payer,
    moderate, biographical, constrained, national).

% Bear the ongoing psychological and social cost of speech that operates as intimidation or exclusion rather than a contestable factual claim; under the marketplace frame this speech is treated as an input to be countered by argument, which does not address speech functioning as a threat or exclusion signal rather than a truth claim.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, marginalized_group_members_subject_to_hostile_speech, payer,
    powerless, biographical, trapped, national).

% Have true or important claims to make but lack the reach to compete with saturation campaigns; the formal equality of the marketplace model does not correct for radically unequal capacity to be heard, so their 'more speech' is functionally inaudible.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, low_resource_speakers_drowned_out, payer,
    powerless, biographical, constrained, national).

% Audiences without the time, training, or cross-checking resources to evaluate competing claims absorb whichever version reaches them first or most often; the marketplace model assumes a rational, resourced listener that many actual listeners are not.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, epistemically_vulnerable_audiences, payer,
    powerless, biographical, trapped, national).

% Argue that some speech functions as conduct (threat, subordination, coordinated harassment) rather than as a truth-claim subject to correction, and that the marketplace frame structurally cannot see this category. Their position is litigated and academically prominent but has not displaced the doctrine's operative core in most jurisdictions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, harm_threshold_and_dignity_advocates, excluded,
    organized, biographical, constrained, national).

% Apply and refine the marketplace rationale case by case, citing its truth-discovery justification even where empirical evidence about actual discourse dynamics (virality, asymmetric reach, bot amplification) increasingly diverges from the doctrine's founding assumptions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, courts_as_doctrine_administrators, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, content-neutral rule for adjudicating speech disputes: rather than empowering officials to judge truth or value case-by-case, it lets contested claims circulate and compete, reducing the risk of state-enforced orthodoxy and enabling genuine inquiry to proceed without prior restraint.
% TRANSFER_FUNCTION: Moves the burden of correcting false or harmful speech from ex ante restriction onto the targets and audiences of that speech, who must generate, fund, and successfully circulate counterspeech fast enough and far enough to neutralize the original harm — a burden that falls unevenly by resource and reach.
% ABSENT_VOICES: Advocates of the harm-threshold and dignity readings are present in legal and academic discourse but structurally excluded from controlling doctrine in most marketplace-governed jurisdictions; their empirical critiques of unequal discourse capacity are cited in dissents and scholarship but rarely displace the operative rule.
% DISAPPEARANCE_RATIONALE: If the marketplace rationale were withdrawn as the operative justification, courts would need a different content-based framework (harm threshold, dignity-based, or democratic-participation) to decide the same disputes, likely permitting more content-based restriction; well-resourced speakers and institutions would lose the doctrinal shield that currently treats volume and reach as neutral inputs.
% FOUNDING_PROBLEM: Twentieth-century courts sought a rule that would prevent government from suppressing dissent, minority viewpoints, and unpopular science under the guise of protecting 'truth' or public order, especially after historical episodes of state-enforced orthodoxy and censorship of political dissidents.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and constitutional scholars outside the beneficiary set (independent of incumbent political and media institutions) attest the anti-censorship problem remains partly live, but empirical communication researchers studying disinformation dynamics and platform virality — also outside the beneficiary set — attest that the doctrine's founding assumption of roughly symmetric discourse capacity no longer holds and that the remedy has drifted from its original target.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).
:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising over the measured interval: the doctrine's coordination function (preventing state-enforced orthodoxy) remains real, but the empirical premise underwriting it — that discourse capacity is roughly symmetric so 'more speech' is an adequate remedy — has become progressively less true as media consolidation, algorithmic amplification, and disinformation-at-scale widened the gap between formal equality of speech rights and actual equality of reach. Suppression is comparatively low and roughly flat (0.28) because the doctrine's primary mechanism is permissive (it forecloses restriction) rather than coercive in the ordinary sense; what suppression exists is directed at attempts to regulate content, not at speakers. Theater ratio rises meaningfully (0.15 to 0.40) because judicial and public invocation of 'the marketplace corrects itself' increasingly functions as justificatory performance for outcomes (viral disinformation, coordinated harassment campaigns going unaddressed) that the empirical record shows the marketplace mechanism does not actually correct within a timeframe that matters to the harmed party.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting and beneficiary seats (courts, institutions, well-resourced speakers), the arrangement reads as principled content-neutrality solving a real historical problem of censorship. From the payer seats (targets of disinformation, marginalized groups, low-resource speakers), the same doctrine reads as a formal equality that fails to produce substantive correction, leaving them to absorb harm the 'more speech' remedy does not timely address. The engine should compute these as structurally different experiences of the identical rule, not as disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those with the resources, reach, and institutional standing to compete effectively in the marketplace: established media, well-resourced individual speakers, academic/scientific institutions whose long time horizons let slow correction work, and incumbent political actors whose authority the doctrine helps stabilize. Victims are those the doctrine formally protects but whose ability to use the marketplace mechanism for actual redress is structurally weak: individual targets of disinformation, marginalized groups subject to speech that functions as exclusion rather than a claim to be argued with, low-resource speakers who cannot achieve comparable reach, and audiences without the resources to evaluate competing claims. The directionality gap is not about who the doctrine names as protected (nearly everyone) but about who can actually convert that protection into the promised correction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing government suppression of dissent and unpopular ideas — remains partly live, which is why this is authored as contested rather than dead. But the specific mechanism this reading relies on (truth-discovery through open contest, correction via counterspeech) has drifted from a plausible empirical premise in a print/broadcast era of comparatively bounded reach to a much weaker one in an era of algorithmic virality and coordinated disinformation at scale. Classifying this as tangled_rope rather than snare preserves the real coordination function (anti-censorship) while registering that the doctrine now also serves as cover for asymmetric extraction it was not originally built to shield.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marketplace_empirical_premise_validity,
    'Does open discourse actually converge on truth over a timeframe relevant to the harmed party, given contemporary media structure (algorithmic amplification, disinformation coordination, unequal reach), or was this premise only ever locally true under mid-20th-century print/broadcast conditions?',
    'Empirical communication science on correction dynamics: measured half-life of false claims versus corrective counterspeech reach, compared across media eras and platform architectures.',
    'If the premise no longer holds at scale, the marketplace reading''s coordination justification for content-neutrality weakens substantially relative to the harm-threshold or dignity readings, which do not depend on the correction mechanism actually working.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_empirical_premise_validity, empirical, 'Whether the truth-discovery/counterspeech mechanism this reading relies on still functions under contemporary discourse conditions.').

omega_variable(
    reading_boundary_speech_as_conduct,
    'Where does the marketplace reading''s category of ''speech to be countered by more speech'' stop being applicable — i.e., at what point does speech function as conduct (threat, harassment coordination, structural subordination) rather than as a truth-claim the mechanism is designed to process?',
    'Doctrinal and philosophical analysis of the speech/conduct distinction, informed by empirical study of psychological and material harm from targeted hostile speech versus contestable propositional claims.',
    'If a substantial share of the speech this reading protects is better characterized as conduct, the marketplace reading''s domain of applicability is narrower than currently administered, and doctrine drifts toward the dignity or harm-threshold readings for that subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_speech_as_conduct, conceptual, 'Whether the marketplace frame''s implicit category boundary (truth-claim vs. conduct) is doing legitimate or illegitimate work in current doctrine.').

omega_variable(
    founding_problem_scope_narrowing,
    'Was the founding anti-censorship problem this doctrine solves specifically about state suppression of political and scientific dissent, or was it always intended to cover the full space of private disinformation and interpersonal hostile speech it now shields?',
    'Historical/originalist analysis of the founding cases and their factual contexts, compared against the scope of speech currently protected under the marketplace rationale.',
    'If the founding problem was narrower than current application, the doctrine has scope-crept from a targeted anti-censorship rule into a general shield for privately-generated harm, which would support reclassifying a subset of currently-protected speech under a different reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_scope_narrowing, conceptual, 'Whether the doctrine''s current scope matches or has drifted from its founding factual context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1919, speech_protection_kernel__marketplace_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement_basis(spee_tr_t1919, observed).
narrative_ontology:measurement(spee_tr_t1960, speech_protection_kernel__marketplace_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement_basis(spee_tr_t1960, observed).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_kernel__marketplace_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement_basis(spee_tr_t1990, observed).
narrative_ontology:measurement(spee_tr_t2005, speech_protection_kernel__marketplace_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement_basis(spee_tr_t2005, observed).
narrative_ontology:measurement(spee_tr_t2016, speech_protection_kernel__marketplace_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement_basis(spee_tr_t2016, observed).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__marketplace_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(spee_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1919, speech_protection_kernel__marketplace_reading, base_extractiveness, 1919, 0.2).
narrative_ontology:measurement_basis(spee_be_t1919, observed).
narrative_ontology:measurement(spee_be_t1960, speech_protection_kernel__marketplace_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement_basis(spee_be_t1960, observed).
narrative_ontology:measurement(spee_be_t1990, speech_protection_kernel__marketplace_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement_basis(spee_be_t1990, observed).
narrative_ontology:measurement(spee_be_t2005, speech_protection_kernel__marketplace_reading, base_extractiveness, 2005, 0.34).
narrative_ontology:measurement_basis(spee_be_t2005, observed).
narrative_ontology:measurement(spee_be_t2016, speech_protection_kernel__marketplace_reading, base_extractiveness, 2016, 0.4).
narrative_ontology:measurement_basis(spee_be_t2016, observed).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__marketplace_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(spee_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1919, speech_protection_kernel__marketplace_reading, suppression_requirement, 1919, 0.25).
narrative_ontology:measurement_basis(spee_su_t1919, observed).
narrative_ontology:measurement(spee_su_t1960, speech_protection_kernel__marketplace_reading, suppression_requirement, 1960, 0.22).
narrative_ontology:measurement_basis(spee_su_t1960, observed).
narrative_ontology:measurement(spee_su_t1990, speech_protection_kernel__marketplace_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement_basis(spee_su_t1990, observed).
narrative_ontology:measurement(spee_su_t2005, speech_protection_kernel__marketplace_reading, suppression_requirement, 2005, 0.24).
narrative_ontology:measurement_basis(spee_su_t2005, observed).
narrative_ontology:measurement(spee_su_t2016, speech_protection_kernel__marketplace_reading, suppression_requirement, 2016, 0.27).
narrative_ontology:measurement_basis(spee_su_t2016, observed).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__marketplace_reading, suppression_requirement, 2024, 0.28).
narrative_ontology:measurement_basis(spee_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__marketplace_reading, 0.1).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraints decomposed from the natural-language label 'free speech protection,' each instantiating a distinct kernel reading with its own epsilon, beneficiary/victim structure, and classification, per the epsilon-invariance principle. The marketplace reading shares the kernel's fixed text (constitutional speech clauses / doctrinal free-speech jurisprudence) with all siblings but grounds protection in a different mechanism (truth-discovery via open contest) than autonomy (absolutist), harm-avoidance (harm_threshold), anti-subordination (dignity), or self-governance (democratic_participation). All five must be read together to reconstruct the full contested kernel; none alone represents 'free speech' as a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
