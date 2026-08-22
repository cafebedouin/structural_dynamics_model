% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Displacement as Irreversible Moral Substrate Shift
 *   domain: historical_sociology/cultural_anthropology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the dueling-disappearance kernel:
 *   the contraction reading, which holds that dueling became culturally
 *   unthinkable because dignity-culture axioms (intrinsic, equal, inalienable
 *   worth) displaced honor-culture axioms (worth as a contingent, defensible
 *   possession redeemable through combat) at the substrate level. On this
 *   reading dueling did not lose a competition to better institutions (that
 *   is the institutional_displacement_reading, a different constraint) and
 *   its decline was not merely one of several independently sufficient causes
 *   acting together (that is the overdetermined_composite_reading, also a
 *   different constraint). Here the mechanism is a total axiom replacement:
 *   honor-culture's vocabulary became not illegal-and-avoided but
 *   incoherent-and-unspeakable. Because the claim is that the successor
 *   framework is now an irreducible feature of the present moral landscape
 *   rather than a contestable policy choice, this reading's claimed_type is
 *   mountain, not rope — the coordination-on-honor-norms framing belongs to a
 *   prior epoch this reading treats as substrate-superseded.
 *
 * KEY AGENTS:
 *   - dignity_culture_adherents: structural beneficiary, institutional/analytical exit — inherits the entire social field
 *   - honor_culture_practitioners: primary victim, powerless/trapped — framework becomes illegible, not merely prohibited
 *   - clergy_and_moral_reformers: agenda-setters who propagated the displacing axioms
 *   - historians_of_honor_culture: analytical observers reconstructing the pre-displacement framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.25).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Displacement as Irreversible Moral Substrate Shift").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd').
narrative_ontology:cs_kernel_codification('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', implicit).
narrative_ontology:cs_authority_grounding('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', diffuse_epistemic).
narrative_ontology:cs_reading_relation('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', foundational, worth_is_intrinsic_and_inalienable).
narrative_ontology:cs_axiom_status(worth_is_intrinsic_and_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', worth_is_intrinsic_and_inalienable, deontological).
narrative_ontology:cs_axiom('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', foundational, axiom_substrate_replacement_is_sufficient_without_institutional_substitution).
narrative_ontology:cs_axiom_status(axiom_substrate_replacement_is_sufficient_without_institutional_substitution, holdable).
narrative_ontology:cs_axiom_grounding('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', axiom_substrate_replacement_is_sufficient_without_institutional_substitution, empirically_contingent).
narrative_ontology:cs_reference_frame('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', honor_culture_contingent_worth_framework).
narrative_ontology:cs_drift_state('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', post_civil_war_dignity_consolidation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('6e3bbc5d-a3db-4fd0-98ee-f3aeb428d8cd', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, universal_equal_dignity_doctrine).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, state_monopoly_on_legitimate_violence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold that personal worth is intrinsic and inalienable, not contingent on public vindication through combat. Under this axiom, submitting an insult to third-party adjudication (courts, mediation, simply ignoring it) carries no loss of standing. They do not administer dueling's disappearance so much as occupy the moral substrate that makes dueling's premises stop registering as coherent — they benefit by inheriting the entire social field once honor-vindication exits it.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents, beneficiary,
    institutional, civilizational, analytical, national).

% Held that a gentleman's public reputation was a possession that could be stolen by insult and restored only by a formal, witnessed risk of death. As the surrounding culture's axioms shift, their entire vocabulary of standing, courage, and vindication becomes not merely illegal but unintelligible to those around them. They cannot translate their loss into the new idiom — there is no dignity-culture equivalent of 'satisfaction' — so the practice does not survive being explained, only being punished or mocked.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerless, generational, trapped, regional).

% Preached and published against dueling as a violation of the sanctity of life and a usurpation of divine/state judgment. They actively propagated the dignity-culture axioms (equal worth before God, submission to lawful authority) that made honor-vindication look like vanity and murder rather than virtue. Their exit is unconstrained — they are not caught in the framework shift, they are producing it.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, clergy_and_moral_reformers, agenda_setter,
    organized, generational, mobile, national).

% Study the axiom shift from outside both frameworks, reconstructing what honor-culture participants believed was at stake and how the vocabulary became untranslatable. They can render honor-culture legible retrospectively without being able to restore its lived force.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historians_of_honor_culture, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, on this reading, at the point of disappearance: dueling's original coordination function (deterring insult through costly signaling, substituting for absent formal courts) had already been real, but by the disappearance period the operative force is not a coordination failure — it is that the substrate of shared axioms in which the coordination made sense has been displaced. Dignity culture does not out-coordinate honor culture; it makes honor culture's terms stop parsing.
% TRANSFER_FUNCTION: Moves social and moral standing away from those who could win or credibly threaten single combat and toward everyone equally, since dignity culture assigns worth independent of demonstrated physical courage. Honor-culture practitioners lose the only currency (willingness to risk death for reputation) in which they held disproportionate advantage; nothing replaces it for them specifically.
% ABSENT_VOICES: Honor-culture practitioners themselves have no forum in the successor culture to argue for their framework's validity — dignity culture's terms make the honor position sound like a category error (glorifying violence over life) rather than a competing value system worth adjudicating. Their objection cannot even be voiced in vocabulary the successor culture will hear as an objection.
% DISAPPEARANCE_RATIONALE: On this reading the axiom substrate is treated as an accomplished, load-bearing fact of the present social world, not a live, revisable arrangement — asking 'what if dignity culture disappeared overnight' is asking what if the ground gave way, not what if a policy were repealed. Nothing is currently organized around dueling's absence in a way that would visibly rearrange if the norm were lifted; the honor-culture alternative is not merely disfavored but unavailable as a live option, which is the mountain signature this reading claims.
% FOUNDING_PROBLEM: Honor culture's own founding problem — reputational insult with no trusted third-party adjudicator — is not this reading's subject. This reading's founding problem is different: how a society metabolizes a moral-axiom replacement so completely that the prior framework's practitioners cannot be understood on their own terms by their descendants.
% FOUNDING_PROBLEM_CORROBORATION: Social historians studying 19th-century sermon literature, newspaper editorials, and court records attest independently that the vocabulary shift preceded and outlasted specific legal prohibitions — corroboration comes from outside dignity-culture's own self-congratulatory narrative, via textual analysis of how 'honor' language itself changed meaning across the century.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.15, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.15 at interval end) because on this reading nothing is actively extracted by dignity culture from honor-culture practitioners in an ongoing transactional sense — the harm is a one-time (though generationally extended) substrate replacement, not a recurring rent. Suppression (0.25) is likewise moderate rather than severe: legal prohibition existed, but the dominant mechanism this reading claims is axiom illegibility, not coercion. Accessibility collapse is authored very high (0.88) because the honor-culture alternative becomes not merely disfavored but unavailable as a coherent lived option once the surrounding vocabulary shifts. Resistance is authored low (0.12) because by the time the axiom shift is complete, honor-culture practitioners have no framework left in which to mount resistance that the successor culture would recognize as resistance rather than atavism.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity_culture_adherents sit at the beneficiary end: institutional power, generational-to-civilizational horizon, and an exit position best described as analytical because they are not straining against anything — the field simply reorganizes around their axioms. Honor_culture_practitioners sit at the target end: powerless once the surrounding culture stops recognizing their claims, trapped because there is no migration path back into a framework the world no longer speaks. Clergy_and_moral_reformers are agenda-setters with mobile exit — they are producing the shift, not caught by it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandatrophy question is unusual: rather than asking whether an institution's mandate outlived its function, it asks whether the very possibility of stating honor-culture's founding problem (unadjudicated reputational insult) survived the axiom replacement. The founding_problem_status is authored 'dead' not because the problem was solved by a better institution (that is a different reading's claim) but because dignity culture's axioms make the problem itself stop being a problem — insult no longer damages an intrinsic, inalienable worth, so there is nothing left to vindicate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_substrate,
    'Is the dignity-culture substrate a genuine, now-irreversible feature of moral reality (a mountain), or is it itself a constructed cultural arrangement that happens to benefit dignity-culture adherents by rendering the honor-culture alternative illegible — making this reading''s mountain claim a false summit?',
    'Test for reversibility: identify subcultures or historical episodes (e.g., duel-adjacent violence in modern gang or prison honor codes, contemporary ''stand your ground'' reputational violence) where honor-culture axioms partially re-emerge under stress; persistent re-emergence would suggest a constructed, contestable arrangement rather than an irreversible substrate.',
    'If honor-culture axioms demonstrably re-emerge under the right structural conditions (institutional collapse, resource scarcity, weak state monopoly on violence), this reading''s mountain claim should be revised toward tangled_rope or scaffold — dignity culture would be a durable-but-conditional coordination equilibrium favoring its inheritors, not an irreversible law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_substrate, conceptual, 'Whether dignity-culture displacement is a genuine irreversible substrate shift or a contestable, beneficiary-favoring arrangement dressed as natural law.').

omega_variable(
    kernel_disaggregation_locus,
    'Where exactly does the disagreement between the three kernel readings live — is it about WHICH cause was operative (institutional vs. axiomatic vs. overdetermined), or about WHAT KIND of claim is being made (a mechanism claim vs. a sufficiency claim vs. a substrate claim)?',
    'Compare the three readings'' implicit counterfactuals: the contraction_reading implies dueling could not resume even if courts and banks vanished (axioms alone bar it); the institutional_displacement_reading implies dueling would resume if courts/banking/libel-law infrastructure collapsed even with dignity-culture rhetoric intact; the overdetermined_composite_reading implies removing any single factor would not have prevented the decline. These counterfactuals are empirically distinguishable in principle via comparative case studies of societies with partial institutional collapse but retained dignity-culture rhetoric.',
    'If the counterfactual test favors the institutional reading (dueling resumes when courts vanish regardless of rhetoric), this reading''s mountain claim collapses into an overstated case of what is actually rope-level coordination dependent on background institutions — favoring reclassification toward tangled_rope or rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disaggregation_locus, conceptual, 'Where the three sibling readings of the dueling-disappearance kernel actually locate their disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(duel_tr_t20, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(duel_tr_t40, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 40, 0.06).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(duel_tr_t80, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 80, 0.09).
narrative_ontology:measurement(duel_tr_t100, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(duel_be_t20, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 20, 0.07).
narrative_ontology:measurement(duel_be_t40, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(duel_be_t80, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 80, 0.14).
narrative_ontology:measurement(duel_be_t100, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 100, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dueling_disappearance_mechanism__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dueling_disappearance_mechanism kernel. contraction_reading (this file) claims axiom-substrate replacement as sufficient and treats the successor dignity-culture framework as an irreversible mountain. institutional_displacement_reading claims institutional substitution (courts, banking, libel law) as the operative mechanism and would likely classify as rope or tangled_rope (functional institutional coordination outcompeting an older mechanism). overdetermined_composite_reading claims no single sufficient cause and would likely classify differently again, treating the decline as a convergence rather than a substrate replacement or institutional substitution. Each reading authors its own epsilon, beneficiary/victim structure, and claimed_type independently, per the ε-invariance principle; they are linked here for contamination and family-tracing purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
