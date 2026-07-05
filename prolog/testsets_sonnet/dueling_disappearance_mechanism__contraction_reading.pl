% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Substrate Displacement of Honor-Culture Axioms
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION reading of the
 *   dueling-disappearance kernel: dueling did not lose a competition to
 *   better dispute-resolution institutions (that is the sibling
 *   institutional_displacement_reading) and was not simply overdetermined by
 *   many independent causes acting at once (the sibling
 *   overdetermined_composite_reading). On this reading, the decisive
 *   mechanism is a substrate-level displacement of honor-culture's
 *   foundational axiom (worth is publicly contestable and must be defended
 *   through risk of death) by dignity-culture's foundational axiom (worth is
 *   intrinsic and does not require public vindication). Once the substrate
 *   shifted, dueling did not merely become illegal or inefficient — it became
 *   unintelligible as a meaningful act to the growing population operating
 *   inside dignity-culture premises. This is why the claimed type here is
 *   mountain rather than rope: the constraint is not primarily a coordination
 *   mechanism that lost adherents to competitors, but an irreversible-feeling
 *   shift in the substrate of what counts as a valid reason for lethal risk,
 *   from which there is no legible return path for the displaced framework.
 *
 * KEY AGENTS:
 *   - dignity_culture_bearers: ambient beneficiaries of the substrate shift, institutional/analytical exit
 *   - bureaucratic_professional_classes: institutional beneficiaries who narrate and accelerate the shift, arbitrage exit
 *   - honor_culture_practitioners: primary bearers of the cost, trapped by the axiom's collapse rather than by any single enforcer
 *   - displaced_gentry_codes_of_conduct: the framework itself, non-agent, payer by proxy
 *   - women_and_dependents_of_duelists: excluded incidental beneficiaries, never consulted
 *   - cultural_historians: analytical observer seat reconstructing the mechanism from the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.28).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.62).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Substrate Displacement of Honor-Culture Axioms").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '03a92f89-95f1-4c65-a8bd-dcf9e0c1a857').
narrative_ontology:cs_kernel_codification('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', distributed).
narrative_ontology:cs_authority_grounding('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', practice).
narrative_ontology:cs_interpretation_layer_present('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857').
narrative_ontology:cs_reading_relation('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', foundational, worth_is_intrinsic_not_publicly_contestable).
narrative_ontology:cs_axiom_status(worth_is_intrinsic_not_publicly_contestable, holdable).
narrative_ontology:cs_axiom_grounding('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', worth_is_intrinsic_not_publicly_contestable, deontological).
narrative_ontology:cs_axiom('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', foundational, axiomatic_substrate_change_is_causally_sufficient_alone).
narrative_ontology:cs_axiom_status(axiomatic_substrate_change_is_causally_sufficient_alone, holdable).
narrative_ontology:cs_axiom_grounding('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', axiomatic_substrate_change_is_causally_sufficient_alone, empirically_contingent).
narrative_ontology:cs_reference_frame('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', honor_culture_public_contestability_norm).
narrative_ontology:cs_drift_state('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', post_bellum_professionalization_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('03a92f89-95f1-4c65-a8bd-dcf9e0c1a857', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_bearers).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, bureaucratic_professional_classes).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, displaced_gentry_codes_of_conduct).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inhabit a moral vocabulary in which worth is intrinsic and does not require public vindication through combat. As this vocabulary becomes the ambient common sense of professional, legal, and civic life, dueling stops registering as a coherent option rather than being merely forbidden. They do not administer this shift; they simply live inside the substrate it produces.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_bearers, beneficiary,
    institutional, civilizational, analytical, national).

% Lawyers, civil servants, and professional-class men whose status accrues through credentialed competence and institutional standing rather than personal honor defended by violence. Dignity-culture norms make their basis of status legible and defensible without recourse to the code duello; they have every incentive to narrate the older code as barbaric and are positioned, through control of schools, press, and professional associations, to accelerate the substrate shift.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, bureaucratic_professional_classes, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__contraction_reading, bureaucratic_professional_classes, agenda_setter).

% Men whose entire framework of personal worth is constituted by the willingness to answer insult with the code duello. As dignity-culture displaces the honor axioms that made this framework intelligible to anyone else, their commitments become not merely illegal but unreadable — a duel fought under the old code looks, to the emerging dignity-culture observer, like simple assault or absurd theater rather than an intelligible defense of honor. They cannot translate their framework into the new substrate; there is no exit because the exit is a change in what other people are capable of recognizing as meaningful.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    moderate, biographical, trapped, regional).

% The code duello itself, as a normative system — not a person but a framework — loses its social readability. It persists in isolated pockets and in nostalgic literary form but ceases to function as an intersubjectively recognized method of settling honor claims. Listed here for completeness; it is a framework, not an actor, and is excluded from directionality computation.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, displaced_gentry_codes_of_conduct, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(dueling_disappearance_mechanism__contraction_reading, displaced_gentry_codes_of_conduct).

% Wives, children, and dependents of men bound by the honor code bore the risk of the duelist's death or maiming without ever being consulted on the code's legitimacy. Their objections, where voiced at all, appear in private letters and periodical complaint literature, not in the public discourse that negotiated the code's decline; the substrate shift that ended dueling was not fought or won on their behalf, though they benefited incidentally from its disappearance.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, women_and_dependents_of_duelists, excluded,
    powerless, biographical, trapped, local).

% Reconstruct the axiomatic substrate shift from correspondence, periodical literature, dueling-ground statistics, and legal records. They can trace the vocabulary of honor being progressively unable to find purchase in a print and legal culture increasingly organized around dignity, but cannot experimentally isolate this mechanism from institutional and legal co-factors — that isolation problem is the reading's own omega.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor culture originally coordinated status competition among social equals lacking a trusted third-party enforcer: the duel (and its antecedent threat) let elite men signal credible commitment to defending reputation in a world where courts were weak, slow, or beneath one's dignity to use against a social equal. This reading holds that this coordination function did not get out-competed by better institutions but became conceptually unavailable once the underlying axiom (worth is publicly contestable and must be defended) was displaced.
% TRANSFER_FUNCTION: Nothing is extracted or transferred by the dignity-culture substrate itself, in this reading — it is not a transactional arrangement but a change in the shared premises available for status claims. What is 'transferred' is illegibility: honor-culture practitioners' framework loses its audience, and with it, its capacity to compel recognition or deference from anyone operating in dignity-culture terms.
% ABSENT_VOICES: Honor-culture practitioners themselves, in the historical record, mostly do not theorize their own obsolescence — they experience it as ridicule, as being treated as anachronistic or criminal rather than as parties to a live philosophical dispute about which moral vocabulary should govern. Their objection, where it survives, is defensive and personal (letters, memoirs) rather than a structured counter-argument to dignity-culture's claims, because the very terms of structured public argument had already shifted against them.
% DISAPPEARANCE_RATIONALE: If this reading's mechanism vanished — that is, if dignity-culture axioms suddenly failed to be ambient common sense — the honor-culture practitioners' framework would in principle become legible again, but whether the world 'rearranges' depends on whether one credits the mountain framing (dignity culture as an irreversible substrate, in which case disappearance is close to physically incoherent) or treats it as one contingent cultural formation among others (in which case reversion is imaginable). The verdict is contested precisely because it is contested BETWEEN the readings of this kernel, not merely within this one.
% FOUNDING_PROBLEM: The problem this reading identifies as 'solved' by dueling's disappearance is not a coordination problem at all, in the strict sense — it is the problem of honor-culture axioms losing their grip on what could count as a valid reason to risk death, once an alternative, non-combative theory of intrinsic worth became dominant.
% FOUNDING_PROBLEM_CORROBORATION: Social historians of honor culture (e.g. the bertram wyatt-brown tradition and its critics) attest, from outside the interests of either honor-culture nostalgists or dignity-culture triumphalists, that the vocabulary shift is empirically traceable in periodical and legal-defense literature; however, they are divided on whether this vocabulary shift was itself causally sufficient (this reading's claim) or merely epiphenomenal to institutional change (the sibling institutional_displacement_reading's claim) — no source outside the kernel's own contest adjudicates between the two.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, contested).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-moderate (0.28 at interval end) because this reading denies that anyone is actively extracting from honor-culture practitioners in an ongoing transactional sense — the cost to them is the loss of an intelligible framework, not a rent collected by an identifiable party. Suppression is authored substantially higher (0.62) because dignity-culture's ascendance did involve real coercive machinery layered on top of the substrate shift (anti-dueling statutes, professional sanction, social ostracism of duelists) even though, on this reading, that machinery is downstream of the axiom shift rather than the primary mechanism. Accessibility collapse is authored very high (0.88): once dignity-culture premises become ambient, the honor-culture alternative does not merely become disfavored, it becomes unreadable as a valid framework to most participants in public life. Resistance is authored low (0.12) because honor-culture practitioners largely could not mount an intelligible counter-argument once the shared vocabulary for evaluating such arguments had already moved — resistance requires an audience capable of hearing the claim, and this reading holds that audience contracted, hence the constraint_id.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity-culture bearers and the professional classes sit near the beneficiary end: the shift subsidizes their existing basis of status and requires no violent defense of reputation from them. Honor-culture practitioners sit near the full-target end despite lacking a single enforcer to blame — their directionality is high because the constraint's cost to them (loss of an intelligible framework) is total and non-negotiable, and their exit option is trapped in a distinctive sense: there is no migration path back to honor-culture legibility once the surrounding population has moved. The displaced framework itself is marked non-agent and excluded from directionality math, per the schema's agent-hood gate, though it is named for narrative completeness.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (honor-culture's need for a credible, court-independent mechanism for defending reputation among social equals) is authored dead — dignity-culture premises removed the felt need for that mechanism rather than merely providing a cheaper substitute for it. This differs from a mandatrophy case where an institution persists after its function dies: here, this reading claims the underlying axiomatic need itself dissolved, which is why the type shifts toward mountain (an irreversible substrate condition) rather than piton (an institution kept alive past its function by inertia). The engine's classification of this reading against the sibling readings, where the founding problem is read as institutionally displaced or overdetermined rather than axiomatically dissolved, is exactly the divergence the kernel-reading apparatus exists to expose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_displacement_vs_institutional_substitution,
    'Was the decisive mechanism in dueling''s disappearance a genuine axiomatic substrate shift (dignity-culture premises displacing honor-culture premises), or was the apparent axiom shift itself a downstream rationalization of institutional substitution (courts, banking, libel remedies) that had already made dueling functionally unnecessary?',
    'Fine-grained tracing of the sequence in specific jurisdictions: does dueling decline before, alongside, or after the availability of credible institutional alternatives? If axiomatic-shift language in periodicals and sermons precedes institutional maturation by a meaningful lag, this reading is favored; if institutional maturation consistently precedes the vocabulary shift, the sibling institutional_displacement_reading is favored.',
    'If institutional substitution is causally prior, this reading''s mountain classification is undermined and the constraint is better modeled as a rope whose coordination function was outcompeted — a fundamentally different structural type with a different victim analysis (the honor-culture practitioners would then be victims of institutional obsolescence, not of substrate illegibility).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(axiom_displacement_vs_institutional_substitution, empirical, 'Whether axiom displacement or institutional substitution is causally prior.').

omega_variable(
    mountain_or_constructed_dignity_norm,
    'Is dignity-culture''s ascendance itself a natural, irreversible substrate condition (as this reading''s mountain claim asserts), or is it a constructed cultural formation actively promoted by the bureaucratic-professional classes because it is more compatible with their existing status basis — in which case the mountain framing is itself a false summit obscuring an interested beneficiary group?',
    'Comparative sociology of societies where dignity-culture premises did not become dominant, or later reversions/coexistences (e.g. persistence of honor-culture logics in some regional or subcultural contexts into the 20th century) — genuine irreversibility should show no viable reversion path anywhere, while a constructed-norm reading predicts observable pockets of honor-culture persistence correlated with professional-class weakness.',
    'If dignity-culture''s dominance is shown to be actively maintained by an interested beneficiary class rather than a natural substrate condition, this constraint should reclassify from mountain toward tangled_rope or snare, with bureaucratic_professional_classes as a concentrated beneficiary rather than an incidental one — this is precisely the false-summit pattern the schema''s FSM check exists to catch, which is why beneficiaries were declared on this mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_constructed_dignity_norm, conceptual, 'Whether dignity-culture dominance is a natural substrate or a constructed, interested norm.').

omega_variable(
    kernel_reading_adjudication,
    'Which of the three declared readings of the dueling_disappearance_mechanism kernel (contraction, institutional_displacement, overdetermined_composite) best fits the historical record, and is the disagreement resolvable or a genuine framing indeterminacy?',
    'Cross-reading comparison requires the sibling constraint files (institutional_displacement_reading, overdetermined_composite_reading) to be generated and their computed types compared against this reading''s computed type across the same historical interval; convergence or divergence in engine-computed classification is itself evidence about whether the kernel is genuinely under-determined.',
    'If this reading and institutional_displacement_reading compute to substantially different types (mountain vs rope) on structurally similar underlying data, that divergence is the intended signal of a genuinely contested kernel rather than an authoring error, and should be reported as such rather than reconciled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_adjudication, conceptual, 'Meta-level adjudication question across the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1770, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1770, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1770, 0.05).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1800, 0.06).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1820, 0.08).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1860, 0.12).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1880, 0.14).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(duel_be_t1770, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1770, 0.1).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.14).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1820, 0.18).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1840, 0.22).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1860, 0.25).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1880, 0.27).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1770, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1770, 0.15).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.22).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1820, 0.35).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1840, 0.48).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1860, 0.55).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1880, 0.6).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language claim 'dueling became culturally unthinkable / fell into disuse,' per the ε-invariance principle: measuring the mechanism as axiomatic substrate displacement yields a low-extraction, high-accessibility-collapse mountain profile; measuring it as institutional substitution (sibling institutional_displacement_reading) yields a coordination-function rope/tangled-rope profile with different beneficiaries (legal and banking professions) and different victims; measuring it as overdetermined (sibling overdetermined_composite_reading) yields a composite profile that resists clean typing. All three share the historical interval 1770-1900 and the same nominal subject matter but are structurally distinct constraints with distinct ε values, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
