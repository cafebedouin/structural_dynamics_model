% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__welfare_reading, []).

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
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Animal Welfare Regulation Regime (Suffering-Minimization within Property Status)
 *   domain: moral_philosophy/legal_theory/animal_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the welfare reading of the contested
 *   animal-status kernel: animals are sentient beings whose suffering counts
 *   morally, but property status and continued instrumental use are retained,
 *   constrained rather than abolished by welfare obligations. Industry
 *   absorbs compliance costs while retaining the underlying extraction
 *   (animal life and labor converted to product); the reduction in acute
 *   suffering is real but partial, and the regime's stability depends on the
 *   public accepting regulated use as morally adequate. The extractiveness is
 *   authored as moderate (0.52) rather than high or low precisely because the
 *   welfare reading's own structure caps suffering without eliminating the
 *   underlying transfer — this is the reading's defining structural feature,
 *   not a hedge between the sibling readings.
 *
 * KEY AGENTS:
 *   - animal_agriculture_industry: primary beneficiary and de facto co-author of the standards it must meet
 *   - farmed_animals: primary bearer of residual, regulation-permitted suffering, wholly without direct voice
 *   - welfare_regulators: agenda-setters whose institutional survival depends on the regulated-use equilibrium persisting
 *   - abolitionist_advocates: excluded critics whose 'new welfarism' argument is this story's central omega
 *   - animal_welfare_scientists: analytical observers whose findings are used selectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.52).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.58).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Animal Welfare Regulation Regime (Suffering-Minimization within Property Status)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/legal_theory/animal_ethics").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '0196bc1f-54b5-4178-a19d-08e891fa197c').
narrative_ontology:cs_kernel_codification('0196bc1f-54b5-4178-a19d-08e891fa197c', distributed).
narrative_ontology:cs_authority_grounding('0196bc1f-54b5-4178-a19d-08e891fa197c', practice).
narrative_ontology:cs_interpretation_layer_present('0196bc1f-54b5-4178-a19d-08e891fa197c').
narrative_ontology:cs_reading_relation('0196bc1f-54b5-4178-a19d-08e891fa197c', animal_status_kernel__property_reading, influences).
narrative_ontology:cs_reading_relation('0196bc1f-54b5-4178-a19d-08e891fa197c', animal_status_kernel__abolitionist_reading, influences).
narrative_ontology:cs_axiom('0196bc1f-54b5-4178-a19d-08e891fa197c', foundational, suffering_capacity_grounds_partial_moral_status).
narrative_ontology:cs_axiom_status(suffering_capacity_grounds_partial_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('0196bc1f-54b5-4178-a19d-08e891fa197c', suffering_capacity_grounds_partial_moral_status, deontological).
narrative_ontology:cs_axiom('0196bc1f-54b5-4178-a19d-08e891fa197c', foundational, regulated_use_is_morally_adequate_response_to_sentience).
narrative_ontology:cs_axiom_status(regulated_use_is_morally_adequate_response_to_sentience, holdable).
narrative_ontology:cs_axiom_grounding('0196bc1f-54b5-4178-a19d-08e891fa197c', regulated_use_is_morally_adequate_response_to_sentience, instrumental).
narrative_ontology:cs_reference_frame('0196bc1f-54b5-4178-a19d-08e891fa197c', unregulated_industrial_use_baseline).
narrative_ontology:cs_drift_state('0196bc1f-54b5-4178-a19d-08e891fa197c', contemporary_new_welfarism_critique_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0196bc1f-54b5-4178-a19d-08e891fa197c', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, welfare_certification_bodies).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, consumers_seeking_ethical_reassurance).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, abolitionist_advocates).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, animal_sentience_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, humane_use_compatibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Absorbs welfare-compliance costs (larger cages, stunning protocols, transport limits) as a manageable operating expense, then markets compliance as ethical assurance. Lobbies to set the welfare standards it must meet, ensuring thresholds remain compatible with continued high-volume production. Can relocate production or reformulate branding faster than regulation can tighten.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_agriculture_industry, agenda_setter).

% Generate revenue by auditing and certifying compliance with welfare standards. Their continued existence depends on use continuing under regulation rather than ending; abolition of use would eliminate their function entirely.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, welfare_certification_bodies, beneficiary,
    organized, biographical, mobile, national).

% Purchase welfare-labeled products to resolve moral discomfort about consumption without changing consumption itself. Benefit from the reassurance the labeling provides regardless of how much suffering the underlying standard actually prevents.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, consumers_seeking_ethical_reassurance, beneficiary,
    moderate, biographical, mobile, national).

% Bear whatever residual suffering the welfare standard permits — standards constrain but do not eliminate confinement, transport stress, and slaughter. Have no capacity to exit, contest, or be represented directly in the standard-setting process; their interests are proxied entirely by human advocates and regulators.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, farmed_animals, payer,
    powerless, immediate, trapped, national).

% Argue that welfare reform legitimizes and stabilizes continued use by making the public comfortable with 'happy meat,' displacing political energy that could otherwise pursue abolition. Their critique is heard in public discourse but rarely shapes the regulatory instruments themselves, which are drafted primarily by industry and welfare-science technocrats.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, abolitionist_advocates, excluded).

% Draft and enforce minimum-suffering standards, balancing industry feasibility against welfare-science recommendations. Their institutional survival depends on the continued existence of a regulated-use regime rather than either pole (unregulated use or abolition).
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, welfare_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Study indicators of animal suffering and stress to inform standards. Their findings are used selectively by regulators and industry to justify whatever threshold is politically and economically feasible, sometimes independent of what the science would support if followed to its conclusion.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_welfare_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:fixing_cost_class(animal_status_kernel__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, auditable standard for what counts as acceptable treatment of sentient animals under continued use, allowing industry, regulators, and consumers to coordinate around a single threshold rather than each actor privately negotiating or ignoring suffering.
% TRANSFER_FUNCTION: Moves moral legitimacy and consumer trust toward the industry and certification apparatus, in exchange for constrained (not eliminated) suffering reduction; the animals bear the residual suffering the standard still permits, while advocates who want full abolition are moved further from their goal as the regulated regime stabilizes public acceptance of use.
% ABSENT_VOICES: The animals themselves have no direct voice in standard-setting and are represented only through proxies (scientists, advocates) whose recommendations are filtered through industry feasibility constraints. Abolitionist advocates are present in public debate but structurally excluded from drafting the instruments that actually set thresholds.
% DISAPPEARANCE_RATIONALE: Industry, certifiers, and reassurance-seeking consumers would say the world rearranges catastrophically — without welfare standards, uncontrolled practices and consumer backlash would destabilize markets. Abolitionists would say the world barely changes in the morally relevant sense: use continues either way, and the welfare label is chiefly cosmetic legitimation rather than a substantive check on suffering. The two camps dispute which counterfactual is the honest one.
% FOUNDING_PROBLEM: Documented systemic cruelty in industrial animal use (extreme confinement, inhumane slaughter, unregulated transport) that provoked public outrage and threatened the social license of the industries involved.
% FOUNDING_PROBLEM_CORROBORATION: Independent animal welfare scientists and investigative journalism outside the industry attest that some acute cruelties have been reduced under regulation, corroborating partial success. However, longitudinal audits by advocacy organizations and some regulatory-capture research (e.g. on industry-drafted standards) attest that the founding problem — systemic suffering at scale — remains substantially live, just less visible; no corroborating source from entirely outside both industry and advocacy exists, which is itself a gap worth naming.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, contested).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__welfare_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction starts higher (0.62) reflecting early-regime conditions where welfare science was less developed and standards were laxer, then declines and stabilizes around 0.52 as compliance regimes matured — genuine suffering-reduction did occur. Theater ratio rises over the interval (0.25 to 0.42) as certification and labeling apparatus grows faster than the underlying substantive change in practice, consistent with the abolitionist 'happy meat' critique: an increasing share of the visible activity is reassurance-generation rather than suffering-reduction. Suppression rises modestly (0.45 to 0.58) as the regime matures into an enforcement apparatus that forecloses more radical alternatives (outright bans, rights-based reform) by occupying the political space with a seemingly adequate compromise.
 *
 * PERSPECTIVAL GAP:
 *   From the industry and certifier seats, this looks like a rope: genuine coordination around a real ethical concern, delivering measurable suffering reduction at bearable cost. From the farmed-animal and abolitionist seats, it looks like a tangled rope at best (real but partial coordination riding on continued extraction) or a snare in the abolitionist's harshest reading (coordination language covering for the fact that suffering and property status persist). The engine should compute this seat divergence structurally from power/exit differentials, not from any authored per-seat label.
 *
 * DIRECTIONALITY LOGIC:
 *   Industry and certifiers sit near the beneficiary end: they collect economic and reputational value from a system that legitimizes continued use at manageable cost. Farmed animals sit at the extreme target end — trapped, powerless, immediate time horizon, bearing whatever suffering the standard still permits with zero capacity to exit or renegotiate. Abolitionist advocates are payers in a different register: they pay in lost political momentum, their preferred abolition path structurally disadvantaged by the welfare regime's public legitimacy. Regulators are agenda-setters whose interests are institutional continuity of the regulated-use compromise itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (systemic, visible cruelty) is genuinely partially resolved in the acute-cruelty dimension — this prevents dismissing the whole regime as pure theater. But the underlying moral claim animating the original reform (suffering matters) has NOT been fully honored, because property status and instrumental use continue; the regime's mandate has shifted from 'address the cruelty problem' to 'manage public comfort with continued use,' which is a live mandatrophy candidate the founding_problem_status: contested field is designed to surface rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    new_welfarism_legitimation_effect,
    'Does welfare regulation function as a genuine step toward reduced suffering and eventual abolition, or does it primarily stabilize and legitimize continued use by resolving public moral discomfort without proportionate suffering reduction?',
    'Longitudinal comparison of per-capita suffering-relevant outcomes (confinement density, transport duration, slaughter method distribution) against public support for abolition-oriented policy in jurisdictions with mature welfare regimes versus those without, controlling for advocacy intensity.',
    'If welfare regulation measurably suppresses movement toward abolition beyond what suffering reduction alone would predict, the welfare reading functions partly as a tangled rope propping up continued extraction under coordination cover; if abolition-oriented momentum is unaffected or increased, the welfare reading is closer to a genuine transitional rope/scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_welfarism_legitimation_effect, empirical, 'Whether welfare reform legitimizes continued use rather than merely reducing its severity — the ''happy meat'' critique.').

omega_variable(
    sentience_threshold_for_moral_relevance,
    'Is the welfare reading''s premise — that suffering-capacity grounds partial moral inclusion without full personhood — a stable, non-arbitrary line, or does it collapse toward either the property reading (suffering is irrelevant, only ownership matters) or the abolitionist reading (suffering-capacity implies a right against being property) once pressed?',
    'Philosophical analysis of whether any principled threshold exists between ''suffering counts but personhood-based rights do not follow'' and the two poles; examine analogous historical moral-status disputes (e.g. evolving legal personhood doctrines) for precedent on whether such intermediate positions have proven stable over time.',
    'If no stable threshold exists, the welfare reading is inherently transitional and likely to drift toward one pole or the other over long time horizons, which would inform whether this constraint should itself carry a sunset/scaffold character rather than being treated as a stable equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sentience_threshold_for_moral_relevance, conceptual, 'Whether the welfare reading''s intermediate moral-status claim is philosophically stable or an unstable compromise.').

omega_variable(
    who_sets_the_threshold,
    'To what extent are welfare standards actually set by independent animal welfare science versus by industry feasibility constraints, given regulators'' institutional dependence on the continued viability of the regulated-use industries they oversee?',
    'Comparative analysis of welfare-science recommendations at the point of proposal versus the final adopted regulatory thresholds, tracking where and why divergence occurs and which stakeholders'' input predicts the divergence.',
    'High divergence tracking industry feasibility rather than science would indicate regulatory capture, pushing this constraint''s structural reality toward tangled_rope or even snare at the regulator-industry seat; low divergence would support the coordination-function reading regulators claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_sets_the_threshold, empirical, 'Whether welfare thresholds are science-driven or industry-captured.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anim_tr_t8, animal_status_kernel__welfare_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(anim_tr_t16, animal_status_kernel__welfare_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(anim_tr_t24, animal_status_kernel__welfare_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(anim_tr_t32, animal_status_kernel__welfare_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__welfare_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(anim_be_t8, animal_status_kernel__welfare_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(anim_be_t16, animal_status_kernel__welfare_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(anim_be_t24, animal_status_kernel__welfare_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(anim_be_t32, animal_status_kernel__welfare_reading, base_extractiveness, 32, 0.52).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__welfare_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(anim_su_t8, animal_status_kernel__welfare_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(anim_su_t16, animal_status_kernel__welfare_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(anim_su_t24, animal_status_kernel__welfare_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(anim_su_t32, animal_status_kernel__welfare_reading, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__welfare_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__welfare_reading, 0.12).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of animal_status_kernel. property_reading treats animals as pure property with no suffering-relevant moral status (lowest suppression of alternative moral claims, but this is because the claim is not contested on its own terms — it simply denies the premise). abolitionist_reading treats property status itself as the injustice, generating a full victim-set (all used animals) and near-total extractiveness under its own lights. welfare_reading sits structurally between them: it partially adopts the abolitionist premise (suffering matters) while preserving the property_reading's practical outcome (use continues), which produces the moderate ε and the tangled_rope structural claim authored here. Each reading was generated independently with its own ε, beneficiaries, and victims per the ε-invariance principle; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
