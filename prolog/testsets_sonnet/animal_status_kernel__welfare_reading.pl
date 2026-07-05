% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Animal Welfare Regulation Regime (Sentience-Constrained Property Reading)
 *   domain: moral_philosophy/legal_theory/animal_ethics
 *
 * SUMMARY:
 *   This story instantiates the welfare reading of the contested animal
 *   status kernel: animals are sentient beings whose suffering matters
 *   morally, but property status is retained and use continues, constrained
 *   by welfare obligations that regulate pain rather than eliminate use. This
 *   is a distinct constraint from the property reading (no
 *   suffering-relevance, no welfare floor) and the abolitionist reading
 *   (property status itself is the injustice; all use impermissible) — each
 *   is a separate structural claim with its own extraction profile, authored
 *   as a separate story and linked via network.affects_constraints. The
 *   welfare reading produces moderate extractiveness: welfare compliance
 *   imposes real costs on industry (housing, transport, slaughter methods)
 *   that would not exist under unconstrained property, but the framework's
 *   core function is to make continued use of animals as resources
 *   sustainable rather than to end it. Extractiveness declines over the
 *   interval as welfare standards became normalized industry cost rather than
 *   contested overhead; theater ratio rises as certification and labeling
 *   increasingly perform ethical reassurance beyond what the underlying
 *   welfare improvements deliver — the 'happy meat' phenomenon abolitionists
 *   identify as new welfarism's core critique.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.52).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.58).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Animal Welfare Regulation Regime (Sentience-Constrained Property Reading)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/legal_theory/animal_ethics").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '8e0d3ba4-4644-45a0-a4ba-7cf81e941e78').
narrative_ontology:cs_kernel_codification('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', formalized).
narrative_ontology:cs_authority_grounding('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', extraction).
narrative_ontology:cs_interpretation_layer_present('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78').
narrative_ontology:cs_reading_relation('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', animal_status_kernel__property_reading, influences).
narrative_ontology:cs_reading_relation('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', foundational, suffering_capacity_grounds_moral_relevance).
narrative_ontology:cs_axiom_status(suffering_capacity_grounds_moral_relevance, holdable).
narrative_ontology:cs_axiom_grounding('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', suffering_capacity_grounds_moral_relevance, deontological).
narrative_ontology:cs_axiom('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', foundational, regulated_use_is_morally_permissible).
narrative_ontology:cs_axiom_status(regulated_use_is_morally_permissible, holdable).
narrative_ontology:cs_axiom_grounding('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', regulated_use_is_morally_permissible, instrumental).
narrative_ontology:cs_reference_frame('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', post_sentience_science_regulatory_consensus).
narrative_ontology:cs_drift_state('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', contemporary_new_welfarism_critique_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8e0d3ba4-4644-45a0-a4ba-7cf81e941e78', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, livestock_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, welfare_certification_bodies).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, consumers_seeking_ethical_reassurance).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, abolitionist_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, consumers_seeking_ethical_reassurance).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, animal_sentience_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, regulated_use_permissibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Absorbs the compliance cost of welfare standards (cage-free space requirements, stunning protocols, transport limits) but retains the core right to breed, confine, and slaughter animals for profit. Lobbies extensively on the content of welfare rules, shaping them to be compatible with continued production at scale. Markets compliance itself as a consumer-facing virtue, converting a cost center into a premium-pricing opportunity.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, livestock_industry, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, livestock_industry, agenda_setter).

% Operates under institutional animal care and use committees that require pain minimization and justification of procedures, but retains authority to use animals in experimentation. The welfare framework legitimizes continued use by demonstrating due diligence rather than eliminating the underlying practice.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, national).

% Designs and audits welfare standards (humane certification labels, transport regulations, housing minimums), collecting fees and building institutional authority from administering the regime. Has a direct financial interest in the continuation of regulated use rather than abolition, since certification has no product if use ends.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, welfare_certification_bodies, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, welfare_certification_bodies, agenda_setter).

% Purchases welfare-labeled products at a premium to reduce moral discomfort about consumption. Pays more money for a credible sense that suffering has been minimized, but the underlying practice of raising and killing animals for food or research continues unchanged in kind, only moderated in degree.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, consumers_seeking_ethical_reassurance, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, consumers_seeking_ethical_reassurance, payer).

% Confined, bred, and slaughtered under conditions regulated to reduce (not eliminate) pain — larger cages, faster stunning, shorter transport times. Have no capacity to consent, object, or exit; their suffering is measured, budgeted, and traded off against production cost, but their fundamental status as usable property is never in question under this reading.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, farmed_animals, payer,
    powerless, immediate, trapped, national).

% Used in experimentation under protocols requiring justification and pain mitigation (anesthesia requirements, endpoint criteria, the 3Rs framework). Welfare oversight determines HOW they may be used, never WHETHER — their institutional purpose as research material persists across every welfare reform.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, laboratory_animals, payer,
    powerless, immediate, trapped, national).

% Argue the welfare framework is the mechanism by which continued use is legitimized rather than challenged — that welfare reforms make the public comfortable with 'happy meat' and defer the more fundamental question of property status indefinitely. Their position that use itself is categorically wrong is treated as politically unserious within the regulatory process that welfare bodies and industry jointly control; they participate in public comment processes that rarely alter the underlying property framework.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, abolitionist_advocates, payer).

% Drafts and enforces welfare statutes (housing standards, slaughter methods, transport rules), balancing industry input against animal advocacy pressure. Positioned as neutral arbiter but structurally dependent on industry cooperation for enforcement data and compliance, and on the continued legitimacy of use-with-welfare as the governing paradigm.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__welfare_reading, livestock_industry).
narrative_ontology:fixing_cost_class(animal_status_kernel__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared floor of treatment standards across an industry that would otherwise face a race-to-the-bottom on animal treatment, giving producers a predictable compliance target and consumers a legible signal of reduced-suffering production.
% TRANSFER_FUNCTION: Moves compliance costs from producers to consumers (via price premiums) and moves suffering-reduction (not elimination) from animals to the regulatory apparatus; moves moral legitimacy from the practice of use itself to the practice of regulated use.
% ABSENT_VOICES: Abolitionist advocates are present in public comment and advocacy but structurally excluded from the premise-setting stage — the welfare framework's regulatory processes take continued use as a given and negotiate only its conditions. The animals themselves have no voice in any procedural sense; their interests are represented exclusively through human intermediaries who have their own institutional stakes in the framework's continuation.
% DISAPPEARANCE_RATIONALE: If welfare regulation vanished overnight, either the property reading would reassert unconstrained (worse conditions, no pain-minimization floor) or public pressure would force a rapid renegotiation toward either stricter regulation or abolition — either way, the entire apparatus of certification bodies, compliance industries, and premium-labeled markets would need to reorganize, and animal treatment conditions would shift materially in one direction or the other.
% FOUNDING_PROBLEM: Industrial-scale animal use (factory farming, biomedical testing) produced visible, publicly objectionable suffering that threatened both the moral legitimacy and the continued social license of the underlying industries; welfare regulation was built to make continued use survivable against rising sentience-based moral concern.
% FOUNDING_PROBLEM_CORROBORATION: Industry and certification bodies attest the founding problem (excessive, gratuitous suffering) has been substantially addressed through welfare standards and continues to be actively managed. Abolitionist scholars and animal law academics — a source outside the beneficiary set — argue the founding problem (animals' fundamental status as usable property) was never addressed at all, only its most visible symptoms, and that welfare reform functions precisely to prevent the deeper question from being asked; this corroboration is external to livestock industry, certification bodies, and research institutions.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.52) sits in the moderate band deliberately: the welfare reading is neither the unconstrained extraction of the property reading nor the zero-tolerance abolition of the abolitionist reading. Suppression (0.58) reflects that animals remain legally without standing to exit or object — the suppression is total for the animals themselves but the metric here describes the constraint's structural coercive requirement (legal enforcement of welfare minimums against non-compliant producers) rather than animal agency, which does not exist under any reading. Theater ratio (0.44) is substantial and rising, reflecting the increasing gap between welfare labeling as consumer-facing signal and welfare improvement as measured suffering reduction — precisely the 'new welfarism' critique. Accessibility collapse (0.5) and resistance (0.55) are moderate: alternatives (full abolition, or unconstrained use) both remain live positions actively argued by organized advocacy on both sides, unlike a mountain where alternatives have genuinely closed off.
 *
 * DIRECTIONALITY LOGIC:
 *   Livestock industry, biomedical research institutions, and welfare certification bodies are structural beneficiaries — the industry retains its core extractive function while shifting compliance cost into consumer prices and certification fees; certification bodies have no product without continued use. Consumers seeking ethical reassurance are a hybrid beneficiary/payer: they pay premiums but receive genuine (if partial) moral comfort. Farmed and laboratory animals are the clearest victims — trapped, powerless, immediate time horizon, with no capacity for exit of any kind; their suffering is what the entire regulatory apparatus claims to manage but does not eliminate. Abolitionist advocates are a distinct kind of victim: not physically harmed but structurally excluded from having their premise (property status itself is the injustice) taken seriously within the regulatory process the welfare framework legitimizes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (visible, publicly objectionable industrial suffering) is genuinely partially addressed — welfare standards have measurably improved conditions in many jurisdictions relative to the unconstrained property baseline. This prevents a naive mandatrophy read that would dismiss welfare reform as pure theater. But the founding_problem_status is authored as contested rather than resolved, because the deeper question the constraint was arguably built to defer — animals' fundamental status as usable property — remains untouched by every welfare reform. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is the signal: this is not a dead mandate coasting on inertia (a piton), it is an actively defended settlement (tangled_rope) that performs resolution of a problem it has only partially solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    new_welfarism_legitimation_effect,
    'Does welfare regulation reduce net animal suffering over time by improving conditions, or does it increase net suffering by extending the social and moral license for use — making the public comfortable enough with ''happy meat'' framing that use expands or persists longer than it would under an unmediated property reading facing full moral scrutiny?',
    'Longitudinal comparison of aggregate animal use volumes and per-animal suffering metrics in jurisdictions with strong welfare regimes versus jurisdictions with weak or absent welfare regulation, controlling for economic and cultural confounds; also survey data on whether welfare labeling measurably reduces consumer willingness to support abolition.',
    'If welfare regulation net-increases use volume by legitimizing continued consumption, the tangled_rope classification is conservative — the coordination function (reduced per-unit suffering) may be structurally outweighed by the extraction function (expanded legitimacy for the underlying practice), pushing the true classification toward snare from the animals'' perspective specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(new_welfarism_legitimation_effect, empirical, 'The abolitionist new-welfarism critique: does welfare reform reduce or entrench suffering-generating use.').

omega_variable(
    sentience_threshold_ambiguity,
    'Which animals qualify as sufficiently sentient to trigger welfare obligations under this reading, and is that threshold principled or drawn to minimize disruption to existing industries (e.g. widespread exclusion of fish, insects, and many laboratory-used species from meaningful welfare protection)?',
    'Comparative analysis of welfare statute coverage against best-available comparative neuroscience on nociception and affective states across taxa; examine correlation between economic stakes of an industry and the strength of welfare protections extended to the animals it uses.',
    'If sentience thresholds correlate more strongly with industry lobbying power than with neuroscientific evidence, the welfare reading''s moral premise (suffering-capacity determines moral relevance) is being applied selectively in a way that serves extraction rather than genuinely constraining it — strengthening the tangled_rope reading over a more benign rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_threshold_ambiguity, conceptual, 'Whether sentience-based inclusion criteria are principled or industry-shaped.').

omega_variable(
    reading_choice_as_kernel_framing,
    'Is the welfare reading the natural default reading of animal moral status, or is its dominance in law and policy itself a product of the industries it constrains having successfully steered the kernel''s interpretation away from the property reading (too extractive to defend post-sentience-science) and away from the abolitionist reading (too costly to accommodate)?',
    'Historical analysis of animal welfare legislation''s origins — whether it emerged primarily from grassroots abolitionist pressure that was diluted through industry negotiation, or from industry-initiated self-regulation later formalized into law.',
    'If the welfare reading''s dominance is itself a product of successful industry framing rather than independent philosophical consensus, this reframes the entire kernel contest: the ''moderate, reasonable middle'' position is not neutral but is the outcome preferred by whichever side has the most to lose from either alternative reading prevailing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_choice_as_kernel_framing, conceptual, 'Whether the welfare reading''s centrality reflects genuine consensus or successful industry framing of the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1960, animal_status_kernel__welfare_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(anim_tr_t1975, animal_status_kernel__welfare_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(anim_tr_t1990, animal_status_kernel__welfare_reading, theater_ratio, 1990, 0.34).
narrative_ontology:measurement(anim_tr_t2005, animal_status_kernel__welfare_reading, theater_ratio, 2005, 0.39).
narrative_ontology:measurement(anim_tr_t2015, animal_status_kernel__welfare_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(anim_tr_t2025, animal_status_kernel__welfare_reading, theater_ratio, 2025, 0.44).

% Extraction over time
narrative_ontology:measurement(anim_be_t1960, animal_status_kernel__welfare_reading, base_extractiveness, 1960, 0.68).
narrative_ontology:measurement(anim_be_t1975, animal_status_kernel__welfare_reading, base_extractiveness, 1975, 0.63).
narrative_ontology:measurement(anim_be_t1990, animal_status_kernel__welfare_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(anim_be_t2005, animal_status_kernel__welfare_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(anim_be_t2015, animal_status_kernel__welfare_reading, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement(anim_be_t2025, animal_status_kernel__welfare_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1960, animal_status_kernel__welfare_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(anim_su_t1975, animal_status_kernel__welfare_reading, suppression_requirement, 1975, 0.66).
narrative_ontology:measurement(anim_su_t1990, animal_status_kernel__welfare_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement(anim_su_t2005, animal_status_kernel__welfare_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(anim_su_t2015, animal_status_kernel__welfare_reading, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(anim_su_t2025, animal_status_kernel__welfare_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__welfare_reading, 0.1).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the animal_status_kernel, each authored as a separate ε-invariant story per the decomposition principle: property_reading (animals excluded from moral considerability; ownership rights only), welfare_reading (this story; sentience triggers pain-minimization obligations but use continues), and abolitionist_reading (property status itself is the injustice; all use impermissible). The three readings share the same underlying kernel — what moral status animals hold — but instantiate structurally distinct constraints with different beneficiary/victim sets, different extractiveness profiles, and different classifications. They are linked bidirectionally: property_reading's persistence creates the baseline the welfare_reading reforms against and is itself pressured by welfare_reading's partial legitimacy gains; welfare_reading's continuation is the abolitionist_reading's primary target of critique (the new-welfarism argument that welfare reform forecloses abolition by making use tolerable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
