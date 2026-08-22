% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Restrictive Anthropocentric Reading of the Legal Personhood Boundary
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested legal-personhood-boundary
 *   kernel: the restrictive anthropocentric reading, under which legal
 *   personhood attaches only to born human beings possessing baseline
 *   cognitive capacity. Under this reading, fetuses, ecosystems, and
 *   non-human or artificial cognitive systems are categorically excluded from
 *   the victim set for standing purposes, pregnant-person autonomy is
 *   maximized, and state intervention into reproduction and environmental
 *   permitting is minimized because there is no rights-bearing claimant on
 *   the other side of those disputes. This is not a story about the kernel
 *   contest itself — it does not average over or describe the sibling
 *   readings, which are separate constraints with their own ε and their own
 *   beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - pregnant_persons: primary beneficiary (moderate/constrained) — retains bodily autonomy because no competing personhood claim exists
 *   - fetuses_denied_standing: primary excluded party (powerless/trapped) — has no legal voice under this reading
 *   - industrial_and_agricultural_operators: secondary beneficiary (institutional/arbitrage) — externalizes environmental costs absent ecosystem standing
 *   - constitutional_courts: agenda_setter (institutional/analytical) — administers and could shift the line
 *   - developmental_potentiality_advocates and functional_capacity_advocates: excluded/organized — proxy representatives for the excluded classes under sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.42).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.55).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Restrictive Anthropocentric Reading of the Legal Personhood Boundary").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '2bb16c52-29e4-4555-ae32-8e21ccd3b18c').
narrative_ontology:cs_kernel_codification('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', distributed).
narrative_ontology:cs_authority_grounding('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', lineage).
narrative_ontology:cs_interpretation_layer_present('2bb16c52-29e4-4555-ae32-8e21ccd3b18c').
narrative_ontology:cs_reading_relation('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', foundational, birth_is_the_threshold_event_for_legal_personhood).
narrative_ontology:cs_axiom_status(birth_is_the_threshold_event_for_legal_personhood, holdable).
narrative_ontology:cs_axiom_grounding('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', birth_is_the_threshold_event_for_legal_personhood, conventional).
narrative_ontology:cs_axiom('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', foundational, bodily_autonomy_of_the_born_person_is_categorically_prior_to_any_prenatal_interest).
narrative_ontology:cs_axiom_status(bodily_autonomy_of_the_born_person_is_categorically_prior_to_any_prenatal_interest, holdable).
narrative_ontology:cs_axiom_grounding('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', bodily_autonomy_of_the_born_person_is_categorically_prior_to_any_prenatal_interest, deontological).
narrative_ontology:cs_reference_frame('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', born_alive_common_law_rule).
narrative_ontology:cs_drift_state('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', post_neuroscience_and_ai_capability_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2bb16c52-29e4-4555-ae32-8e21ccd3b18c', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_healthcare_providers).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, industrial_and_agricultural_operators).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, born_rights_bearing_citizens).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses_denied_standing).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems_denied_standing).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, advanced_ai_systems_denied_standing).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, future_generations_bearing_externalized_costs).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, bodily_autonomy_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, born_alive_rule).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, liberal_individualist_personhood_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain sole legal decision-making authority over their own bodies and pregnancies because the fetus is not counted as a rights-bearing person against them. This reading is the structural basis of their protection from state-compelled gestation and criminal liability for pregnancy outcomes.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, constrained, national).

% Can lawfully perform abortion and related reproductive care without exposure to homicide or wrongful-death liability, because the boundary places the fetus outside the class the law protects as a person. Their professional viability depends on this line holding.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_healthcare_providers, beneficiary,
    organized, biographical, constrained, national).

% Externalize environmental costs — emissions, habitat destruction, resource depletion — without facing standing-based legal claims from the ecosystems or non-human animals harmed, because those entities are not persons who can sue or be represented as rights-holders in their own right. Cost internalization would require statutory or constitutional change, not standing under this reading.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, industrial_and_agricultural_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Enjoy a clean, stable, judicially administrable personhood line: everyone born and possessing baseline cognitive capacity is a rights-bearer, full stop. This predictability underwrites contracts, inheritance, criminal procedure, and constitutional litigation without case-by-case capacity or gestational-stage adjudication.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, born_rights_bearing_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Have no independent legal voice, representation, or standing under this reading regardless of gestational stage; any protection they receive is incidental to regulation of the pregnant person or the provider, never grounded in a personhood claim of their own. They cannot exit because they have no legal existence as a claimant at all.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses_denied_standing, payer,
    powerless, immediate, trapped, local).

% Rivers, forests, and species populations can be degraded or destroyed with no legal claimant able to assert harm to the ecosystem itself as a rights-holder; environmental law proceeds instead through statutory permitting and third-party human standing doctrines, which are structurally weaker and more easily captured or defunded.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems_denied_standing, payer,
    powerless, civilizational, trapped, global).

% Regardless of any future demonstrated cognitive sophistication, are categorically excluded from personhood consideration because they are not born humans; any question of their interests or treatment is resolved entirely as property or product-liability law, never as a rights claim of the entity itself.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, advanced_ai_systems_denied_standing, payer,
    powerless, generational, trapped, global).

% Inherit environmental and resource depletion externalities that current standing doctrine could not prevent, because no present-day rights-holder existed to represent the ecosystems or the future persons themselves at the time the harm was done. They pay the accumulated cost with no seat in the litigation that produced it.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, future_generations_bearing_externalized_costs, payer,
    powerless, civilizational, trapped, global).

% Argue personhood should attach at conception and object that this reading licenses what they consider the killing of rights-bearers; they are structurally locked out of prevailing case law under the current constitutional settlement in most permissive jurisdictions, though they remain politically and legislatively active.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, developmental_potentiality_advocates, excluded,
    organized, generational, analytical, national).

% Argue personhood should track demonstrated cognitive capacity regardless of species or biological origin, and object that this reading arbitrarily privileges human birth over demonstrated sentience or rationality; they have no current path to standing for great apes, cetaceans, or advanced AI systems under this reading.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, functional_capacity_advocates, excluded,
    organized, generational, analytical, global).

% Adjudicate and administer the born-human-with-cognitive-capacity line, treating it as the operative constitutional and common-law default; can shift the boundary through case law but have historically done so incrementally and only under sustained political or scientific pressure.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, administrable line for allocating the entire apparatus of legal rights, standing, and protections: born humans with baseline cognitive capacity are persons, full stop, avoiding case-by-case litigation over gestational stage, species membership, or machine cognition in every downstream area of law (contracts, criminal law, tort, inheritance, constitutional rights).
% TRANSFER_FUNCTION: Moves decisional authority and freedom from potential claimants (fetuses, ecosystems, non-human and non-biological entities) to actually-recognized persons (pregnant individuals, born citizens) and to the institutions that benefit from not having to represent those excluded classes (industrial operators avoiding ecosystem standing suits, the state avoiding compelled-gestation liability).
% ABSENT_VOICES: Fetuses, ecosystems, and prospective non-human or artificial cognitive entities have no direct voice in the personhood determination — by construction, none of them can appear as a claimant. Advocates for developmental-potentiality and functional-capacity readings represent them by proxy, but proxy representation is not the same as standing and is structurally weaker in litigation.
% DISAPPEARANCE_RATIONALE: If this restrictive line disappeared and personhood attached at conception or to demonstrated cognitive capacity instead, reproductive law, environmental permitting, tort liability, and AI governance would all be reorganized: abortion access would collapse or require a new balancing doctrine, industrial operators would face ecosystem-standing suits, and any sufficiently sophisticated AI system could in principle claim legal interests against its operator.
% FOUNDING_PROBLEM: Common law and constitutional traditions needed a workable, non-arbitrary boundary for who counts as a rights-bearer, at a time when the practical candidates for personhood were only born human beings; the born-alive rule and capacity thresholds solved administrability and avoided theological or metaphysical disputes about ensoulment or the moral status of the unborn.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and mainstream liberal legal scholarship attest the boundary still solves a live administrability problem and protects settled autonomy interests. Developmental-potentiality and functional-capacity advocates — outside the beneficiary set — attest the founding problem (a non-arbitrary, non-question-begging line) is not actually solved by this reading, only assumed by it, and that the line is under increasing empirical pressure from fetal neuroscience and machine cognition research.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).
:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42): the reading concentrates real benefits (autonomy, litigation predictability, avoided liability) on identifiable beneficiaries while imposing real, if categorically unrecognized, costs on excluded classes — but because the excluded classes are non-agents under this reading's own terms, the extraction operates as an absence of remedy rather than an active transfer, which caps its measured severity relative to a Snare. Suppression (0.55) reflects the active doctrinal and political work required to keep the line where it is against sustained developmental-potentiality and functional-capacity advocacy. Theater is low (0.2): the personhood line does substantive doctrinal work; it is not primarily performative. Accessibility collapse (0.5) is moderate because the line, while judicially entrenched, remains a live subject of amendment and legislative contest — it has not achieved mountain-like closure. Resistance (0.7) is high, reflecting the sustained, organized, well-resourced opposition from both sibling-reading camps.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons and providers sit near the beneficiary end: the reading directly subsidizes their autonomy and practice. Industrial and agricultural operators are institutional beneficiaries with arbitrage-grade exit — they can relocate operations across jurisdictions with different environmental standing regimes, which further dampens their effective extraction exposure under this reading. The four payer classes are powerless and trapped by construction: they cannot exit a boundary that denies them the standing to even appear in the forum that draws it. Future generations are a distinct payer class from present ecosystems because the harm is temporally displaced but structurally identical — no present claimant, no present remedy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a workable, non-arbitrary line for legal personhood) is contested rather than resolved: courts and mainstream liberal scholarship treat it as still live and well-served by this boundary, while excluded-class advocates argue the line was always a convenience assumption rather than a principled resolution, and that empirical developments (fetal neuroscience, machine cognition) have outrun the doctrine's original justifications. This divergence is exactly the kind of status the R5 corroboration check is built to surface — it does not resolve the dispute, it records that the dispute has a genuine outside-the-beneficiary-set challenge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_line_naturalness_vs_construction,
    'Is the born-human-with-cognitive-capacity boundary a principled discovery about the nature of moral/legal personhood, or a historically contingent convenience line that happens to benefit currently-recognized persons and the institutions that rely on the absence of competing claimants?',
    'Track whether the line holds constant as the empirical basis for its two sibling premises (fetal neurological development timelines; demonstrated non-human/artificial cognitive capacity) becomes more precise. A line that shifts in response to new capacity evidence behaves like a functional-capacity line in disguise; a line that holds regardless of capacity evidence behaves like a birth-status convention.',
    'If the line is shown to track birth status alone regardless of capacity evidence, this reading''s coordination justification (administrability grounded in capacity) weakens and its extractive character (excluding capacity-bearing entities purely on grounds of birth/species) becomes more visible, pushing the classification toward Snare from the excluded classes'' perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personhood_line_naturalness_vs_construction, conceptual, 'Whether the restrictive reading tracks a principled capacity criterion or merely birth status.').

omega_variable(
    kernel_reading_which_sibling_prevails,
    'Which of the three sibling readings (restrictive_anthropocentric, developmental_potentiality, functional_capacity) will constitutional courts and legislatures converge toward as fetal neuroscience and machine cognition research mature?',
    'Longitudinal tracking of case law and statute across jurisdictions as the empirical bases cited by each reading''s advocates develop; a stable multi-decade equilibrium on one reading would indicate convergence, continued jurisdictional divergence would indicate the kernel remains genuinely contested.',
    'Convergence toward a sibling reading would not change this story''s authored ε (which is fixed to this reading, per the ε-invariance principle) but would change the reading''s real-world prevalence and hence how much of the constraint space it actually governs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_which_sibling_prevails, empirical, 'Which reading of the personhood kernel will structurally dominate going forward.').

omega_variable(
    environmental_externality_representation_gap,
    'Is the absence of ecosystem standing under this reading better modeled as an intrinsic feature of anthropocentric personhood theory, or as a separable policy choice that could be corrected by statutory standing grants (e.g., rights-of-nature statutes) without abandoning the born-human-with-capacity line for human personhood questions?',
    'Examine jurisdictions that have granted statutory or constitutional standing to rivers/ecosystems while retaining the anthropocentric personhood line for reproductive and civil-rights purposes (e.g., Ecuador, New Zealand''s Whanganui River) — if human personhood and ecosystem standing can vary independently, they are separable constraints.',
    'If separable, the environmental-externality extraction currently attributed to this reading is better decomposed into its own constraint story (an ecosystem-standing constraint) rather than treated as intrinsic to the anthropocentric personhood reading, per the ε-invariance decomposition principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_externality_representation_gap, conceptual, 'Whether ecosystem-standing exclusion is intrinsic to this reading or a separable policy layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement(lega_tr_t60, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(lega_be_t60, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement(lega_su_t60, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__functional_capacity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legal_personhood_boundary kernel. developmental_potentiality_reading assigns personhood at conception (different victim set: born persons' autonomy becomes the payer class; ε authored independently and substantially higher for pregnant persons under that reading). functional_capacity_reading assigns personhood by demonstrated cognitive capacity regardless of species (different victim set: excludes low-capacity born humans in principle, includes high-capacity non-humans; ε authored independently). Each reading is ε-invariant on its own terms; do not average or reconcile ε across the three files. All three link to each other via affects_constraints to preserve the kernel-family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
