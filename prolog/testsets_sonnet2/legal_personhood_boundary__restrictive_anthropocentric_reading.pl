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
 *   human_readable: Personhood Boundary — Restrictive Anthropocentric Reading (Born Humans With Cognitive Capacity)
 *   domain: legal philosophy/constitutional law/rights theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested legal-personhood
 *   kernel: the restrictive anthropocentric reading, which fixes personhood
 *   at born humans possessing cognitive capacity. This reading structurally
 *   maximizes pregnant-person autonomy (fetuses are not rights-competitors),
 *   forecloses direct standing for ecosystems and non-human entities, and
 *   minimizes state intervention in both reproductive decisions and
 *   environmental regulation. Two sibling readings exist as separate
 *   constraints, not as alternatives folded into this one: the
 *   developmental_potentiality_reading (personhood at conception) and the
 *   functional_capacity_reading (personhood via demonstrated capacity
 *   regardless of species). Each sibling has its own epsilon, its own victim
 *   set, and its own classification — this file describes only the
 *   restrictive anthropocentric reading's operation, assessed by its own
 *   lights.
 *
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
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Personhood Boundary — Restrictive Anthropocentric Reading (Born Humans With Cognitive Capacity)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal philosophy/constitutional law/rights theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '9802d2cf-9596-4895-b902-0fad610b70bd').
narrative_ontology:cs_kernel_codification('9802d2cf-9596-4895-b902-0fad610b70bd', distributed).
narrative_ontology:cs_authority_grounding('9802d2cf-9596-4895-b902-0fad610b70bd', distributed).
narrative_ontology:cs_reading_relation('9802d2cf-9596-4895-b902-0fad610b70bd', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('9802d2cf-9596-4895-b902-0fad610b70bd', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('9802d2cf-9596-4895-b902-0fad610b70bd', foundational, birth_as_moral_threshold).
narrative_ontology:cs_axiom_status(birth_as_moral_threshold, holdable).
narrative_ontology:cs_axiom_grounding('9802d2cf-9596-4895-b902-0fad610b70bd', birth_as_moral_threshold, conventional).
narrative_ontology:cs_axiom('9802d2cf-9596-4895-b902-0fad610b70bd', foundational, bodily_autonomy_supersedes_prenatal_interest).
narrative_ontology:cs_axiom_status(bodily_autonomy_supersedes_prenatal_interest, holdable).
narrative_ontology:cs_axiom_grounding('9802d2cf-9596-4895-b902-0fad610b70bd', bodily_autonomy_supersedes_prenatal_interest, deontological).
narrative_ontology:cs_axiom('9802d2cf-9596-4895-b902-0fad610b70bd', secondary, species_membership_as_necessary_condition).
narrative_ontology:cs_axiom_status(species_membership_as_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('9802d2cf-9596-4895-b902-0fad610b70bd', species_membership_as_necessary_condition, conventional).
narrative_ontology:cs_reference_frame('9802d2cf-9596-4895-b902-0fad610b70bd', common_law_born_alive_tradition).
narrative_ontology:cs_drift_state('9802d2cf-9596-4895-b902-0fad610b70bd', post_roe_dobbs_realignment_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9802d2cf-9596-4895-b902-0fad610b70bd', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_healthcare_providers).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, extractive_industry_operators).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, biomedical_research_sector).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, late_stage_fetuses_under_this_reading).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_advocates_seeking_ecosystem_standing).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, future_generations_affected_by_resource_extraction).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitively_impaired_infants_and_severely_disabled_humans).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, bodily_autonomy_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, born_alive_rule).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_as_moral_threshold).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain full decisional authority over pregnancy because the fetus does not hold competing personhood status under this reading. Their autonomy is maximized precisely because the boundary excludes the fetus from the rights-bearing class; this is the structural core of the reading's benefit distribution.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, constrained, national).

% Operate within a legal framework that does not treat the fetus as a second patient with independent standing, which lets clinical practice proceed without a competing-rights litigation regime. They benefit from doctrinal clarity but remain exposed to political efforts to relitigate the boundary.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_healthcare_providers, beneficiary,
    organized, biographical, constrained, national).

% Operate under an anthropocentric personhood boundary that denies standing to ecosystems, species, or future generations as such, meaning environmental harms must be litigated through human proxy plaintiffs (nuisance, property, statutory standing) rather than direct rights claims by the harmed entity. This structurally limits injunctive exposure and damages theories available against them.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, extractive_industry_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Conducts embryo research, IVF discard practices, and early-gestation experimentation within a framework that assigns no independent personhood to pre-born entities, enabling research pipelines that a conception-based boundary would foreclose.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, biomedical_research_sector, beneficiary,
    institutional, generational, mobile, global).

% Have no independent legal standing under this reading regardless of gestational development, viability, or capacity for pain perception. Their interests, if any, are represented only derivatively through the pregnant person's decision or through state interest arguments that this reading treats as subordinate to the born-human threshold. They cannot exit the classification that governs them.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, late_stage_fetuses_under_this_reading, payer,
    powerless, biographical, trapped, local).

% Attempt to litigate on behalf of rivers, forests, and species using rights-of-nature theories; under this reading such entities have no direct personhood, so advocates must repeatedly construct human-proxy standing (injury-in-fact to a human plaintiff) to get any hearing at all, raising litigation cost and narrowing available remedies.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_advocates_seeking_ecosystem_standing, payer,
    moderate, generational, constrained, global).

% Bear the long-horizon costs of resource depletion and environmental degradation decided under a legal regime that recognizes no rights-holder capable of representing their interests directly; they have no seat at any table because they do not yet exist as legal subjects.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, future_generations_affected_by_resource_extraction, payer,
    powerless, civilizational, trapped, global).

% Are born humans but the reading's dual requirement (born AND possessing cognitive capacity) creates doctrinal ambiguity about whether severely cognitively impaired humans occupy the same secure personhood status as others — a residual instability the reading does not fully resolve, exposing this group to intermittent guardianship and end-of-life-rights disputes that trade on the capacity clause.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitively_impaired_infants_and_severely_disabled_humans, payer,
    powerless, biographical, trapped, national).

% Legislatures and courts administer where the born-human-with-cognitive-capacity line falls, adjudicating edge cases (anencephalic infants, permanent vegetative state, late abortion regulation) and setting the enforcement machinery — statutes, case law, agency rules — that gives this reading operative force.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, state_reproductive_policy_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the doctrinal architecture and its comparative alternatives, publishing critiques of both over- and under-inclusiveness without directly bearing the constraint's costs or benefits.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, administrable rule for who counts as a rights-bearing legal subject, avoiding the adjudicative chaos of case-by-case metaphysical determinations about moral status and enabling courts, hospitals, and agencies to apply a consistent threshold.
% TRANSFER_FUNCTION: Moves decisional authority and litigation standing toward born humans with cognitive capacity (chiefly pregnant persons and human plaintiffs) and away from entities that might otherwise claim direct rights — fetuses, ecosystems, non-human animals, and future generations — who must instead rely on derivative or proxy representation, if any.
% ABSENT_VOICES: Late-stage fetuses, ecosystems, non-human sentient animals, and future generations have no direct voice in the proceedings that determine their treatment; environmental advocates and disability-rights groups partially speak for excluded interests but hold no personhood-based standing to compel outcomes.
% DISAPPEARANCE_RATIONALE: If this specific reading were abandoned in favor of a sibling reading, reproductive law, environmental standing doctrine, and biomedical research regulation would all restructure substantially — abortion regulation would tighten under the developmental reading, and environmental/animal-welfare litigation would open new direct-standing avenues under the functional-capacity reading. The current legal architecture of multiple major fields depends on this boundary sitting where it currently sits.
% FOUNDING_PROBLEM: Common law and constitutional doctrine needed an administrable, enforceable definition of legal personhood that avoided both the practical unworkability of protecting all potential life at every developmental stage and the perceived overreach of extending rights-bearing status to non-human entities, while preserving bodily autonomy as a foundational liberal commitment.
% FOUNDING_PROBLEM_CORROBORATION: Supreme Court and appellate jurisprudence on born-alive rules and reproductive rights attest the doctrine as settled and functionally necessary. Disability-rights scholars, environmental-law academics, and fetal-personhood advocacy organizations — parties outside the beneficiary set — dispute both the coherence of the cognitive-capacity clause for disabled humans and the moral defensibility of excluding ecosystems and future generations, arguing the founding administrability rationale has calcified into an unexamined anthropocentric default.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.42) because the reading redistributes standing rather than material resources directly — its cost is borne by excluded classes (fetuses, ecosystems, future generations, and an ambiguous residual class of severely cognitively impaired humans) in the form of foreclosed legal remedies rather than extracted rents. Suppression (0.55) reflects the active doctrinal and enforcement work required to hold the born-human-plus-capacity line against two live contesting readings — courts must repeatedly relitigate edge cases (anencephaly, persistent vegetative states, late-term viability) to keep the boundary from drifting toward either sibling. Accessibility collapse (0.6) is moderate-high: once a jurisdiction adopts this reading, alternative standing theories for excluded classes become very difficult (though not impossible, given ongoing rights-of-nature litigation) to mount. Resistance (0.7) is substantial and organized: fetal-personhood movements, disability-rights advocates, and rights-of-nature litigants all actively contest the boundary from outside.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (pregnant persons, industry, researchers) this reading looks like coordination — a workable, liberty-preserving rule that avoids metaphysical overreach. From the payer seats (excluded classes and their advocates) the same rule looks like an arbitrary extraction of standing, engineered to protect incumbent interests (autonomy maximalism, industrial externalization) under cover of administrability. The engine computes both seat-types from the same structural data; the divergence is the intended measurement, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons, reproductive healthcare providers, extractive industry operators, and the biomedical research sector are beneficiaries because the boundary's placement directly expands their operative freedom (autonomy, clinical practice, resource development, research pipelines) by denying competing rights claims. Late-stage fetuses, environmental advocates, future generations, and the disabled/impaired-infant class sit on the payer side because the boundary's placement forecloses direct legal remedies or creates residual doctrinal instability that falls on them. The state reproductive policy bodies are the agenda-setter seat: they administer, litigate, and periodically re-draw the line's edge cases.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing an administrable personhood threshold — remains partially live (courts still need SOME workable line) but is contested as to whether THIS particular line remains the right one, given fifty years of biomedical, environmental, and disability-rights developments the original threshold did not anticipate. The founding_problem_status of 'contested' combined with a 'world_rearranges' disappearance verdict signals active dispute rather than settled capture — this is not a zombie mandate, but a live boundary fight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_boundary_reading_indeterminacy,
    'Is the born-human-plus-cognitive-capacity threshold a principled moral-philosophical boundary, or a historically contingent legal convenience that happens to serve incumbent interests (reproductive autonomy advocates, extractive industry, biomedical research)?',
    'Comparative jurisprudential analysis across jurisdictions adopting different readings, tracking whether outcomes track independent moral reasoning or track which interest groups had political power at the moment of doctrinal formation.',
    'If the boundary is primarily interest-driven rather than principled, the reading''s coordination-function claim weakens relative to its extraction-function, pushing the computed classification toward snare; if principled, tangled_rope or even a rope-like reading becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personhood_boundary_reading_indeterminacy, conceptual, 'Whether the anthropocentric threshold is principled or interest-contingent.').

omega_variable(
    cognitive_capacity_clause_disabled_humans,
    'Does the cognitive-capacity clause, as actually litigated, create genuine doctrinal risk for severely cognitively impaired born humans, or is it consistently read to include them via a status-based rather than functional-capacity test?',
    'Survey of guardianship, end-of-life, and disability-rights case law to determine whether courts apply a strict functional-capacity test (risking exclusion) or a categorical born-human status test (no risk) to this population.',
    'If courts apply a strict functional test, this reading''s harm to the disabled/impaired class is substantially larger than the current extraction score reflects, and disability-rights corroboration should carry more weight in future revisions of this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_clause_disabled_humans, empirical, 'Whether the capacity clause creates real or merely theoretical risk for disabled born humans.').

omega_variable(
    kernel_framing_alternative_axis,
    'Is the correct decomposition axis for this kernel ''which entities count'' (the three readings as authored) or an orthogonal axis about ''who decides the boundary'' (courts vs. legislatures vs. international bodies), which could recombine with any of the three substantive readings?',
    'Compare classification outcomes if the story were re-authored around institutional-authority axis instead of substantive-scope axis; check whether cs_pattern classification changes.',
    'If the authority axis produces a materially different cs_pattern than the substantive-scope axis, the framing choice itself is under-determined and a fourth axis of stories may be warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_axis, conceptual, 'Whether entity-scope or decision-authority is the more structurally correct decomposition axis for this kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__functional_capacity_reading).

% DUAL FORMULATION NOTE:
% This is one of three sibling stories decomposing the natural-language 'personhood boundary' concept per the epsilon-invariance principle. Each reading has a distinct epsilon, distinct victim set, and distinct classification: the restrictive_anthropocentric_reading (this file) computes moderate extraction concentrated on excluded non-human/pre-born/future-generation classes; the developmental_potentiality_reading would show near-zero extraction toward fetuses but substantial extraction toward pregnant persons' autonomy; the functional_capacity_reading would show a differently-shaped victim set entirely (non-sentient humans potentially excluded, sentient non-humans potentially included). All three are linked via affects_constraints because jurisdictional adoption of one reading directly changes the political and legal resource environment available to advocates of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
