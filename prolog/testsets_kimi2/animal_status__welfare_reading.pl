% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Welfare Reading of Animal Status: Sentient but Usable
 *   domain: applied ethics / legal philosophy / political economy
 *
 * SUMMARY:
 *   This constraint story models the welfare reading of the animal_status
 *   kernel: the standing legal and social arrangement that treats animals as
 *   sentient beings whose interests constrain but do not prohibit human
 *   instrumental use. The reading is instantiated in welfare statutes that
 *   prohibit gratuitous harm while exempting standard agricultural, research,
 *   and entertainment practices. It is structurally distinct from the
 *   abolitionist reading (which treats sentience as generating rights against
 *   all use) and the property reading (which denies independent moral
 *   standing). The authored metrics describe the actual operation of this
 *   arrangement: moderate-high extractiveness driven by exemption structures,
 *   active enforcement of the boundary between permissible and impermissible
 *   harm, and a widening gap between the framework's compassionate reference
 *   frame and industrial practice.
 *
 * KEY AGENTS:
 *   - Welfare regulatory apparatus (agenda_setter, institutional, national) â sets and enforces the boundary between prohibited cruelty and exempted use.
 *   - Animal-using industries (beneficiary, powerful, global) â capture value through statutory exemptions.
 *   - Animals in exempt sectors (payer, powerless, local/trapped) â bear the physical costs of the arrangement.
 *   - Abolitionist advocates (excluded, moderate) â reject the framework's premises but are outside its operative window.
 *   - Veterinary ethicists (observer, analytical) â document the gap between welfare promise and practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.55).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Welfare Reading of Animal Status: Sentient but Usable").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied ethics / legal philosophy / political economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, '51cca387-6857-45c0-99c6-ffa33127bb4d').
narrative_ontology:cs_kernel_codification('51cca387-6857-45c0-99c6-ffa33127bb4d', formalized).
narrative_ontology:cs_authority_grounding('51cca387-6857-45c0-99c6-ffa33127bb4d', lineage).
narrative_ontology:cs_interpretation_layer_present('51cca387-6857-45c0-99c6-ffa33127bb4d').
narrative_ontology:cs_reading_relation('51cca387-6857-45c0-99c6-ffa33127bb4d', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('51cca387-6857-45c0-99c6-ffa33127bb4d', animal_status__property_reading, influences).
narrative_ontology:cs_axiom('51cca387-6857-45c0-99c6-ffa33127bb4d', foundational, sentience_generates_welfare_duties_not_rights).
narrative_ontology:cs_axiom_status(sentience_generates_welfare_duties_not_rights, holdable).
narrative_ontology:cs_axiom_grounding('51cca387-6857-45c0-99c6-ffa33127bb4d', sentience_generates_welfare_duties_not_rights, deontological).
narrative_ontology:cs_axiom('51cca387-6857-45c0-99c6-ffa33127bb4d', foundational, instrumental_use_permissible_under_welfare_review).
narrative_ontology:cs_axiom_status(instrumental_use_permissible_under_welfare_review, holdable).
narrative_ontology:cs_axiom_grounding('51cca387-6857-45c0-99c6-ffa33127bb4d', instrumental_use_permissible_under_welfare_review, instrumental).
narrative_ontology:cs_reference_frame('51cca387-6857-45c0-99c6-ffa33127bb4d', regulated_sentient_use_framework).
narrative_ontology:cs_drift_state('51cca387-6857-45c0-99c6-ffa33127bb4d', contemporary_industrial_agriculture_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('51cca387-6857-45c0-99c6-ffa33127bb4d', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_using_industries).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals_in_exempt_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and enforces statutes that prohibit gratuitous cruelty to animals while exempting standard practices in agriculture, research, and entertainment; inspects facilities and prosecutes egregious abuse, but leaves the bulk of instrumental use legally sheltered.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, welfare_regulatory_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Operate farms, laboratories, and processing facilities under welfare statutes that permit routine confinement, surgical alterations without anesthesia in exempt categories, and slaughter; comply nominally with welfare standards while relying on statutory exemptions for core revenue practices.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_using_industries, beneficiary,
    powerful, biographical, constrained, global).

% Born into breeding programs for food, research, or entertainment; confined, altered, and killed under practices explicitly exempted from anti-cruelty provisions; cannot exit, contest categorization, or access legal standing.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals_in_exempt_sectors, payer,
    powerless, immediate, trapped, local).

% Argue that sentience precludes all property status and instrumental use; their policy proposals lie outside the operative welfare framework and are treated as legally unreachable or politically extreme.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, national).

% Assess the divergence between statutory welfare standards and actual industry practice; publish on captive-bolt failure rates, cage-free transition costs, and enforcement gaps without direct authority to alter exemptions.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, veterinary_ethicists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a baseline prohibition against gratuitous animal cruelty and coordinates human-animal interaction through legally enforceable welfare standards, permitting regulated instrumental use.
% TRANSFER_FUNCTION: Moves life-years, labor, and biological products from animals to human industries and consumers, with nominal welfare constraints as the condition of transfer.
% ABSENT_VOICES: Abolitionist advocates who reject all instrumental use, and the animals themselves who are structurally incapable of contesting their categorization within exempt sectors.
% DISAPPEARANCE_RATIONALE: The arrangement underpins global food systems, biomedical research pipelines, and entertainment economies; its disappearance would trigger immediate legal reclassification of animals and force restructuring of these sectors.
% FOUNDING_PROBLEM: Unregulated public and private cruelty to animals in pre-industrial and early industrial contexts, lacking any legal mechanism to account for animal suffering.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians attest the founding anti-cruelty problem was genuine; abolitionist scholars and independent animal ethicists attest the framework now primarily legitimates industrial extraction. Industry beneficiaries assert the problem remains live (risk of regression), but this is self-interested.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the massive scale of animal use legally sheltered by welfare exemptions, not the small volume of prosecuted gratuitous cruelty. Suppression (0.55) captures the active legal and economic suppression of abolitionist alternatives and the statutory silence of animals. Theater ratio (0.40) registers the growing performative gap between welfare marketing (cage-free, humane labels) and baseline exempt practices that remain unchanged. Accessibility collapse (0.60) reflects how the welfare framework crowds out radical alternatives by presenting itself as the only humane option. Resistance (0.45) comes from abolitionist movements and sporadic regulatory pressure. Measurements track the industrial intensification of the post-1970 era.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (welfare regulators) experiences the constraint as a progressive coordination mechanism preventing cruelty; the payer seat (animals in exempt sectors) experiences it as the structural condition of their confinement and use. The excluded seat (abolitionist advocates) sees the constraint as a legitimating ideology for extraction. These divergences are structurally determined by power and exit asymmetry, not by evaluative disagreement alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal-using industries are structural beneficiaries (low d): they profit from the exemption structure and could not maintain current scale without it. Animals in exempt sectors are structural targets (high d): they bear the physical costs, have zero exit, and their scope is local and immediate. Abolitionist advocates are excluded from the coordination, not beneficiaries. No override is needed because the structural derivation from beneficiary/victim declarations and exit options is accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problemâunregulated crueltyâhas been partially solved, but the arrangement has not atrophied; instead, it has expanded to legitimate industrial practices far beyond its original scope. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) flags the constraint as a candidate for mandate drift: it persists because the world is organized around it, not because its original problem remains live in the same form. This prevents misclassifying it as a pure coordination mechanism (rope) by showing that the coordination story now serves extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victim_status_ideological_exclusion,
    'Does the welfare reading''s exclusion of animals in instrumental use from the victim set reflect a genuine structural non-victimization, or an ideological move that obscures extraction?',
    'Comparative ethological and economic audit: if animals in exempt sectors show chronic stress, injury, and premature death rates comparable to gratuitous-harm cases, their exclusion from victim status is ideological.',
    'If ideological, the constraint''s victim set is artificially narrow and effective extraction is higher than the reading admits; this pushes classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_ideological_exclusion, conceptual, 'Ideological vs structural victim exclusion in instrumental use').

omega_variable(
    exemption_structural_function,
    'Are statutory exemptions (e.g., for standard agricultural practices) structurally necessary for the coordination function, or do they constitute extractive overhead captured by industries?',
    'Cross-jurisdictional comparison of production costs and welfare outcomes in jurisdictions with narrower exemptions versus those with broad exemptions.',
    'If narrow exemptions do not collapse the industries, the broad exemptions are extractive; the coordination function is separable from the extraction, clarifying the tangled-rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_structural_function, empirical, 'Necessity of statutory exemptions for coordination').

omega_variable(
    kernel_reading_stability,
    'Can the welfare reading maintain coherence as industrial practice drifts further from its compassionate-use reference frame, or does severe practice drift convert it into a piton?',
    'Trajectory of theater_ratio and extraction metrics: if theater rises while functional welfare stagnates, the reading is becoming performative.',
    'If the gap widens unacknowledged, the constraint may be reclassified as piton or snare in later temporal readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_stability, empirical, 'Temporal coherence of the welfare reading under practice drift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anim_tr_t10, animal_status__welfare_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(anim_tr_t20, animal_status__welfare_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(anim_tr_t30, animal_status__welfare_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(anim_tr_t50, animal_status__welfare_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anim_be_t10, animal_status__welfare_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(anim_be_t20, animal_status__welfare_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(anim_be_t30, animal_status__welfare_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(anim_be_t50, animal_status__welfare_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(anim_su_t10, animal_status__welfare_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(anim_su_t20, animal_status__welfare_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(anim_su_t30, animal_status__welfare_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(anim_su_t50, animal_status__welfare_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three structurally distinct constraints because the colloquial label 'animal status' conflates three different referents: a rights-based prohibition (abolitionist), a welfare-regulation regime (welfare), and a pure property framework (property). Each has a different epsilon, different victim sets, and different coordination/extraction profiles. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
