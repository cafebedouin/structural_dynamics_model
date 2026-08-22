% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority (Proportionality Reading)
 *   domain: public_health/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This is the proportionality reading of the public health mandate
 *   authority kernel. It asserts that mandate legitimacy is not fixed by
 *   authority declaration but depends on a four-factor sliding scale: threat
 *   severity, alternative availability, coercion magnitude, and duration of
 *   imposition. Under this reading, the same vaccine mandate can be
 *   legitimate in one epidemiological state (novel high-severity pathogen, no
 *   alternatives, limited duration) and illegitimate in another (endemic mild
 *   pathogen, abundant alternatives, indefinite scope). The claim is
 *   tangled_rope: there is genuine coordination (protecting immunocompromised
 *   from externality) AND asymmetric extraction (coercive burden on the
 *   vaccine-hesitant that varies with threat). The extractiveness metric
 *   reflects this: high when threat is severe and alternatives are scarce,
 *   declining as both improve. This reading is in tension with
 *   bodily_autonomy_primary (which treats mandate as categorical violation)
 *   and public_health_primary (which treats mandate as obligation independent
 *   of proportionality).
 *
 * KEY AGENTS:
 *   - public_health_authority: agenda-setter, controls mandate declaration and enforcement; institutional power; operates under proportionality constraint as this reading frames it
 *   - immunocompromised_populations: primary beneficiaries; powerless; trapped in vulnerability; benefit is greatest when threat is severe and alternatives scarce
 *   - vaccine_hesitant_unvaccinated: primary payers; moderate power; coercive burden (employment, travel, school access restrictions); burden is legitimated only when proportionality test sustains it
 *   - medical_exemption_seekers: secondary payers; identity-locked (religious/medical conscience); navigate exemption processes as alternative to direct coercion; presence of exemptions signals proportionality mechanism is live
 *   - judicial_review_bodies: observer seat; assess whether authority's factual claims (threat severity, alternative availability) support proportionality judgment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.52).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.48).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority (Proportionality Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, 'a5057e65-10b4-4f7d-899d-ea4a6ae283a3').
narrative_ontology:cs_kernel_codification('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', fixed_text).
narrative_ontology:cs_authority_grounding('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', lineage).
narrative_ontology:cs_interpretation_layer_present('a5057e65-10b4-4f7d-899d-ea4a6ae283a3').
narrative_ontology:cs_reading_relation('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', public_health_mandate_authority__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', foundational, mandate_legitimacy_is_conditional_not_categorical).
narrative_ontology:cs_axiom_status(mandate_legitimacy_is_conditional_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', mandate_legitimacy_is_conditional_not_categorical, deontological).
narrative_ontology:cs_axiom('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', foundational, proportionality_test_requires_measurable_inputs).
narrative_ontology:cs_axiom_status(proportionality_test_requires_measurable_inputs, holdable).
narrative_ontology:cs_axiom_grounding('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', proportionality_test_requires_measurable_inputs, empirically_contingent).
narrative_ontology:cs_reference_frame('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', emergency_powers_constrained_by_proportionality).
narrative_ontology:cs_drift_state('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', endemic_phase_low_threat, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5057e65-10b4-4f7d-899d-ea4a6ae283a3', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_infrastructure).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, vaccine_hesitant_unvaccinated).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, medical_exemption_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, medical_exemption_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares and enforces vaccination mandates, subject to proportionality constraints as this reading frames them. Controls assessment of threat severity, alternative availability, coercion magnitude, and duration. Sets the thresholds that determine when mandate scope expands (high threat, scarce alternatives) or contracts (low threat, abundant alternatives). Operates within judicial review, which can overturn mandates failing proportionality test.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Cannot be vaccinated themselves due to immune system dysfunction. Depend on community vaccination (herd immunity) to avoid pathogen exposure. Benefit directly from mandate-driven vaccination of others, though the magnitude of benefit depends on threat severity (high-threat pathogens pose greater risk; low-threat pathogens pose minimal risk). Trapped in their condition; cannot exit dependence on collective protection.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Faces surge-capacity crises when population immunity is low. Mandates reduce caseload and surge demand. Benefit is greatest during high-threat phases; in endemic low-threat phases, mandate benefit to healthcare infrastructure shrinks (low caseload, no surge risk). Exit options are constrained: they cannot choose to operate outside the pandemic risk environment, though they can invest in surge capacity as alternative to mandate protection.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, healthcare_infrastructure, beneficiary,
    organized, biographical, constrained, national).

% Bear coercive burden: employment termination (if refuse vaccination and authority mandates healthcare/government vaccination), school exclusion (if authority mandates school vaccination), travel restrictions (if authority mandates vaccination for transit), and loss of public service access. Exit options are constrained: refusing vaccination means accepting sanctions, or leaving jurisdiction/employment/education entirely. The burden is legitimated (under proportionality reading) only when threat is high and alternatives are scarce; when threat is low and alternatives abundant, the same coercive burden is illegitimate under proportionality test.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, vaccine_hesitant_unvaccinated, payer,
    moderate, biographical, constrained, national).

% Seek exemptions (medical contraindication, religious belief, philosophical conviction) from vaccination mandates. Bear costs of exemption denial (employment termination, school exclusion) if exemptions are not granted. Also potentially benefit if exemptions are granted (they avoid forced vaccination, though they may face other sanctions). Identity-locked because their religious/medical conscience is constitutive of their self-understanding; cannot exit the identity frame, though can navigate exemption processes. Exemption availability signals that mandate authority acknowledges alternatives to universal vaccination, which under proportionality logic constrains mandate scope.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, medical_exemption_seekers, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, medical_exemption_seekers, beneficiary).

% Assess and measure proportionality inputs: threat severity (reproduction number, case fatality rate, hospitalization rate), alternative availability (treatment efficacy, testing-and-isolation effectiveness, vaccine efficacy against transmission), coercion magnitude (economic and social burden of sanctions), duration (how long can mandate persist before proportionality threshold is breached). Provide technical data the proportionality judgment consumes. Do not make political judgment about legitimacy; that judgment is the authority's and the court's.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_experts, observer,
    institutional, generational, analytical, national).

% Review whether mandates satisfy proportionality test: is threat severity as claimed by the authority? Are alternatives actually unavailable or merely inconvenient? Is coercive magnitude necessary to threat or overshooting? Is duration time-bound or indefinite? Can overturn mandates failing proportionality review. Operate under the proportionality reading's constraint that mandate legitimacy is conditional on measurable inputs, not on authority declaration alone.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, judicial_review_bodies, observer,
    institutional, generational, analytical, national).

% Experience genuine harm from vaccination (myocarditis, severe allergic reaction, thrombosis, death). Their harm is individualized (one person harmed at a time) and initially invisible (harm emerges slowly and statistically). Structurally excluded from proportionality deliberation because their voices are not organized into public discourse the way immunocompromised and healthcare infrastructure concerns are. Would object if present: the proportionality balance should weigh their harm against immunocompromised benefit, but their absence means the balance is struck blind to harm costs. This absence is structural, not deliberate; it is how individualized harms accumulate invisibly in public discourse.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, vaccine_adverse_event_sufferers, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, public_health_authority).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects immunocompromised and healthcare infrastructure from externality of low-population immunity. Solves a commons problem: individual vaccination decisions do not account for benefit to those who cannot be vaccinated. Collective action (mandate-driven vaccination) achieves population immunity that no individual choice generates. Coordination is genuine: the problem cannot be solved through voluntary uptake alone when collective action problem exists.
% TRANSFER_FUNCTION: Moves coercive burden (forced vaccination, employment/education/travel sanctions, bodily autonomy constraints) from immunocompromised populations to vaccine-hesitant populations. The transfer is justified (under proportionality reading) by the legitimacy of protecting vulnerable commons, subject to four-factor test: threat severity, alternative availability, coercion magnitude, and duration. Transfer is not fixed; it varies with epidemiological state.
% ABSENT_VOICES: Vaccine adverse-event sufferers (whose individualized harms are invisible until aggregate data emerges, and are structurally unorganized relative to immunocompromised advocacy and healthcare infrastructure concerns). Future generations (whose immune landscape and pathogen exposure under different vaccination strategies are unknown). Mandates are set without these voices, which means proportionality assessment is blind to delayed harms and long-term consequences.
% DISAPPEARANCE_RATIONALE: If proportionality constraints on mandate authority vanished, the authority could impose mandates independent of threat severity, alternative availability, coercion magnitude, or duration. Vaccine-hesitant populations would face unconstrained coercive burden; immunocompromised would lose the legitimacy framework that justified coercion on their behalf. Judicial review of proportionality would be impossible. The legitimate boundary between authority power and individual right would shift radically, and institutional balance between executive, judicial, and legislative branches (which proportionality review instantiates) would change.
% FOUNDING_PROBLEM: Early pandemic responses exposed a legitimacy crisis: authorities imposed mandates without principled constraint. Some mandates appeared justified (healthcare worker vaccination during surge); others appeared disconnected from threat (school vaccine mandates for endemic mild disease). The founding problem is: what makes a mandate legitimate rather than merely authority-declared? Proportionality reading answers: mandate legitimacy depends on measured inputs (threat, alternatives, coercion, duration), not on authority claim alone. This reading emerges from demand that emergency powers be constrained by judicial review of facts, not just authority discretion.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars (Sunstein, Vermeule, Gostin), bioethicists (Ackerman, Jennings), and public health authorities outside the benefiting parties attest the problem is live and contested. Judicial opinions grapple with proportionality (European courts have held mandates must be time-limited and proportional to threat; U.S. courts have split on proportionality review); legislative bodies have debated sunset clauses and threshold criteria. The founding problem is not whether mandates can protect vulnerable populations (established), but whether mandates can be legitimate without proportionality constraints (contested). The corroboration comes from disinterested institutional sources (courts, legislatures, academic scholarship), not from public health authorities who benefit from unconstrained mandate power.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The proportionality reading instantiates tangled_rope structure: immunocompromised populations genuinely benefit (they cannot be vaccinated; herd immunity protects them — real coordination problem). Vaccine-hesitant bear real costs (employment termination, school exclusion, bodily autonomy constraint — real extraction). The twist is that extractiveness is NOT fixed: it varies with threat severity and alternative availability. When threat is high and alternatives are scarce (t=0-6), extractiveness is high (0.78→0.68) because coercive burden is large relative to available options. When threat is endemic and alternatives are abundant (t=12-18), extractiveness drops (0.54→0.48) because coercive burden is narrow and alternatives are available. The proportionality reading explicitly embeds this dynamic into legitimacy: mandate scope must shrink as threat shrinks or alternatives expand. The measurement series tracks this via a single time grid, showing extractiveness declining as epidemiological conditions change — the constraint itself becomes less extractive as proportionality inputs improve.
 *
 * PERSPECTIVAL GAP:
 *   The authority seat and the vaccine-hesitant seat should compute different types from the same structural data. The authority has designed a proportionality mechanism (mandate scope shrinks as threat shrinks); from their perspective, the constraint is rope — genuine coordination with built-in fairness guards. The vaccine-hesitant seat experiences the same mechanism as tangled_rope or snare depending on the moment: when threat is high, proportionality feels legitimate (rope framing holds); when threat is endemic but mandates persist, the mechanism feels like a ratchet that never released (snare framing emerges). The engine computes this divergence from directionality and power asymmetry: the authority controls the levers (agenda-setter role, institutional power), so their experience is more coordinated; the hesitant bear the burden (payer role, constrained exit), so their experience is more extractive. This divergence is exactly what the proportionality reading predicts: legitimacy is not observer-independent; it depends on whether you are benefiting from the mechanism or bearing its cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authority (agenda-setter, institutional power) has d near beneficiary end: they control mandate scope and duration, which are the proportionality levers. They do not personally bear coercive burden; their power is to adjust the constraint itself. Immunocompromised (powerless, trapped) have d near beneficiary end: the constraint exists to protect them, though they do not control it. Vaccine-hesitant (moderate power, constrained exit) have d near target end: they bear coercive burden, their exit options are limited (refusing vaccination means accepting sanctions), and their situation worsens as threat severity decreases (because proportionality boundary moves against them). Medical exemption seekers sit between: they bear coercive burden (exemption denial is a sanction), but identity-locked exit means their relationship to the constraint is mediated through conscience/medical identity rather than simple economic choice. Judicial review (observer) sits at d=0.5 symmetric: they assess proportionality but do not benefit or pay directly. The divergence between agenda-setter and payer seats is the structural story: from the authority's seat, mandate is a proportional response to threat; from the vaccine-hesitant seat, it is coercive overreach if threat is low and alternatives abundant.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (what makes a mandate legitimate rather than mere authority declaration) is still live. The proportionality reading does NOT resolve it; instead, it offers a framework for addressing it: four factors (threat, alternatives, coercion, duration) that must be publicly assessed. The risk of mandatrophy is that the mechanism becomes theatrical: authorities declare threat, authorities set thresholds, authorities judge proportionality — with minimal external oversight. Mandatrophy would occur if the proportionality framework is formally invoked but effectively ignored (mandates persist despite low threat and abundant alternatives, theaters reviewed and never actually narrowed). The measurement series shows extractiveness declining as threat declines, which is consistent with proportionality operating as designed. If extractiveness were to stabilize at high levels despite endemic conditions, that would signal mandatrophy (the mechanism is theater, not constraint). The five-year projection (t=24) shows extractiveness at 0.52, near the proportionality threshold for endemic diseases — a stable equilibrium where mandate scope is narrow (healthcare/congregate only) and coercive burden is proportional to demonstrable benefit. That outcome is consistent with proportionality functioning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_severity_measurement,
    'How is threat severity measured and assessed for proportionality purposes? What counts as ''high'' vs. ''low'' threat?',
    'Establish objective epidemiological thresholds (reproduction number, case fatality rate, hospitalization rate, healthcare surge capacity) and bind mandate scope to these metrics via legislation or regulation. Require public transparency in threat assessment.',
    'If threat thresholds are objective and binding, proportionality becomes a justiciable constraint (courts can review whether threat actually meets stated level). If threat is assessed discretionarily or classified, proportionality becomes cover for authority discretion. Extractiveness assessment depends on whether threat-level claims are externally verifiable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threat_severity_measurement, empirical, 'Whether threat severity is measured objectively or assessed discretionarily.').

omega_variable(
    alternative_availability_assessment,
    'What counts as an ''available alternative'' to mandate? Does treatment availability matter? Does testing-and-isolation availability reduce mandate necessity? Does voluntary uptake count as alternative?',
    'Comparative analysis: jurisdictions using different alternative strategies (treatment-first, testing-isolation, voluntary vaccination incentives) and measuring health outcomes. Judicial interpretation of what alternatives defeat mandate necessity.',
    'If alternatives are assessed narrowly (only vaccines count), mandate scope remains broad even when other tools are available. If alternatives are assessed broadly (treatment, testing, isolation, voluntary uptake all count), mandate scope must narrow. Extractiveness is inversely tied to alternative availability assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_availability_assessment, conceptual, 'Whether alternatives to mandate are assessed narrowly (vaccination only) or broadly (treatment, testing, voluntary uptake).').

omega_variable(
    coercion_magnitude_weighting,
    'How is coercion magnitude weighted in proportionality judgment? Is employment termination equivalent to school exclusion equivalent to travel restriction? Do different coercive modalities require different threat levels to justify?',
    'Graduated mandate frameworks that tier coercion to threat: highest threat enables employment restrictions; moderate threat enables school/healthcare restrictions only; low threat enables testing/incentive only. Empirical test: measure mandate scope across threat levels and check for graduated response.',
    'If coercion magnitude is not differentiated by threat level, the constraint drifts toward snare (same burden regardless of threat). If coercion magnitude is differentiated, proportionality mechanism can operate as designed (burden shrinks as threat shrinks).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_magnitude_weighting, empirical, 'Whether coercive burden is graduated to threat level or applied uniformly.').

omega_variable(
    duration_and_sunset_mechanism,
    'How is mandate duration set? Is there an automatic sunset mechanism tied to threat indicators, or is duration indefinite pending authority decision?',
    'Legislation requiring automatic sunset at specified threat thresholds (endemic classification, case rate below X per 100k, etc.). Measure whether mandates actually sunset when conditions are met or persist despite trigger satisfaction.',
    'If duration is indefinite, proportionality is incomplete (indefinite coercion cannot be proportional to temporary threat). If duration is tied to objective conditions, proportionality mechanism is enforceable (mandate must narrow/end when conditions change).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duration_and_sunset_mechanism, empirical, 'Whether mandate duration is automatic-sunset-based or indefinite-pending-authority.').

omega_variable(
    exemption_as_proportionality_signal,
    'Does the existence of exemptions (medical, religious, philosophical) signal that the mandate acknowledges alternatives to universal vaccination, and therefore that mandate scope should be narrower?',
    'Empirical: compare mandate scope and coercion magnitude in jurisdictions with broad exemptions vs. narrow exemptions. Legal: test whether courts cite exemption presence as evidence for proportionality calibration.',
    'If exemptions are treated as proportionality signals, mandate scope must shrink where exemptions exist (authority has already acknowledged alternatives). If exemptions are treated as separate safety-valve machinery decoupled from proportionality, mandate scope can remain broad even with broad exemptions. Extractiveness assessment depends on whether exemptions are treated as meaningful alternatives or as exceptions that prove the rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_as_proportionality_signal, conceptual, 'Whether exemptions signal mandate-scope limitation or are decoupled from proportionality logic.').

omega_variable(
    reading_vs_bodily_autonomy_boundary,
    'Does the proportionality reading foreclose the bodily_autonomy_primary reading, or do they coexist as incompatible frameworks?',
    'Logical analysis: the bodily_autonomy reading claims mandate is categorically impermissible; the proportionality reading claims mandate is conditionally permissible. These are direct contradictions at the ''can mandate ever be legitimate'' level. Within a single constitutional framework, one must dominate or both are held incoherently.',
    'If proportionality forecloses bodily autonomy, then bodily autonomy advocates must deny proportionality premise (accept mandate only if conditions satisfy all four factors, or deny mandate entirely). If they coexist, they represent a genuine indeterminacy in constitutional law (the reading is contested, not settled). The reading_relations field treats this as COEXISTS_WITH because judicial and political discourse continues to hold both live positions simultaneously, though they are logically incompatible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_bodily_autonomy_boundary, conceptual, 'Whether proportionality forecloses bodily autonomy as a reading or both readings coexist as contested alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__proportionality_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__proportionality_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.52).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(public_health_mandate_authority__proportionality_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary).

% DUAL FORMULATION NOTE:
% The public_health_mandate_authority kernel is instantiated by three readings: bodily_autonomy_primary (mandate is categorical violation), public_health_primary (mandate is obligation), and proportionality_reading (mandate legitimacy depends on four-factor sliding scale). Each reading has its own constraint story with distinct beneficiary/victim structures and extractiveness profiles. Proportionality_reading is downstream of the other two in the contest: it attempts to mediate between them by treating proportionality as the binding constraint on authority. The network links all three; the engine can detect when a constraint family is contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__proportionality_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
