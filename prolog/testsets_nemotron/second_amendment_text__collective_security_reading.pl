% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment — Collective Security Reading
 *   domain: constitutional_law/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the collective-security reading of the
 *   Second Amendment: the militia clause ('A well regulated Militia, being
 *   necessary to the security of a free State') operates as a condition on
 *   the right, authorizing the state to regulate arms possession and carriage
 *   to serve collective security. The state regulatory apparatus
 *   (legislatures, agencies, courts) is the primary beneficiary, gaining
 *   expansive regulatory authority. Individual gun owners, instructors, and
 *   competitive shooters bear compliance costs and restrictions. The reading
 *   has gained regulatory ground since 1934 (NFA) through 1968 (GCA), 1994
 *   (AWB), and post-Bruen (2022) regulatory responses. The claimed type is
 *   tangled_rope: the constraint performs genuine coordination (preventing
 *   dangerous persons from easily accessing firearms, standardizing
 *   background checks) while simultaneously extracting compliance burdens
 *   from a defined class whose exit is constrained.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.42).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.35).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment — Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'aef35822-4241-4a4e-bb21-97f00765fb25').
narrative_ontology:cs_kernel_codification('aef35822-4241-4a4e-bb21-97f00765fb25', fixed_text).
narrative_ontology:cs_authority_grounding('aef35822-4241-4a4e-bb21-97f00765fb25', lineage).
narrative_ontology:cs_interpretation_layer_present('aef35822-4241-4a4e-bb21-97f00765fb25').
narrative_ontology:cs_reading_relation('aef35822-4241-4a4e-bb21-97f00765fb25', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('aef35822-4241-4a4e-bb21-97f00765fb25', second_amendment_text__originalist_civic_virtue_reading, forecloses).
narrative_ontology:cs_axiom('aef35822-4241-4a4e-bb21-97f00765fb25', foundational, militia_clause_conditions_right).
narrative_ontology:cs_axiom_status(militia_clause_conditions_right, holdable).
narrative_ontology:cs_axiom_grounding('aef35822-4241-4a4e-bb21-97f00765fb25', militia_clause_conditions_right, conventional).
narrative_ontology:cs_axiom('aef35822-4241-4a4e-bb21-97f00765fb25', foundational, state_police_power_includes_arms_regulation).
narrative_ontology:cs_axiom_status(state_police_power_includes_arms_regulation, holdable).
narrative_ontology:cs_axiom_grounding('aef35822-4241-4a4e-bb21-97f00765fb25', state_police_power_includes_arms_regulation, conventional).
narrative_ontology:cs_axiom('aef35822-4241-4a4e-bb21-97f00765fb25', secondary, individual_self_defense_not_core_protected_activity).
narrative_ontology:cs_axiom_status(individual_self_defense_not_core_protected_activity, holdable).
narrative_ontology:cs_axiom_grounding('aef35822-4241-4a4e-bb21-97f00765fb25', individual_self_defense_not_core_protected_activity, conventional).
narrative_ontology:cs_reference_frame('aef35822-4241-4a4e-bb21-97f00765fb25', founding_era_militia_necessity).
narrative_ontology:cs_drift_state('aef35822-4241-4a4e-bb21-97f00765fb25', post_bruen_regulatory_response, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aef35822-4241-4a4e-bb21-97f00765fb25', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, public_safety_advocacy_groups).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, firearms_instructors).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, competitive_shooters).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, collective_security_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, state_police_power_primacy).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, militia_clause_conditionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers licensing regimes, background check systems, and prohibited-person categories. Justifies regulation as necessary for collective security and preventing dangerous persons from accessing firearms. Collects permit fees and exercises rulemaking authority over the commercial firearms market. Can modify regulatory stringency within judicial review boundaries.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives regulatory tools (trace data, prohibited possessor databases, carry permit records) that aid criminal investigations. Benefits from reduced illegal firearms flow through universal background checks and dealer licensing. Their operational capacity expands with each regulatory layer, but they bear enforcement costs and political blowback when regulations are unpopular.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, law_enforcement_agencies, beneficiary,
    institutional, biographical, constrained, national).

% Leverages the collective-security framing to advance legislative agendas (universal background checks, assault weapon bans, red flag laws). Gains fundraising capacity and policy influence from each regulatory victory. Their organizational survival depends on the constraint's regulatory momentum continuing; if the reading collapses, their core rationale dissolves.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, public_safety_advocacy_groups, beneficiary,
    organized, biographical, mobile, national).

% Bear compliance costs (permit fees, training mandates, transfer delays, prohibited feature restrictions) and face criminal liability for technical violations. Their exit options are constrained: they can relocate to friendlier jurisdictions (constrained exit) or disengage from the right (identity_locked for many). The constraint extracts time, money, and legal risk from this class.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    organized, biographical, constrained, national).

% Face expanded mandatory training curricula, instructor certification requirements, and range regulation that increase overhead and reduce margins. The regulatory apparatus benefits from their compliance labor (they deliver the mandated training). Exit requires career change or relocation — constrained by professional licensure and local market conditions.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, firearms_instructors, payer,
    moderate, biographical, constrained, regional).

% Subject to magazine limits, feature bans, and transport restrictions that directly impair their sport. The collective-security reading treats competitive shooting as incidental to the militia purpose, not protected. Their exit is constrained by the national patchwork of laws — they can travel to competitions but must navigate contradictory regulatory regimes.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, competitive_shooters, payer,
    moderate, biographical, constrained, national).

% Categories of persons (felons, domestic violence misdemeanants, certain mental health adjudications) categorically barred from firearms access. The collective-security reading expands these categories over time (e.g., non-violent felons, temporary restraining order subjects). They have no voice in the regulatory process and no legal exit from the prohibition.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, prohibited_persons_categories, excluded,
    powerless, biographical, trapped, national).

% Analyze the historical record to argue the militia clause was not a condition but a declaratory statement. They do not bear the constraint's costs nor collect its benefits. Their work influences judicial interpretation but they are not parties to the regulatory arrangement.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, constitutional_scholars_originalist, observer,
    analytical, civilizational, analytical, universal).

% Analyze the text through evolving standards of public safety and democratic governance. They provide intellectual scaffolding for the collective-security reading but are not regulated by it. Their professional standing correlates with the reading's judicial success.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, constitutional_scholars_living_constitutionalist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional framework for balancing individual arms possession against collective security needs, enabling the state to regulate dangerous weapons and dangerous possessors while preserving a militia-related right.
% TRANSFER_FUNCTION: Moves regulatory authority and compliance costs from individual gun owners to the state regulatory apparatus. The state gains rulemaking power, enforcement tools, and data; gun owners lose unfettered access and bear financial and legal burdens.
% ABSENT_VOICES: Prohibited persons categories (especially non-violent felons and temporary restraining order subjects) are structurally excluded from the regulatory conversation. Rural communities for whom firearms are daily tools rather than political symbols are often absent from urban-centric policy debates. Future generations who will inherit the regulatory architecture have no voice.
% DISAPPEARANCE_RATIONALE: If the collective-security reading vanished overnight, the constitutional basis for federal and state licensing regimes, universal background checks, assault weapon bans, and red flag laws would collapse. The regulatory apparatus would lose its primary constitutional justification, triggering immediate litigation and legislative vacuum. The firearms market would reorganize around the individual-right reading's constraints.
% FOUNDING_PROBLEM: The Founding generation feared a standing army and wanted to ensure the militia — the body of armed citizens — could not be disarmed by the federal government. The collective-security reading reinterprets this as authorizing government regulation to ensure the militia is 'well-regulated' in the modern sense: trained, vetted, and equipped for collective defense rather than private violence.
% FOUNDING_PROBLEM_CORROBORATION: Gun control advocacy organizations and progressive legal scholars attest the founding problem is live (modern threats require regulation). Originalist scholars and gun rights organizations attest it is dead (the militia purpose was a prefatory clause, not a limitation). The Heller majority (Scalia) explicitly rejected the collective-security reading as the sole meaning, but the four dissenters endorsed it. No consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).
:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the regulatory apparatus gains substantial authority and resources while the regulated class bears real but not catastrophic costs — the right persists in attenuated form. Suppression (0.35) is moderate: alternatives exist (moving states, using permitted firearms, complying) but the regulatory net is broad and deepening. Theater ratio (0.28) captures that much regulatory activity (enhanced background checks, red flag laws) has genuine public safety rationale, but performative measures (feature bans on rarely-used configurations, registration of lawful owners) are increasing. Accessibility collapse (0.58) is moderate: the collective-security reading has become the dominant framework in blue states and federal courts pre-Bruen, but the individual-right reading remains legally live and politically potent. Resistance (0.48) reflects sustained litigation, non-compliance in some jurisdictions, and political mobilization by gun rights organizations.
 *
 * DIRECTIONALITY LOGIC:
 *   The state regulatory apparatus sits at d ≈ 0.1 (full beneficiary): it writes the rules, collects fees, and expands its authority. Law enforcement and advocacy groups sit at d ≈ 0.2–0.3 (beneficiaries with some enforcement costs). Individual gun owners sit at d ≈ 0.8 (targets): they pay the transfer, face criminal liability, and have constrained exit. Instructors and competitive shooters sit at d ≈ 0.75–0.85 (targets with specialized burdens). Prohibited persons sit at d ≈ 1.0 (fully trapped, no voice). Observers sit at d = 0.5 (analytical). The constraint is structurally asymmetrical: the agenda-setters are also the primary beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing federal disarmament of the militia) has substantially transformed. The modern regulatory state uses the collective-security reading to justify regulations the Founders could not have imagined (universal background checks, assault weapon definitions, red flag laws). The mandate has not atrophied — it has expanded. The constraint is not a piton because the regulatory apparatus actively maintains and extends it; it is not a scaffold because no sunset is contemplated. The tangled_rope classification captures the dual nature: genuine coordination around preventing prohibited persons from accessing firearms coexists with extraction from lawful owners through expanding regulatory burdens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_conditionality_scope,
    'Does the militia clause condition the right only on organized, government-supervised militia service, or does it protect an individual right that serves the militia purpose indirectly?',
    'Supreme Court precedent (Heller, McDonald, Bruen) has ruled the right is individual and not conditioned on militia service. However, the collective-security reading persists in dissenting opinions, state court decisions, and legislative findings. Resolution requires either a definitive Court majority rejecting all collective-security framing or a constitutional amendment.',
    'If the militia clause is a condition, the state''s regulatory power is near-plenary (ε → 0.7+). If it is merely prefatory, the individual right constrains regulation (ε → 0.2–0.3). The classification shifts from tangled_rope toward snare or rope depending on the answer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militia_clause_conditionality_scope, conceptual, 'Whether the militia clause operates as a condition on the right or a declaratory statement of purpose.').

omega_variable(
    regulatory_benefit_measurement,
    'How much of the observed reduction in firearms homicide is attributable to regulations justified by the collective-security reading, versus demographic, economic, or policing factors?',
    'Natural experiments from state policy variation (e.g., Missouri''s permit-to-purchase repeal, Connecticut''s adoption, post-Bruen carry law changes) combined with synthetic control methods.',
    'If regulations produce substantial measurable safety benefits, the coordination function is real and the constraint''s extraction is partially justified (tangled_rope confirmed). If benefits are negligible or negative, the coordination story is cover and the constraint is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_benefit_measurement, empirical, 'Whether the regulatory regime''s claimed public safety benefits are empirically substantiated.').

omega_variable(
    prohibited_persons_expansion_trajectory,
    'Will the categories of prohibited persons continue expanding under the collective-security logic until the right is effectively nullified for a growing share of the population?',
    'Track legislative and judicial expansion of prohibited categories (non-violent felons, misdemeanor domestic violence, drug users, mental health adjudications, temporary restraining orders) over the next decade.',
    'If expansion continues unchecked, the constraint''s extraction becomes total for an expanding class — shifting toward snare. If courts or legislatures establish limiting principles, the tangled_rope balance holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibited_persons_expansion_trajectory, empirical, 'Whether the collective-security logic has an internal stopping point or tends toward universal prohibition.').

omega_variable(
    reading_relations_individual_right,
    'Does the collective-security reading logically foreclose the individual-right reading within a single constitutional framework, or do they coexist as competing interpretations?',
    'Analyze whether a jurisdiction can simultaneously hold that the right is conditioned on militia service AND that individuals have a right to arms for self-defense. The Heller majority held they are incompatible; the dissent held the collective-security reading subsumes the individual claim.',
    'If forecloses: the two readings cannot both be law in the same jurisdiction — one must prevail. If coexists_with: different courts or jurisdictions can apply different readings simultaneously. If influences: the collective-security reading creates pressure on the individual-right reading''s scope without eliminating it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_individual_right, conceptual, 'Structural relationship between collective-security and individual-right readings of the Second Amendment.').

omega_variable(
    reading_relations_civic_virtue,
    'Does the originalist civic virtue reading (universal armed citizenry as militia) foreclose, coexist with, or influence the collective-security reading?',
    'Compare the civic virtue reading''s claim that the militia IS the armed citizenry (so the right protects universal access) against the collective-security reading''s claim that the militia is a government-organized entity (so the right permits regulation). These are logically opposed framings of ''militia''.',
    'If forecloses: civic virtue and collective-security cannot both be true in one framework. If coexists_with: they are held by different political factions simultaneously. If influences: civic virtue arguments constrain how far collective-security regulation can go before triggering ''disarmament of the militia'' challenges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_civic_virtue, conceptual, 'Structural relationship between collective-security and originalist civic virtue readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 1934, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_collective_sec_tr_t1934, second_amendment_text__collective_security_reading, theater_ratio, 1934, 0.12).
narrative_ontology:measurement(sa_collective_sec_tr_t1968, second_amendment_text__collective_security_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(sa_collective_sec_tr_t1994, second_amendment_text__collective_security_reading, theater_ratio, 1994, 0.22).
narrative_ontology:measurement(sa_collective_sec_tr_t2008, second_amendment_text__collective_security_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(sa_collective_sec_tr_t2022, second_amendment_text__collective_security_reading, theater_ratio, 2022, 0.27).
narrative_ontology:measurement(sa_collective_sec_tr_t2024, second_amendment_text__collective_security_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(sa_collective_sec_be_t1934, second_amendment_text__collective_security_reading, base_extractiveness, 1934, 0.18).
narrative_ontology:measurement(sa_collective_sec_be_t1968, second_amendment_text__collective_security_reading, base_extractiveness, 1968, 0.28).
narrative_ontology:measurement(sa_collective_sec_be_t1994, second_amendment_text__collective_security_reading, base_extractiveness, 1994, 0.35).
narrative_ontology:measurement(sa_collective_sec_be_t2008, second_amendment_text__collective_security_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement(sa_collective_sec_be_t2022, second_amendment_text__collective_security_reading, base_extractiveness, 2022, 0.41).
narrative_ontology:measurement(sa_collective_sec_be_t2024, second_amendment_text__collective_security_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sa_collective_sec_su_t1934, second_amendment_text__collective_security_reading, suppression_requirement, 1934, 0.25).
narrative_ontology:measurement(sa_collective_sec_su_t1968, second_amendment_text__collective_security_reading, suppression_requirement, 1968, 0.32).
narrative_ontology:measurement(sa_collective_sec_su_t1994, second_amendment_text__collective_security_reading, suppression_requirement, 1994, 0.38).
narrative_ontology:measurement(sa_collective_sec_su_t2008, second_amendment_text__collective_security_reading, suppression_requirement, 2008, 0.34).
narrative_ontology:measurement(sa_collective_sec_su_t2022, second_amendment_text__collective_security_reading, suppression_requirement, 2022, 0.36).
narrative_ontology:measurement(sa_collective_sec_su_t2024, second_amendment_text__collective_security_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, national_firearms_act_1934).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, gun_control_act_1968).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, brady_background_check_system).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, assault_weapon_ban_1994).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the second_amendment_text kernel. The individual_right_reading and originalist_civic_virtue_reading are sibling constraints with different ε values, different beneficiary/victim structures, and different claimed types. All three are linked via affects_constraints. The collective-security reading's regulatory apparatus is the primary beneficiary; the individual-right reading makes gun owners the primary beneficiaries; the civic-virtue reading makes the 'universal citizenry' the beneficiary. These are not the same constraint viewed from different angles — they are structurally distinct claims with different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__collective_security_reading, organized, 0.75).
constraint_indexing:directionality_override(second_amendment_text__collective_security_reading, moderate, 0.8).
constraint_indexing:directionality_override(second_amendment_text__collective_security_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
