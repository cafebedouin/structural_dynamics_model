% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Commemorative Husk of Stone Land-Use Rule
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   A physical stone erected after a past disaster to mark a lethal hazard
 *   boundary and enforce a prohibition on construction. Over decades, the
 *   rule decayed: the stone remains polished, fenced, and inscribed, but
 *   building permits are issued all around it and the zone is fully
 *   developed. The constraint is read here as a commemorative husk â a
 *   piton where the original land-use function has atrophied and what
 *   persists is theatrical maintenance of memory that occupies the governance
 *   space where a real rule should be. The sibling reading
 *   (behavioral_competence) treats the same stone as a live prohibition; the
 *   two are decomposed per Îµ-invariance because they have different
 *   stakeholder structures, different Îµ values, and different failure modes.
 *
 * KEY AGENTS:
 *   - Municipal heritage administration: agenda_setter (institutional/constrained) â maintains the stone, could change its status but faces political cost
 *   - Coastal residents and future occupants: primary payer (powerless/trapped) â bear the disaster risk externalized by the regulatory void
 *   - Disaster memory community: excluded from land-use governance (organized/constrained) â invested in the stone's presence but absent from zoning decisions
 *   - Waterfront developers: analytical observer in the constraint story â they profit from the absence of enforcement but do not capture the constraint's extraction directly; their gain is mediated through the regulatory void, so they are omitted from the formal beneficiary set to preserve the piton structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.79).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.22).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.79).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.88).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Commemorative Husk of Stone Land-Use Rule").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '0b2af03f-55b3-4538-9408-a1d34000bc19').
narrative_ontology:cs_kernel_codification('0b2af03f-55b3-4538-9408-a1d34000bc19', fixed_text).
narrative_ontology:cs_authority_grounding('0b2af03f-55b3-4538-9408-a1d34000bc19', lineage).
narrative_ontology:cs_interpretation_layer_present('0b2af03f-55b3-4538-9408-a1d34000bc19').
narrative_ontology:cs_reading_relation('0b2af03f-55b3-4538-9408-a1d34000bc19', stone_land_use_rule__behavioral_competence, forecloses).
narrative_ontology:cs_axiom('0b2af03f-55b3-4538-9408-a1d34000bc19', foundational, memorial_fulfills_safety_obligation).
narrative_ontology:cs_axiom_status(memorial_fulfills_safety_obligation, holdable).
narrative_ontology:cs_axiom_grounding('0b2af03f-55b3-4538-9408-a1d34000bc19', memorial_fulfills_safety_obligation, conventional).
narrative_ontology:cs_axiom('0b2af03f-55b3-4538-9408-a1d34000bc19', foundational, temporal_decay_of_regulatory_force).
narrative_ontology:cs_axiom_status(temporal_decay_of_regulatory_force, holdable).
narrative_ontology:cs_axiom_grounding('0b2af03f-55b3-4538-9408-a1d34000bc19', temporal_decay_of_regulatory_force, conventional).
narrative_ontology:cs_reference_frame('0b2af03f-55b3-4538-9408-a1d34000bc19', commemorative_sufficiency).
narrative_ontology:cs_drift_state('0b2af03f-55b3-4538-9408-a1d34000bc19', contemporary_development_pressure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0b2af03f-55b3-4538-9408-a1d34000bc19', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, coastal_residents).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, heritage_supersedes_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the commemorative stone and its ceremonial calendar; holds bureaucratic authority to reclassify or remove the marker but faces political friction from memory communities and the symbolic cost of admitting the old rule is dead. The stone's presence satisfies the administrative requirement for disaster remembrance without requiring costly land-use enforcement or developer conflict.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_heritage_admin, agenda_setter,
    institutional, generational, constrained, local).

% Live and work in the hazard zone where the stone once enforced a building prohibition; the marker now signals historical risk while offering no structural protection. They bear the physical and financial risk of future disasters, often with no affordable relocation options because the unregulated waterfront housing market prices them into the danger zone.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_residents, payer,
    powerless, biographical, trapped, local).

% Comprised of descendants, survivors, and local historians who tend the stone and hold annual ceremonies; they are present in the commemorative space but excluded from the land-use governance process that decides whether the zone is built. Their emotional investment in the stone makes removing or repurposing it politically difficult, yet they do not set zoning policy.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_memory_community, excluded,
    organized, generational, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, diffuse).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: to prohibit construction and settlement in a lethal hazard zone by physically marking the boundary with a stone. Currently: none â the arrangement coordinates only the annual commemorative ceremony and heritage tourism, not spatial practice or safety.
% TRANSFER_FUNCTION: Originally: moved land-use discretion away from individual owners toward a collective safety rule. Currently: moves regulatory attention and political capital away from active land-use governance toward symbolic maintenance, externalizing disaster risk to future occupants while waterfront development proceeds unimpeded.
% ABSENT_VOICES: Disaster-risk engineers, geomorphologists, and affordable-housing advocates who would demand either formal repeal of the old rule (clearing the way for explicit zoning) or its reactivation are absent from the heritage-administration process; their seat is institutionally weaker than the heritage commission.
% DISAPPEARANCE_RATIONALE: If the stone and its heritage apparatus vanished overnight, the ambiguous equilibrium would collapse. The municipality would face pressure to either formally abolish the prohibition (inviting memory-community backlash) or reactivate land-use enforcement (invoking developer backlash), but the current pattern of building in the hazard zone while a memorial pretends to govern would end.
% FOUNDING_PROBLEM: A past disaster demonstrated that the waterfront zone was lethally unsafe; the stone was erected to permanently mark the hazard boundary and enforce a prohibition on resettlement and construction.
% FOUNDING_PROBLEM_CORROBORATION: Disaster historians and geomorphologists attest the hazard remains live. The municipal heritage administration acknowledges the founding event only in commemorative speeches, not in land-use policy. No corroboration from outside the heritage-administration beneficiaries exists that the problem is still being solved by this arrangement.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.79, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79) because the atrophied constraint imposes a massive opportunity-cost externality: it occupies institutional memory, budget, and political space that could hold an active land-use rule, while the hazard zone is developed. Theater ratio is very high (0.88) because nearly all remaining activity is polishing, fencing, and ceremony rather than spatial enforcement. Suppression is low (0.22) because there is no behavioral coercion left; the stone is ignored. Resistance is modest (0.31) because safety advocates occasionally challenge permits but are institutionally outgunned. Accessibility collapse is moderate (0.42): alternatives like formal repeal or reactivation are institutionally available but politically blocked by the inertia of the commemorative apparatus. The metrics and the piton claim are authored independently; if the engine detects sufficient extraction to flag a snare, that divergence is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   The heritage administration experiences the constraint as a benign, low-friction duty with reputational upside. Coastal residents experience it as a failed promise that advertises safety while offering none. The engine computes this divergence from the structural data: the agenda-setter has institutional power and constrained exit (bureaucratic inertia), while the payer is powerless and trapped in the housing market.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared in base_properties because no agent meaningfully captures the extraction; the gain from the regulatory void is diffuse across the development market and not harvested by any single seat. Coastal residents are the identified victims because they bear the concentrated physical risk. The municipal heritage admin is the agenda_setter who administers the piton and could change it, but the cost of fixing exceeds its incentive to do so.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â lethal hazard requiring a building prohibition â is dead in the sense that the constraint no longer addresses it. The constraint persists not because it coordinates safety but because the institutional apparatus of commemoration has become self-sustaining. Classifying this as a piton rather than a snare prevents mislabeling the memory community's genuine (non-extractive) investment as rent-seeking, while still capturing the diffuse extraction imposed by the regulatory void.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_residue_ambiguity,
    'Is there any residual informal compliance or social stigma around the stone that partially restricts building, or is behavioral compliance fully decoupled from the marker?',
    'Ethnographic observation and building-permit analysis in the immediate vicinity of the stone to determine whether spatial practice still orients to the marker.',
    'If residue exists, effective extraction is lower than authored and the constraint may compute as a tangled rope rather than a piton; if fully decoupled, the piton reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_residue_ambiguity, empirical, 'Whether any behavioral force remains in the supposedly dead rule').

omega_variable(
    inertia_vs_intent,
    'Does the commemorative husk persist purely from bureaucratic inertia, or is it actively maintained by actors who benefit from the regulatory void?',
    'Archival study of municipal land-use and heritage commission minutes to trace who initiates maintenance funding and who blocks rezoning initiatives.',
    'If actively maintained to block regulation, reclassify toward snare; if pure inertia, the piton reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inertia_vs_intent, empirical, 'Whether memorial maintenance is cover for extraction or genuine inertia').

omega_variable(
    cs_framing_underdetermination,
    'Should this constraint be framed as a commitment system (heritage authority grounding legitimacy in the stone-as-fixed-text) or as simple institutional inertia without authoritative interpretation?',
    'Examine whether heritage officials cite the stone as an authoritative source in decisions, or merely as a budget line item.',
    'If the former, the commitment-system apparatus (axiom overriding, drift state) applies; if the latter, the cs_structure block over-interprets the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framing between commitment system and bare inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stone_husk_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.4).
narrative_ontology:measurement(stone_husk_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.55).
narrative_ontology:measurement(stone_husk_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.68).
narrative_ontology:measurement(stone_husk_tr_t30, stone_land_use_rule__commemorative_husk, theater_ratio, 30, 0.75).
narrative_ontology:measurement(stone_husk_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.82).
narrative_ontology:measurement(stone_husk_tr_t50, stone_land_use_rule__commemorative_husk, theater_ratio, 50, 0.88).

% Extraction over time
narrative_ontology:measurement(stone_husk_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(stone_husk_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(stone_husk_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(stone_husk_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(stone_husk_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(stone_husk_be_t50, stone_land_use_rule__commemorative_husk, base_extractiveness, 50, 0.79).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__commemorative_husk, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% This constraint and stone_land_use_rule__behavioral_competence are two readings of the stone_land_use_rule kernel, decomposed per the Îµ-invariance principle. They share the same material referent (the stone) but have different Îµ values, different stakeholder structures, and different failure modes: one describes a live enforcement mechanism, the other describes an atrophied theatrical husk.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
