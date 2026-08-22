% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Land-Use Prohibition (Commemorative Husk Reading)
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   The Aneyoshi marker stone on the Sanriku coast (Japan) is a historical
 *   artifact inscribing a land-use prohibition: do not build below this line.
 *   The stone records a community's response to a prior catastrophe and
 *   encodes hazard knowledge meant to persist across generations. In this
 *   reading, the prohibition has decayed from an operationally enforced rule
 *   (behavioral_competence_reading) to a theatrical symbol. The stone
 *   remains; ceremonies mark its historical significance; but contemporary
 *   development decisions treat the zone as unrestricted. Development
 *   interests and municipal revenue-seekers benefit from reinterpreting the
 *   prohibition as expired; future residents who may occupy structures in the
 *   zone when the hazard recurs become latent victims. The extractiveness is
 *   high because the present actors knowingly shift catastrophic risk to the
 *   future. The theater ratio is very high because the constraint's main
 *   operational content is ceremonial: maintaining the stone's symbolic
 *   presence while its regulatory force has atrophied.
 *
 * KEY AGENTS:
 *   - stone_memorial: the artifact itself (non-agent entity); persists as symbol
 *   - development_interests: powerful, arbitrage-exit; benefit from treating prohibition as historical rather than binding
 *   - local_government_revenue_seekers: institutional, constrained-exit; collect permits and tax revenue from development in the prohibited zone
 *   - heritage_administration: institutional, mobile-exit; maintains the memorial and ceremonies without enforcing the underlying rule
 *   - future_residents_below_line: powerless, trapped-exit; will bear the cost when hazard recurs; absent from present decisions
 *   - original_community: powerless, trapped-exit; produced the prohibition; structurally excluded from contemporary authority structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.78).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.21).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.89).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.21).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.89).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Land-Use Prohibition (Commemorative Husk Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'bd0cb38d-6305-4b3a-9985-3a25b47c7cd7').
narrative_ontology:cs_kernel_codification('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7', fixed_text).
narrative_ontology:cs_authority_grounding('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7', lineage).
narrative_ontology:cs_interpretation_layer_present('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7').
narrative_ontology:cs_reading_relation('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7', aneyoshi_land_use_prohibition__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7', foundational, prohibition_is_commemorative_artifact).
narrative_ontology:cs_axiom_status(prohibition_is_commemorative_artifact, holdable).
narrative_ontology:cs_axiom_grounding('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7', prohibition_is_commemorative_artifact, conventional).
narrative_ontology:cs_axiom('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7', secondary, symbolic_form_decoupled_from_enforcement).
narrative_ontology:cs_axiom_status(symbolic_form_decoupled_from_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7', symbolic_form_decoupled_from_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7', operational_extinction_state).
narrative_ontology:cs_drift_state('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7', contemporary_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd0cb38d-6305-4b3a-9985-3a25b47c7cd7', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_government_revenue_seekers).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A physical monument bearing the prohibition. It persists as an artifact and a symbol; it makes no active interventions in land-use decisions. The stone's presence is decoupled from behavioral compliance with the rule it inscribes.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, stone_memorial, observer,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__commemorative_husk_reading, stone_memorial).

% Benefit from treating the prohibition as historically meaningful but operationally expired. They can develop the zone because the stone's authority has degraded to performative gesture; local and regional authorities no longer enforce the rule. Their position is strengthened by framing the prohibition as a relic, not a law.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests, beneficiary,
    powerful, biographical, arbitrage, national).

% Benefit from development permits and taxes on construction in the prohibited zone. Revenue from allowing development to proceed — licensing, property tax, business tax — accrues to municipal budgets. They maintain the commemoration without enforcing the prohibition: the stone remains, but the rule it was meant to instantiate is inactive.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_government_revenue_seekers, beneficiary,
    institutional, generational, constrained, regional).

% Maintains the stone as a heritage artifact and schedules commemoration ceremonies. Their authority is preserved over the symbol; they administer the memorial's presence and historical narrative. They do NOT administer or enforce the original land-use prohibition — that enforcement capacity has atrophied. Their role is custodial and performative.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, heritage_administration, agenda_setter,
    institutional, generational, mobile, regional).

% Will inhabit or depend on structures built in the prohibited zone in a future hazard event. They bear the cost when the original disaster recurs — flooding, landslide, or tsunami that the prohibition was designed to prevent. They are not present at the time development decisions are made; their victimhood is latent until the hazard manifests.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line, payer,
    powerless, immediate, trapped, local).

% The knowledge-bearer community that produced the prohibition in response to a prior disaster. Their prohibition is not consulted in contemporary decisions; the institutional chain of authority that would have kept their rule alive has been severed. They would object to development in the zone, but their voice is not admitted to current deliberation.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, original_community, excluded,
    powerless, civilizational, trapped, local).

% Examines the constraint from outside the affected community. Sees the theatrical maintenance (the stone, the ceremonies) as distinct from operational enforcement (permit denial, development prohibition). Documents how a rule can persist as symbol while its behavioral force decays.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The prohibition once coordinated community-level disaster risk management: residents knew the zone below the line was unsafe after the prior catastrophe, and the rule externalized that knowledge into a legible ban. In the commemorative reading, no present coordination function exists — the stone marks history, not active risk control.
% TRANSFER_FUNCTION: Moves the risk of future disaster from development interests and municipal revenue-seekers to future residents and occupants of structures built in the prohibited zone. The present actors benefit from reinterpreting the prohibition as expired; the cost (casualty risk, property loss) accrues to those not yet present to object.
% ABSENT_VOICES: The original disaster-bearing community is not consulted; their institutional authority over the prohibition has been severed by time and administrative reorganization. Future residents at risk are structurally absent — they cannot participate in present development decisions. Seismic scientists and hazard-mitigation experts may be absent if development permitting does not solicit their input.
% DISAPPEARANCE_RATIONALE: If the stone and its ceremonial maintenance disappeared, nothing immediate would change — the prohibition is already non-binding operationally. But if the stone's symbolic presence and the implicit mandate to respect it vanished entirely, the psychological and institutional frame that *could* restore compliance would be gone. The constraint itself has already largely disappeared functionally; the disappearance of its final symbolic form would complete the dissolution.
% FOUNDING_PROBLEM: A prior catastrophe (tsunami, flood, or landslide) devastated the community. The survivors placed a stone and established a prohibition: do not build below this line. The prohibition solved the problem of preserving hazard knowledge across generations — encoding it in a durable medium and spatial boundary.
% FOUNDING_PROBLEM_CORROBORATION: Seismic and hydrological data confirm the hazard is recurrent and the original zone identification was geologically sound. Anthropological analysis and historical records document the prior disaster and the community's response. The stone's inscription records the founding problem and the prohibition. However, present-day municipal authorities and development interests do not attest the founding problem as live — they treat the zone as safe for development under modern engineering standards. The corroboration is split: the hazard is real (geological sources); the founding problem is declared dead (development sector, municipal authorities).
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) and rising over the interval because development interests progressively ignore the rule and extract value from the zone while shifting catastrophic risk to future residents. Suppression is low (0.21) and declining because the constraint requires no active suppression — it is already non-binding; no coalition is actively resisting a rule that is no longer enforced. Theater ratio is very high (0.89) and rising because the main activity maintaining the constraint is ceremony and heritage administration, not behavioral compliance or enforcement. The constraint operates almost entirely as performance: the stone is preserved, commemorated, and narrated as historically important, while the rule it instantiates is operationally dead. Accessibility collapse is moderate (0.42) because the zone is technically accessible for development — there are no physical or legal barriers once the rule is reinterpreted as expired — but the symbolic and genealogical weight of the stone creates some friction. Resistance is low (0.31) because the original authority that would enforce the rule (the disaster-bearing community) has no institutional presence; contemporary voices (future residents, seismic scientists) are excluded from permitting decisions. The measurement series shows extraction rising and suppression falling, with theater rising sharply — the classic piton trajectory: a former operative rule maintained by administrative theater rather than function.
 *
 * PERSPECTIVAL GAP:
 *   The piton classification is stable across seats, but the narrative frames differ sharply. From heritage administration's seat, the constraint is a success: the stone is preserved, its history is remembered, and ceremonies maintain cultural continuity with the original community's judgment. From development interests' seat, the constraint is an opportunity: reinterpretation as historical rather than binding allows value extraction. From the future residents' (latent, absent) seat, the constraint is a failed safeguard: the rule that would protect them is operationally dead, replaced by symbol. The engine computes all three seats identically as piton — none benefit enough to maintain it actively, and none are hurt enough by it in the present to fix it — but the NARRATIVE experience diverges sharply. Heritage administration sees preservation; developers see opportunity; future residents see latent peril.
 *
 * DIRECTIONALITY LOGIC:
 *   Development interests (d near 1.0 — target): they benefit from the rule being unenforceable and extract value by developing the zone. Local government (d near 0.8 — target): they collect revenue from permitting development. Heritage administration (d near 0.5 — symmetric): they maintain the memorial and do genuine heritage work, but they also administer the non-enforcement that allows extraction. Future residents (d near 1.0 — target, latent): they bear the risk cost when hazard recurs, but they are not present to experience extraction in real time. Original community (d near 1.0 — target, excluded): their rule is disregarded, but they have no institutional power to object in contemporary decisions. The beneficiary/victim split reflects this structure: those who benefit from present non-enforcement (development interests, local government) are listed as beneficiaries; those who bear the cost (future residents) are victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving hazard knowledge across generations through a physical prohibition) has been officially declared dead (local authorities and development interests treat it as historically resolved). The disappearance_verdict is world_rearranges (if the prohibition were enforced, development would cease and land-use patterns would shift). The mandatrophy mismatch is clear: the founding problem is dead (no one attests it as live in permitting decisions), but the world would rearrange if the rule were re-activated. This is precisely the zombie constraint class: the problem it was designed to solve is officially solved (the hazard was managed), but enforcement has been abandoned and the latent cost (future disaster risk) persists. The constraint persists as theater because the stone is valuable as a heritage artifact and the ceremonies are meaningful to the community, but the rule itself collects no active defense from any present stakeholder.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_decay_vs_symbolic_persistence,
    'Is the prohibition''s shift from enforced rule to symbol a temporary degradation, or an irreversible institutional fact?',
    'A catastrophic recurrence in the development zone would empirically test whether the symbolic form can be re-activated into enforcement, or whether the decay is permanent. Alternative: legislative action to restore explicit enforcement would establish agency rather than inevitability.',
    'If degradation is reversible, the constraint might reclassify as tangled_rope under a renewal scenario. If irreversible, it remains piton. The question determines whether the constraint''s type is stable or contingent on political choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_decay_vs_symbolic_persistence, empirical, 'Whether the prohibition can be operationally restored or is permanently theatrical.').

omega_variable(
    behavioral_competence_vs_commemorative_readings_foreclosure,
    'Do these two readings foreclose each other, or are they genuinely coexistent?',
    'This is a constitutive question about the kernel: can one authority structure (heritage administration) hold the prohibition as both a historically meaningful rule AND a presently non-binding symbol? Or does recognition that the rule has decayed automatically invalidate any claim that it remains operationally enforced? If the kernel admits both readings simultaneously (different stakeholders inhabit different frames), they coexist; if the readings claim mutually exclusive truth about the constraint''s present state, they foreclose.',
    'If coexistent: the engine assigns per-seat types and the two readings are complementary analytical lenses on the same situation. If foreclosing: accepting the commemorative reading requires abandoning the behavioral reading''s core claim that the prohibition is currently enforced. This difference determines network topology and whether the two readings should feed into a contest/switch mechanism or coexist as rival stories of the same constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_competence_vs_commemorative_readings_foreclosure, conceptual, 'Whether the readings are alternative frames on one constraint or mutually exclusive claims about the constraint''s present state.').

omega_variable(
    latent_victimhood_activation,
    'How certain is the hazard recurrence that would convert latent victims (future residents) into manifest victims?',
    'Paleoseismic, paleoclimate, and hydrological records establish recurrence intervals and probability. Engineering analysis of built structures in the zone under hazard conditions determines casualty and damage likelihood.',
    'If recurrence is highly likely (< 100 years), future victimhood is not latent but structurally embedded in the present extractive arrangement — the victims are knowable and the violation is present. If recurrence is centuries-scale or highly uncertain, the victimhood remains speculative and the constraint''s classification might shift toward benign rent-seeking rather than concealed exploitation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(latent_victimhood_activation, empirical, 'Whether future resident victimhood is a certain outcome or a low-probability tail risk.').

omega_variable(
    kernel_reading_boundary,
    'Is this constraint one reading of the aneyoshi_land_use_prohibition kernel, or does it describe a functionally different constraint (memorial governance)?',
    'Does the stone''s present role as a memorial make decisions about its preservation and ceremonial status, or does it play any role in land-use permitting, risk disclosure, or development review? If memorial governance is entirely decoupled from land-use decisions, this might be two constraints (the land-use prohibition itself, and the memorial as a separate heritage constraint). If the memorial''s framing (as historical vs. operational) shapes how authorities treat the underlying rule, they remain one constraint with two readings.',
    'If two constraints: the stories should be decomposed per ε-invariance (the memorial constraint has near-zero extractiveness; the land-use prohibition has high extractiveness). If one constraint with two readings: the ε difference represents the reading-level variance, not a constraint boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the constraint is the land-use rule (with two readings of its enforcement state) or whether memorial governance is a separate constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(aney_tr_t7, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 7, 0.34).
narrative_ontology:measurement(aney_tr_t14, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 14, 0.54).
narrative_ontology:measurement(aney_tr_t21, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 21, 0.71).
narrative_ontology:measurement(aney_tr_t35, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 35, 0.86).
narrative_ontology:measurement(aney_tr_t50, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 50, 0.89).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aney_be_t7, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(aney_be_t14, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 14, 0.62).
narrative_ontology:measurement(aney_be_t21, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 21, 0.71).
narrative_ontology:measurement(aney_be_t35, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 35, 0.76).
narrative_ontology:measurement(aney_be_t50, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(aney_su_t7, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 7, 0.58).
narrative_ontology:measurement(aney_su_t14, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 14, 0.42).
narrative_ontology:measurement(aney_su_t21, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 21, 0.29).
narrative_ontology:measurement(aney_su_t35, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 35, 0.24).
narrative_ontology:measurement(aney_su_t50, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 50, 0.21).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.12).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel decomposes into two readings instantiating distinct constraints with divergent ε values and seat-dependent classifications. Both readings share the physical stone and the historical rule, but they differ on whether the rule is operationally enforced (behavioral_competence_reading: yes, 78-year track record; commemorative_husk_reading: no, decay to symbol). The readings have different extractiveness profiles because development interests have different directionalities under each reading. The network link indicates the readings are siblings under the same kernel — analytical understanding of one informs the other, but they are separate constraints with separate types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
