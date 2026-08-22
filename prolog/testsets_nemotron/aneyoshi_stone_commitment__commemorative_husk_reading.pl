% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi stone (erected 1933 after the Showa Sanriku tsunami) bears
 *   the directive: 'Do not build your homes below this point.' This reading
 *   treats the stone as a commemorative husk — the directive retained
 *   symbolic and ceremonial force but lost behavioral constraint on land-use
 *   decisions by the 1980s. The 2011 tsunami survival of households above the
 *   stone is attributed to topography and post-1960s construction choices
 *   unconnected to the stone, not to ongoing adherence. The stone functions
 *   as a museum piece: visited by tourists, cited in disaster studies,
 *   honored in annual ceremonies, but not consulted when building decisions
 *   are made. The high extractiveness (ε=0.78 at interval end) measures the
 *   gap between the stone's directive and actual land-use autonomy — the
 *   constraint extracts nothing from current behavior because it constrains
 *   nothing; the extraction is the historical decay itself, the gap between
 *   claim and practice that generates commemorative capital.
 *
 * KEY AGENTS:
 *   - local_government_tourism_office: Primary beneficiary (institutional/mobile) — leverages stone for disaster tourism revenue and municipal branding
 *   - academic_disaster_studies_community: Secondary beneficiary (organized/mobile) — cites stone as case study in 'traditional knowledge' and 'community resilience' literature
 *   - descendant_family_members_commemorative_role: Tertiary beneficiary (moderate/identity_locked) — perform annual rites at stone; identity fused with commemorative role rather than behavioral adherence
 *   - current_landowners_above_and_below_stone: Primary payers (moderate/constrained) — make building decisions independently of stone; bear no cost from stone but also receive no behavioral guidance from it
 *   - disaster_anthropology_analyst: Observer (analytical/analytical) — reads the stone's decay trajectory across 91 years
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.78).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.89).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.89).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Stone — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '6bf8971f-24cd-45a8-8dc4-d7c8630f1cea').
narrative_ontology:cs_kernel_codification('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea', fixed_text).
narrative_ontology:cs_authority_grounding('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea', lineage).
narrative_ontology:cs_interpretation_layer_present('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea').
narrative_ontology:cs_reading_relation('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea', foundational, stone_directive_lost_behavioral_force_by_1980).
narrative_ontology:cs_axiom_status(stone_directive_lost_behavioral_force_by_1980, holdable).
narrative_ontology:cs_axiom_grounding('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea', stone_directive_lost_behavioral_force_by_1980, empirically_contingent).
narrative_ontology:cs_axiom('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea', foundational, commemorative_function_is_primary_current_operation).
narrative_ontology:cs_axiom_status(commemorative_function_is_primary_current_operation, holdable).
narrative_ontology:cs_axiom_grounding('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea', commemorative_function_is_primary_current_operation, conventional).
narrative_ontology:cs_reference_frame('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea', id_1933_behavioral_mandate).
narrative_ontology:cs_drift_state('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea', post_2011_commemorative_consolidation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('6bf8971f-24cd-45a8-8dc4-d7c8630f1cea', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, local_government_tourism_office).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, academic_disaster_studies_community).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, descendant_family_members_commemorative_role).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, descendant_family_members_commemorative_role).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, current_landowners_above_and_below_stone).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the disaster tourism circuit centered on the stone. Funds annual ceremony, maintains signage, promotes 'ancient wisdom' narrative in marketing. Revenue from stone-related tourism is a measurable budget line. Could redirect tourism to other sites if stone lost its draw — exit is mobile.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, local_government_tourism_office, beneficiary,
    institutional, biographical, mobile, regional).

% Cites the Aneyoshi stone in peer-reviewed literature on traditional ecological knowledge, community resilience, and disaster memory. Gains citations, grant justification, and curricular material. The stone's commemorative status (not its behavioral force) is what makes it citable as 'traditional knowledge that worked.' Could study other stone monuments if this one lost academic currency — exit is mobile.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, academic_disaster_studies_community, beneficiary,
    organized, generational, mobile, global).

% Perform the annual ceremony at the stone; their family identity is constituted through this role. They maintain the stone physically and narratively. They also bear the cost of maintaining the performance (time, labor, emotional investment). Exit would mean abandoning a core identity anchor — identity_locked. The secondary payer role reflects that they invest in the commemorative apparatus without receiving its external benefits (tourism revenue, academic prestige).
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, descendant_family_members_commemorative_role, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, descendant_family_members_commemorative_role, payer).

% Make building and land-use decisions based on economics, family needs, and modern zoning — not the stone's directive. They bear no direct cost from the stone (no enforcement, no fines), but they also receive no risk-reduction guidance from it. The 'payer' role here captures the opportunity cost: a genuine behavioral constraint would have reduced their tsunami risk; the husk provides none. Exit from the stone's non-influence is trivial (it already doesn't constrain them), but exit from the commemorative narrative (which shapes zoning discourse and insurance markets) is constrained.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, current_landowners_above_and_below_stone, payer,
    moderate, biographical, constrained, local).

% Analyzes the stone's 91-year trajectory from behavioral constraint to commemorative husk. Holds no stake in its tourism revenue, academic citations, or family rites. Sees the full structure: the mandate died, the constraint persists, beneficiaries extract commemorative value, no one is coerced. The analytical seat is the only one that reads the stone's full decay trajectory without distortion.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_anthropology_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: coordinate settlement locations to reduce tsunami mortality across generations. In commemorative phase: coordinate community identity around disaster memory and municipal branding — the stone is a rallying point for 'we remember' without requiring 'we obey.'
% TRANSFER_FUNCTION: Moves commemorative capital (tourism revenue, academic citations, identity performance) from the stone's symbolic status to beneficiaries. No behavioral transfer occurs — land-use decisions are not transferred to the stone's directive.
% ABSENT_VOICES: The 1933 tsunami survivors who erected the stone with behavioral intent — they would object to the stone's conversion to a tourist attraction. The households that built below the stone line after 1975 and were lost in 2011 (if any) — they would testify that the stone did not constrain them. Neither group is present in the current commemorative apparatus.
% DISAPPEARANCE_RATIONALE: If the stone vanished overnight, land-use decisions would not change (they already ignore it). Tourism would shift to other disaster sites. Academic literature would cite other cases. The annual ceremony would end, but the commemorative identity it performs would persist in other forms. The world would not rearrange — the stone's behavioral force is already zero.
% FOUNDING_PROBLEM: The 1933 Showa Sanriku tsunami killed hundreds in Aneyoshi. Survivors erected the stone to fix a behavioral rule: never build below this elevation, so future generations would not repeat the settlement pattern that caused the death toll.
% FOUNDING_PROBLEM_CORROBORATION: The behavioral_competence_reading's proponents (local elders, some disaster researchers) attest the problem is live — tsunami risk remains. But land-use records, construction permits, and survivor interviews from 1975-2010 corroborated by municipal archives (outside the commemorative beneficiaries) show the stone's directive was not consulted in building decisions. The founding problem (tsunami mortality from low-elevation settlement) persists, but the stone's mandate to solve it is dead — corroborated by the very beneficiaries who now maintain the stone commemoratively.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The stone began as a genuine behavioral constraint (behavioral_competence_reading's domain) but atrophied: by 1980, new construction below the stone line occurred without community sanction; by 2000, the directive was openly described as 'traditional wisdom' rather than a rule. The 2011 tsunami — which spared homes above the line — was retroactively framed as validation, but land-use records show no correlation between stone awareness and siting decisions after 1975. Theater ratio rises to 0.89 because the annual ceremony, tourism signage, and academic citations perform the stone's authority while its behavioral function is zero. Suppression remains low (0.12) because no one is prevented from building below the line — the constraint simply isn't invoked. This is a piton: a former coordination mechanism (disaster risk reduction via settlement control) whose function has fully atrophied, maintained theatrically by beneficiaries who extract commemorative value from its husk.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (tourism office, academics, commemorative descendants) have d near 0.0 — the stone's commemorative status subsidizes them (revenue, citations, identity). Current landowners have d near 0.5 — they neither benefit nor pay; the stone is irrelevant to their decisions. The stone itself has no agency. The analytical observer sees the full decay trajectory. Directionality is derived from beneficiary declarations and the absence of victims — no one is coerced by the stone in its commemorative phase.
 *
 * MANDATROPHY ANALYSIS:
 *   The stone's mandate (settlement control for tsunami survival) died between 1960-1980 as coastal livelihoods shifted, memory faded, and land pressure increased. The arrangement persists because beneficiaries extract value from its commemorative form (tourism, academic prestige, ritual identity) without bearing the cost of behavioral enforcement. This is classic mandatrophy: the mandate is dead, the constraint remains, and the gap between them is the extraction surface. The piton classification captures this — no concentrated beneficiary captures the extraction (which would make it a snare); instead, diffuse beneficiaries maintain the theater because the cost of ending it exceeds their individual stake.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Aneyoshi stone one kernel with two readings (behavioral_competence_reading vs commemorative_husk_reading), or are these two distinct constraints that share a label?',
    'Test whether the two readings share the same referent arrangement (the stone and its directive) while disagreeing on its operational status, or whether they refer to different arrangements (the stone-as-active-rule vs the stone-as-monument). If the latter, decompose into two kernel-free stories; if the former, retain as kernel readings.',
    'If decomposed, each story gets its own ε, stakeholder set, and classification without reading-relations machinery. If retained as kernel readings, the structural delta between them must be carried through reading_relations and drift_state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the commemorative/behavioral split is a kernel-reading pair or a label-conflation requiring ε-invariance decomposition.').

omega_variable(
    commemorative_extraction_mechanism,
    'Does the commemorative function itself extract (tourism revenue, academic prestige, identity performance) or is the high ε driven entirely by the gap between the stone''s directive and actual land-use decisions?',
    'Revenue and prestige flow analysis: trace whether the stone''s commemorative status generates material benefits for identifiable agents, and whether those benefits depend on maintaining the stone as a ''husk'' rather than restoring it as a behavioral constraint.',
    'If commemorative function extracts, the constraint is a snare with a commemorative cover; if extraction is only the historical gap, it is a piton — theatrical maintenance of a dead mandate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commemorative_extraction_mechanism, empirical, 'Whether the commemorative husk is itself an extraction mechanism or merely the residue of a dead one.').

omega_variable(
    id_2011_survival_attribution,
    'Was the 2011 tsunami survival of Aneyoshi households attributable to the stone''s directive (behavioral competence) or to other factors (geography, luck, later construction choices)?',
    'Counterfactual modeling: compare Aneyoshi''s settlement pattern with neighboring villages that lacked stones but shared topography; interview survivors on decision-making; analyze whether post-1960 construction respected the stone''s line.',
    'If survival was due to the stone''s directive, the behavioral_competence_reading gains empirical grounding; if due to other factors, the commemorative_husk_reading''s claim that the stone had no behavioral force is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(id_2011_survival_attribution, empirical, 'Causal attribution of 2011 survival — the evidentiary anchor for the behavioral reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_commemorative_tr_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aneyoshi_commemorative_tr_t1960, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(aneyoshi_commemorative_tr_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1980, 0.45).
narrative_ontology:measurement(aneyoshi_commemorative_tr_t2000, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2000, 0.71).
narrative_ontology:measurement(aneyoshi_commemorative_tr_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.85).
narrative_ontology:measurement(aneyoshi_commemorative_tr_t2024, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2024, 0.89).

% Extraction over time
narrative_ontology:measurement(aneyoshi_commemorative_be_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aneyoshi_commemorative_be_t1960, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(aneyoshi_commemorative_be_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(aneyoshi_commemorative_be_t2000, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(aneyoshi_commemorative_be_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.72).
narrative_ontology:measurement(aneyoshi_commemorative_be_t2024, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_commemorative_su_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1933, 0.05).
narrative_ontology:measurement(aneyoshi_commemorative_su_t1960, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(aneyoshi_commemorative_su_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(aneyoshi_commemorative_su_t2000, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(aneyoshi_commemorative_su_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 2011, 0.12).
narrative_ontology:measurement(aneyoshi_commemorative_su_t2024, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_commitment kernel decomposes into two readings: behavioral_competence_reading (low ε, genuine coordination function, Mountain/Tangled Rope depending on enforcement period) and commemorative_husk_reading (high ε, atrophied mandate, Piton). They share the same physical stone and directive text but disagree on its operational status. This reading treats the stone as a dead mandate maintained for commemorative extraction; the sibling treats it as a live land-use rule. The ε values differ by ~0.7 — this is not measurement noise but structural decomposition of a conflated label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_stone_commitment__commemorative_husk_reading, moderate, 0.5).
constraint_indexing:directionality_override(aneyoshi_stone_commitment__commemorative_husk_reading, institutional, 0.05).
constraint_indexing:directionality_override(aneyoshi_stone_commitment__commemorative_husk_reading, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
