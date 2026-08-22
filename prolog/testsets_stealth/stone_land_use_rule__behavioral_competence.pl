% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Ancestral High-Water Stone as Binding Land-Use Line (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   After the 1933 Showa Sanriku tsunami, villages on the Sanriku coast
 *   inscribed stones at the demonstrated high-water line bearing prohibitions
 *   against building below it. In the villages this story describes, the line
 *   held as a live land-use rule for the following 78 years: new housing went
 *   uphill, the path network, school, and shrine followed the elevated
 *   pattern, and children were taught the stone's warning as ordinary
 *   knowledge. The economic cost — steeper approaches, costlier slope
 *   foundations, longer carries from the port — was accepted rather than
 *   litigated. At interval end the pattern was vindicated: the housing of
 *   compliant villages stood above the Tohoku run-up while neighboring
 *   villages that had drifted seaward lost theirs. This file instantiates the
 *   behavioral_competence reading of the stone_land_use_rule kernel: the
 *   stone as binding prohibition enforced by daily spatial practice. The
 *   sibling reading (commemorative_husk) treats the same inscriptions as
 *   memorial artifacts without behavioral force — a different constraint with
 *   a different epsilon, authored separately and linked in network.
 *   Assumptions: T0 = 1933 (post-tsunami re-siting and inscription), T78 =
 *   2011; the referent of epsilon is the standing uphill-settlement
 *   arrangement as this reading sees it, never the arrangement this reading
 *   would endorse. Claim and metrics are authored independently: the
 *   constraint is claimed as rope; the metrics describe its actual
 *   low-extraction, low-coercion operation.
 *
 * KEY AGENTS:
 *   - hilltop_households: primary beneficiary and cost-bearer (moderate power / constrained exit) — comply with the uphill line and pay its daily premium
 *   - descendant_generations: silent beneficiary (powerless / trapped) — inherit the settlement pattern the covenant fixes, unable to revisit the founding decision
 *   - stone_custodian_elders: agenda setter (organized / identity_locked) — keep the stone, retell the warning, confront would-be seaward builders
 *   - port_fishing_operators: secondary payer and beneficiary (organized / constrained) — bear the covenant as daily friction on sea-level work, collect from it as survived plant and crews
 *   - lowland_development_planners: excluded voice (institutional / mobile) — postwar planning seat outside the covenant's framework that would have allocated the coast differently
 *   - disaster_ethnographers: analytical observer (analytical / analytical) — hold the comparative siting record that vindicates or undermines the covenant's force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.14).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.12).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.14).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Ancestral High-Water Stone as Binding Land-Use Line (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, '43f33e0a-bec2-4ec0-83d7-101864ca7baa').
narrative_ontology:cs_kernel_codification('43f33e0a-bec2-4ec0-83d7-101864ca7baa', fixed_text).
narrative_ontology:cs_authority_grounding('43f33e0a-bec2-4ec0-83d7-101864ca7baa', practice).
narrative_ontology:cs_interpretation_layer_present('43f33e0a-bec2-4ec0-83d7-101864ca7baa').
narrative_ontology:cs_reading_relation('43f33e0a-bec2-4ec0-83d7-101864ca7baa', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('43f33e0a-bec2-4ec0-83d7-101864ca7baa', foundational, inscribed_high_water_line_is_binding).
narrative_ontology:cs_axiom_status(inscribed_high_water_line_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('43f33e0a-bec2-4ec0-83d7-101864ca7baa', inscribed_high_water_line_is_binding, conventional).
narrative_ontology:cs_axiom('43f33e0a-bec2-4ec0-83d7-101864ca7baa', foundational, flood_memory_must_outlive_living_memory).
narrative_ontology:cs_axiom_status(flood_memory_must_outlive_living_memory, holdable).
narrative_ontology:cs_axiom_grounding('43f33e0a-bec2-4ec0-83d7-101864ca7baa', flood_memory_must_outlive_living_memory, empirically_contingent).
narrative_ontology:cs_reference_frame('43f33e0a-bec2-4ec0-83d7-101864ca7baa', binding_settlement_boundary).
narrative_ontology:cs_drift_state('43f33e0a-bec2-4ec0-83d7-101864ca7baa', post_tohoku_2011, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('43f33e0a-bec2-4ec0-83d7-101864ca7baa', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, hilltop_households).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, descendant_generations).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, port_fishing_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, hilltop_households).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, port_fishing_operators).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, tsunami_recurrence_doctrine).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, ancestral_high_water_reliability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and maintain homes above the inscribed high-water line; their daily movement — paths to fields, the shrine, the school — runs uphill by design. They bear the constraint's cost directly: steeper approaches, costlier slope foundations, longer carries from the port. The same households collect the arrangement's payoff as their own futures and their children's: at interval end their houses stood above the inundation while seaward neighbors' did not. Exit from the rule while staying in the village would mean building seaward against the covenant and its custodians; exit from the village altogether means abandoning livelihoods tied to the coast.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, hilltop_households, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, hilltop_households, payer).

% Not yet present when the line was inscribed and the settlement pattern set; they inherit whatever geography their predecessors chose. They receive the safety the uphill pattern delivers and cannot revisit the founding decision; if the pattern erodes, they inherit exposure instead. They are also the present households' own children — the seats overlap across time, which is why the arrangement operates as intergenerational mutual insurance rather than transfer to a separate class.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, descendant_generations, beneficiary,
    powerless, generational, trapped, local).

% Keep the stone, retell its warning at village gatherings, teach children what the line means, and confront any household proposing to build below it. Their standing in the village rests on the covenant's continued salience; abandoning the custodial role would dissolve the identity that role constitutes. They bear stewardship labor rather than material cost, and they are the seat that could formally renounce the rule — a step the record shows they never took across the interval.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, stone_custodian_elders, agenda_setter,
    organized, generational, identity_locked, local).

% Work the sea-level economy the covenant does not govern — landing catches, running gear sheds — while their homes and their main facilities sit uphill. They pay the rule as daily friction: slope commutes, duplicated storage, costlier plant siting away from the water. They collect from it as well: at interval end their elevated plant and crews were above the run-up. Their exit would be relocating the business to a coast without the covenant, at the cost of leaving the port itself.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, port_fishing_operators, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, port_fishing_operators, beneficiary).

% Postwar municipal and prefectural planners promoting port-centered growth, land reclamation, and seawall-dependent lowland development. They never sat inside the covenant's framework: statutory zoning did not codify the stone line, and their planning documents treated the ancestral boundary as an obstacle or a curiosity. They are the seat that would have allocated the coast differently had the covenant not held; their absence from the conversation is what the covenant's informal force papers over.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, lowland_development_planners, excluded,
    institutional, generational, mobile, regional).

% Field researchers who, before and after the 2011 tsunami, documented which villages kept the uphill pattern and which drifted seaward, and compared inundation outcomes against the stone lines. They hold the comparative record that vindicates or undermines the covenant's behavioral force, but they set nothing, collect nothing, and bear nothing.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_ethnographers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__behavioral_competence, descendant_generations).
narrative_ontology:fixing_cost_class(stone_land_use_rule__behavioral_competence, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes an intergenerational settlement boundary at the demonstrated high-water line so that each generation need not re-derive the tsunami hazard from scratch; encodes accumulated flood memory as a single legible, durable rule that coordinates housing, path, and civic siting across centuries of turnover in who lives in the village.
% TRANSFER_FUNCTION: Moves construction cost and daily convenience uphill — from present households to the village's future members — and moves decision authority backward across time: the ancestors' encoded judgment overrides each present generation's own risk assessment. Nothing moves to any administrator seat; no one collects a rent on compliance.
% ABSENT_VOICES: Descendant generations cannot speak at the founding moment — the rule binds people not yet born, who inherit it as settled fact. Postwar planners and economic modernizers who regarded the customary line as an obstacle to port-centered growth were never party to the covenant's framework; their objection (that engineered seawalls had made ancestral siting obsolete) was voiced in planning documents rather than at the stone, and statutory zoning never codified the line they would have had to argue against.
% DISAPPEARANCE_RATIONALE: If the covenant vanished overnight — if the stone's line stopped binding — settlement would drift seaward toward the port within a generation or two, exactly as it did in villages where the practice failed: the 2011 inundation mapping shows those villages' housing inside the run-up. The path network, school, shrine, and plant siting would all reorganize around sea-level convenience, and the next tsunami would find the village at water level. What this constraint organizes is the village's entire vertical distribution of life.
% FOUNDING_PROBLEM: After the 1896 and 1933 Sanriku tsunamis killed tens of thousands in villages whose disaster memory died with the generation that experienced them, survivors needed a way to keep future settlements out of the inundation zone after living memory faded — a record that would outlast its witnesses.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the paleotsunami record (Jogan-era sand sheets beneath the 2011 deposits) and instrumental seismology establish century-scale recurrence on the Japan trench, attesting the hazard is live; the 2011 joint survey inundation mapping shows compliant villages' housing above the observed run-up, attesting the line tracked a real boundary; post-2011 disaster ethnography comparing stone-line villages with non-compliant neighbors attests the founding problem from the analytical seat. No corroborating voice attests from inside the planning establishment — the excluded planners dispute the founding framing entirely, which is itself signal.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.14 at interval end) because the constraint's take is the convenience premium of uphill living and no administrator seat collects a rent — the custodian elders bear stewardship labor rather than drawing income from compliance. Suppression is low (0.12) because the constraint never built enforcement machinery: it operates through socialization, retelling, and the self-evident geography of an already-uphill village, and building seaward remained physically possible throughout — suppression here is a structural property of the arrangement, not a scaled quantity, and it is small because the arrangement is small. Accessibility collapse is correspondingly moderate-low (0.25): understanding the rule re-weights the siting choice rather than closing alternatives. Resistance is low (0.12): episodic pressure from port economics and youth outmigration, never organized opposition, because the rule's payoff is legible to anyone who knows the coast's history. Theater is low (0.15) with a slow rise across the interval as living memory of 1933 faded and the stone accreted commemorative functions — but in these villages the behavioral force persisted, which is precisely what separates this reading from the husk sibling. The temporal series run on one shared seven-point grid (T0, 13, 26, 39, 52, 65, 78); suppression_requirement is deliberately not tracked as a series because the enforcement picture is static — the constraint never relied on an enforcement apparatus whose capacity could ratchet or decay, and the scalar suppression captures that.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify the same stone differently. From the custodian elders' seat the covenant is a living charge — its persistence is their identity and standing, and its abandonment is unthinkable rather than merely costly. From hilltop households' seat it is barely experienced as a constraint at all once built: the uphill pattern is simply where the village is. From port operators' seat it is a daily friction that 2011 converted into vindication. From the excluded planners' seat it is an uncodified obstacle that never entered statutory zoning at all. The engine computes these divergences from the structural data; this story's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (hilltop_households, descendant_generations, port_fishing_operators) place those seats toward the beneficiary end: the constraint subsidizes their safety at the price of a convenience premium they pay to themselves across time. No seat is declared a victim and none should be — the hill-climb cost is paid by the same population that collects the safety, which is why this is a rope rather than a tangled rope. The custodian elders, as agenda-setters with identity-locked exit, sit near-symmetric: paid in standing and safety, paying in stewardship labor. Descendant generations receive without ever paying, but they are the payers' own children — the seats overlap across generations, so population-level directionality is near-symmetric over time. Spatial scope is local, so the engine's scope amplification of the modest epsilon is minimal. No directionality overrides are declared: the beneficiary structure plus exit options already yield the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting flood memory past the lifespan of its witnesses — is live: the Japan trench recurs on century scales, so the arrangement's function is intact and mandatrophy_resolved is not declared. The classification discipline matters here in both directions. A snare reading would need victims; none exist, and reading the hill-climb premium as extraction from present households mistakes an intergenerational insurance premium for a rent. Conversely, the genuine mandatrophy risk is the sibling reading's home terrain: where practice decays, the stone becomes a piton — commemorative performance around a dead function — and the husk reading authors that case. This story's 78-year record is the evidence that decay is contingent on practice maintenance, not necessary. The R5 mismatch check is clean: founding_problem_status=live with disappearance_verdict=world_rearranges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_force_distribution,
    'Does the behavioral force documented in the compliant villages generalize across all stone-bearing villages, or is it village-specific practice that the stones merely record?',
    'Comparative ethnographic study of siting outcomes 1933-2011 across all villages bearing high-water inscriptions, controlling for port economics and terrain.',
    'If force is village-specific, the kernel decomposes into per-village constraints and the commemorative_husk reading covers the residue; if general, this reading''s epsilon applies across the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_force_distribution, empirical, 'Whether the covenant''s behavioral force is general or village-specific.').

omega_variable(
    compliance_mechanism_ambiguity,
    'Is compliance sustained by the stone''s inscribed authority, or by the self-perpetuating geography of a settlement already committed uphill?',
    'Natural experiment: villages where stones were lost or destroyed but the settlement pattern initially persisted — trace subsequent siting decisions once the mnemonic was gone.',
    'If geography alone sustains compliance, the stone''s active component is smaller than authored, and drift toward the husk reading should accelerate once 2011 reconstruction disrupts the built pattern.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_mechanism_ambiguity, empirical, 'Stone-as-cause versus stone-as-mnemonic in sustaining compliance.').

omega_variable(
    intergenerational_cost_incidence,
    'Do present households accept the uphill premium as fair exchange for expected safety, or does the covenant transfer cost from the present to the future beyond what the present would voluntarily pay?',
    'Revealed-preference comparison of siting choices where the covenant''s social enforcement relaxed versus held, plus contingent valuation of the uphill premium among present households.',
    'If present generations heavily discount century-scale risk, part of the arrangement is normatively compelled intergenerational transfer — nudging the classification toward tangled_rope with present households as part-payers and descendants as beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_cost_incidence, preference, 'Fair exchange versus compelled intergenerational transfer in the cost structure.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel the inscribed text itself (the fixed stone) or the settlement practice, with the stone as one mnemonic among several?',
    'Compare villages with the same uphill practice but no surviving stones: identical compliance indicates the practice is the kernel and the stone an information standard riding on it.',
    'cs_structure values change (kernel_codification implicit rather than fixed_text) and the drift story shifts — practice can outlive the stone, which is the terrain where the husk reading operates — though the constraint''s type and epsilon are largely unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Kernel under-determination between inscription and practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t13, stone_land_use_rule__behavioral_competence, theater_ratio, 13, 0.06).
narrative_ontology:measurement_basis(ston_tr_t13, observed).
narrative_ontology:measurement(ston_tr_t26, stone_land_use_rule__behavioral_competence, theater_ratio, 26, 0.08).
narrative_ontology:measurement_basis(ston_tr_t26, observed).
narrative_ontology:measurement(ston_tr_t39, stone_land_use_rule__behavioral_competence, theater_ratio, 39, 0.1).
narrative_ontology:measurement_basis(ston_tr_t39, observed).
narrative_ontology:measurement(ston_tr_t52, stone_land_use_rule__behavioral_competence, theater_ratio, 52, 0.12).
narrative_ontology:measurement_basis(ston_tr_t52, observed).
narrative_ontology:measurement(ston_tr_t65, stone_land_use_rule__behavioral_competence, theater_ratio, 65, 0.14).
narrative_ontology:measurement_basis(ston_tr_t65, observed).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.15).
narrative_ontology:measurement_basis(ston_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t13, stone_land_use_rule__behavioral_competence, base_extractiveness, 13, 0.11).
narrative_ontology:measurement_basis(ston_be_t13, observed).
narrative_ontology:measurement(ston_be_t26, stone_land_use_rule__behavioral_competence, base_extractiveness, 26, 0.12).
narrative_ontology:measurement_basis(ston_be_t26, observed).
narrative_ontology:measurement(ston_be_t39, stone_land_use_rule__behavioral_competence, base_extractiveness, 39, 0.12).
narrative_ontology:measurement_basis(ston_be_t39, observed).
narrative_ontology:measurement(ston_be_t52, stone_land_use_rule__behavioral_competence, base_extractiveness, 52, 0.13).
narrative_ontology:measurement_basis(ston_be_t52, observed).
narrative_ontology:measurement(ston_be_t65, stone_land_use_rule__behavioral_competence, base_extractiveness, 65, 0.13).
narrative_ontology:measurement_basis(ston_be_t65, observed).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.14).
narrative_ontology:measurement_basis(ston_be_t78, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__behavioral_competence, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, information_standard).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% The natural-language label 'the tsunami stones' covers two structurally distinct claims about the same inscriptions. This story (behavioral_competence) authors the stone as a live land-use prohibition with sustained behavioral force and low epsilon; the sibling story (stone_land_use_rule__commemorative_husk) authors the stone as a memorial artifact whose warning decayed to symbol without behavioral force — high theater, atrophied function, piton-flavored. The readings share the kernel and the physical referent but instantiate different constraints: different epsilon, different beneficiary/victim structure, different failure modes. Linked here per the epsilon-invariance decomposition rule; the disagreement is located in whether daily spatial practice still enforces the line, an empirical question the 1933-2011 siting record adjudicates village by village.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
