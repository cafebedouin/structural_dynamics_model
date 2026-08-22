% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone Marker as Live Behavioral Norm (Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the behavioral-competence reading of the
 *   tsunami-stone kernel: the stone inscription is treated as having retained
 *   genuine, active behavioral force across generations through a living
 *   transmission chain (retelling, ritual visitation, informal enforcement by
 *   elders and descendant households), such that when the 2011 tsunami
 *   arrived, the encoded rule still functioned as intended in villages where
 *   transmission stayed intact. This is one of three declared readings of the
 *   same kernel; the sibling commemorative_husk_reading holds instead that
 *   the marker had decayed to a symbolic artifact by the time of the
 *   disaster, with any compliance coincidental. A third sibling,
 *   catastrophe_validation_axis, treats the 2011 event itself as the decisive
 *   empirical test rather than characterizing the marker's ongoing behavioral
 *   status. This story's ε is authored strictly for the arrangement AS THIS
 *   READING SEES IT — a low-extraction, functioning commitment device — not
 *   averaged against or hedged toward the husk reading's account.
 *
 * KEY AGENTS:
 *   - coastal_village_residents: primary beneficiaries and bearers of the norm (moderate/constrained) — comply with and are protected by the rule
 *   - descendant_households and local_elders_and_norm_transmitters: administer transmission and informal enforcement (moderate/constrained) — no formal office, authority rests on the story's perceived continued truth
 *   - younger_residents_and_migrants: bear the residual cost of restricted low-elevation land use (powerless/constrained) — mildest friction point in the system
 *   - local_government_and_planners and disaster_researchers: analytical/institutional observers who study or formalize the marker's line after the fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.18).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Marker as Live Behavioral Norm (Competence Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '0f4b6615-d976-4e06-8823-4cf73fa84ad4').
narrative_ontology:cs_kernel_codification('0f4b6615-d976-4e06-8823-4cf73fa84ad4', fixed_text).
narrative_ontology:cs_authority_grounding('0f4b6615-d976-4e06-8823-4cf73fa84ad4', practice).
narrative_ontology:cs_interpretation_layer_present('0f4b6615-d976-4e06-8823-4cf73fa84ad4').
narrative_ontology:cs_reading_relation('0f4b6615-d976-4e06-8823-4cf73fa84ad4', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_reading_relation('0f4b6615-d976-4e06-8823-4cf73fa84ad4', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('0f4b6615-d976-4e06-8823-4cf73fa84ad4', foundational, transmission_chain_remained_behaviorally_active).
narrative_ontology:cs_axiom_status(transmission_chain_remained_behaviorally_active, holdable).
narrative_ontology:cs_axiom_grounding('0f4b6615-d976-4e06-8823-4cf73fa84ad4', transmission_chain_remained_behaviorally_active, empirically_contingent).
narrative_ontology:cs_axiom('0f4b6615-d976-4e06-8823-4cf73fa84ad4', secondary, practice_based_enforcement_suffices_without_formal_sanction).
narrative_ontology:cs_axiom_status(practice_based_enforcement_suffices_without_formal_sanction, holdable).
narrative_ontology:cs_axiom_grounding('0f4b6615-d976-4e06-8823-4cf73fa84ad4', practice_based_enforcement_suffices_without_formal_sanction, conventional).
narrative_ontology:cs_reference_frame('0f4b6615-d976-4e06-8823-4cf73fa84ad4', ancestral_survivor_testimony_baseline).
narrative_ontology:cs_drift_state('0f4b6615-d976-4e06-8823-4cf73fa84ad4', post_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0f4b6615-d976-4e06-8823-4cf73fa84ad4', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, coastal_village_residents).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, descendant_households).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, local_elders_and_norm_transmitters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, younger_residents_and_migrants).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, local_government_and_planners).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, younger_residents_and_migrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live below or near the inscribed stone marking historic tsunami inundation lines. Instructed from childhood, through repeated household and community retelling, to treat the marker's line as an absolute rule: do not build permanent dwellings below it, and when the ground shakes, move uphill past it without waiting for official warning. In 2011 many in marked villages who evacuated on the old rule survived; some who ignored it or lived below the line did not.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_village_residents, beneficiary,
    moderate, biographical, constrained, local).

% Households descended from stone-placing generations who retell the founding story at set occasions (funerals, New Year gatherings, school visits to the stone) and correct younger relatives who treat the marker as merely historical. They administer the norm informally — no formal office, but real corrective pressure on kin who build too low or delay evacuation.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, descendant_households, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, descendant_households, agenda_setter).

% Village elders and community leaders who maintain the annual or periodic ritual visits to the stone, retell the inscription's warning in local dialect, and socially sanction (through gossip, exclusion from communal decision-making, or direct rebuke) households that disregard the boundary. Their authority rests entirely on the story's continued perceived truth, not on any statutory power.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, local_elders_and_norm_transmitters, agenda_setter,
    moderate, generational, constrained, local).

% Younger residents, returnees, or newcomers who did not grow up with direct transmission of the story face social friction and land-use restriction if they wish to build or invest below the marked line, since the community actively discourages it. They bear the cost of foregone lower-elevation land (often more convenient, closer to harbor and roads) in exchange for a safety norm whose enforcement they did not choose but from which they also benefit if it holds.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, younger_residents_and_migrants, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, younger_residents_and_migrants, beneficiary).

% Municipal planners and disaster-preparedness officials observe the marker's line, sometimes formalize it into zoning after the fact, and cite pre-2011 survival correlations in official reports. They benefit from a low-cost, community-maintained safety mechanism that predates and outperforms some formal infrastructure, without having built or funded it themselves.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, local_government_and_planners, observer,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, local_government_and_planners, beneficiary).

% Anthropologists and disaster-risk scholars study which marked villages evacuated correctly in 2011 versus which treated the stones as decayed folklore, using survival and building-pattern data to adjudicate whether the transmission chain was still functionally live at the moment it mattered.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, disaster_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine multi-generational coordination problem: tsunami recurrence intervals (often 50-100+ years) exceed a single human memory span, so without a durable transmission mechanism each generation would have to rediscover the safe building line and evacuation threshold from scratch, at potentially fatal cost. The stone plus its oral-transmission ritual functions as an intergenerational commitment device that keeps the behavioral rule alive across the memory gap.
% TRANSFER_FUNCTION: Moves compliance behavior (building-height decisions, evacuation timing) from a costly individual-learning basis (each generation learns from its own disaster) to a low-cost inherited basis (learn from ancestors' encoded warning). The only 'cost' transferred is foregone lower-elevation land use and social friction for those who resist the norm; no rents are extracted by any administering party.
% ABSENT_VOICES: Land developers and some younger returnees who would prefer to build closer to the harbor for economic reasons are informally discouraged and rarely voice open opposition, since doing so publicly contests a norm tied to ancestral memory and disaster survival; their preferences show up mainly as quiet non-compliance in villages where transmission has weakened, not as organized dissent.
% DISAPPEARANCE_RATIONALE: If the behavioral transmission (not merely the physical stone) vanished overnight, building patterns would drift back toward the harbor and low-lying convenient land within a generation, evacuation timing on the next major quake would revert to waiting for official warnings, and mortality risk in the next tsunami cycle would rise measurably — this is precisely the contrast documented between villages where the story stayed behaviorally live in 2011 and those where it had decayed to plaque status.
% FOUNDING_PROBLEM: Following a historic tsunami, survivors carved a stone marker with a warning ('do not build below this point,' or equivalent) to prevent future generations, who would not have witnessed the disaster, from resettling the floodplain and repeating the same fatal error.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 field surveys by disaster anthropologists and NHK/academic reporting (outside any village's own self-report) found a measurable correlation between villages where the marker's warning was actively retold and enforced and lower fatality/higher-elevation building rates in that tsunami — corroboration from researchers with no stake in the village's own account of its tradition's efficacy.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored very low (0.06 at interval end) because under this reading no party captures rents from the arrangement's operation — elders and descendant households who administer the norm bear the same tsunami risk as everyone else and gain no material advantage from enforcing it; the entire function is genuine intergenerational risk-coordination. Suppression is modest (0.18, declining slightly over the century) because compliance rests on social pressure and reputational sanction within a small community, not coercive apparatus, and the norm's own credibility (reinforced sharply after 2011 survival correlations) reduces the need for active enforcement over time. Theater ratio is low but drifts upward slightly (0.05 to 0.12) reflecting the ordinary risk that ritual commemoration accretes performative elements even in a functioning system, without yet crossing into husk territory.
 *
 * PERSPECTIVAL GAP:
 *   The elders/descendant-household seat and the younger-resident seat would compute differently in interesting ways: from the elder seat this is unambiguous coordination they maintain for the whole community's benefit; from the younger-resident seat there is a real, if modest, felt cost (restricted land choice, social pressure) even though they endorse the underlying purpose. The engine should register this as low but non-zero extraction concentrated on the least-tenured, least-powerful seat, rather than zero extraction uniformly — which is exactly what the authored victims[]-empty, payer-role-on-younger-residents structure is meant to capture without overstating it into snare territory.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, coastal_village_residents, descendant_households, and local_elders are beneficiaries: the norm's cost to any of them (foregone low-elevation land, ritual-maintenance labor) is minor relative to the survival benefit it purchases, so directionality sits near the beneficiary end for all of them. younger_residents_and_migrants sit closer to payer because they inherit the restriction without having chosen it and sometimes face genuine economic friction (preferred harbor-adjacent land foreclosed), but they remain net beneficiaries if the tsunami risk materializes, so they are authored as payer/beneficiary dual-role rather than pure victim — hence victims[] is empty. No stakeholder is authored as a pure extraction target because this reading's entire claim is that the coordination function is real and the extraction component is negligible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting future generations who lack direct disaster memory from resettling the floodplain) is authored as still LIVE under this reading, not dead-but-persisting — which is precisely what distinguishes this reading from a mandatrophy case. Mandatrophy would require the mandate to have outlived its function while the apparatus persists on inertia or theater; here the 2011 survival correlation is read as direct, contemporary evidence that the founding problem remains exactly as pressing as it was when the stone was carved, and the transmission mechanism is still doing the work it was built to do. If this reading is correct, there is no mandatrophy to resolve — the constraint is a rope/piton at low ε precisely because it still performs its original function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_continuity_evidence,
    'Was the behavioral rule actually continuously and actively transmitted with corrective social enforcement across the multi-generational gap since the stone was carved, or did compliance in 2011 arise coincidentally (e.g., from unrelated modern zoning, memory of more recent smaller events, or general risk-aversion) despite the marker having become largely commemorative?',
    'Oral-history fieldwork distinguishing villages by density and content of retelling practice prior to 2011, cross-referenced against building-pattern surveys (were structures actually kept above the line as a matter of active practice, or did post-hoc storytelling retroactively credit the stone) and interviews with residents about whether they recalled being told the rule as an active behavioral instruction versus as local color.',
    'If transmission was genuinely continuous and enforced, this reading (behavioral_competence, low ε, rope/piton-functioning) is the structurally correct account. If compliance was coincidental and the marker had decayed to symbol, the sibling commemorative_husk_reading is correct instead, and this story''s claimed coordination function would not hold for that village''s actual history.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_continuity_evidence, empirical, 'Whether behavioral transmission was genuinely active versus the marker having decayed to symbolic status prior to 2011.').

omega_variable(
    natural_vs_constructed_norm_ambiguity,
    'Is the marker''s low measured extraction a genuine feature of a naturally efficient, voluntarily-sustained coordination norm, or does the norm rest on identifiable beneficiaries (elder authority, descendant-household social standing) whose interest in maintaining the story''s credibility could bias how continuity and compliance get reported?',
    'Compare independent researcher-collected survival/building data against village elders'' own retrospective accounts of the tradition''s continuity; discrepancy would indicate self-interested narrative maintenance rather than a neutral coordination mechanism.',
    'If elder/descendant-household narrative maintenance is inflating the perceived continuity of transmission beyond what the evidence supports, this reading''s beneficiary declarations understate a mild status-extraction component (elders gain standing from being the story''s custodians) that would push the classification toward tangled_rope rather than pure rope/piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_norm_ambiguity, conceptual, 'Whether the beneficiary declarations reflect neutral coordination or partly self-interested custodial status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tsun_tr_t20, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(tsun_tr_t40, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(tsun_tr_t60, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(tsun_tr_t80, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(tsun_be_t20, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 20, 0.04).
narrative_ontology:measurement(tsun_be_t40, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 40, 0.04).
narrative_ontology:measurement(tsun_be_t60, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(tsun_be_t80, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 80, 0.05).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.06).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t0, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(tsun_su_t20, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(tsun_su_t40, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(tsun_su_t60, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 60, 0.19).
narrative_ontology:measurement(tsun_su_t80, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 80, 0.18).
narrative_ontology:measurement(tsun_su_t100, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__behavioral_competence_reading, 0.08).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the tsunami_stone_commitment kernel. behavioral_competence_reading (this file) claims very low ε under an active, still-functioning transmission mechanism. commemorative_husk_reading claims the marker had decayed to symbolic status with coincidental compliance. catastrophe_validation_axis treats the 2011 event as the decisive empirical test of which of the other two readings is correct, rather than asserting the marker's ongoing behavioral status directly. The three are linked via affects_constraints rather than merged, per the ε-invariance principle: each reading has a distinct, stable ε and beneficiary/victim structure and must not be averaged into a single file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
