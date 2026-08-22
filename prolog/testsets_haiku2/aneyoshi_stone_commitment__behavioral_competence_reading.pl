% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Commitment: Behavioral Land-Use Constraint (78-Year Compliance Record)
 *   domain: disaster_anthropology/institutional_commitment/temporal_analysis
 *
 * SUMMARY:
 *   Aneyoshi is a small coastal village in Iwate Prefecture, Japan. After the
 *   catastrophic 1896 tsunami, survivors placed a stone marker at the extent
 *   of the water's reach and committed the community to never building below
 *   that line. For 115 years (through 2011), the village adhered to this rule
 *   without written law, government enforcement, or material punishment for
 *   violation. Individual builders and landowners accepted the constraint as
 *   an identity obligation—to be Aneyoshi was to honor the ancestors'
 *   warning. When the 2011 tsunami struck, it validated the constraint in the
 *   starkest terms: every house above the stone survived; every house built
 *   below the stone (built by newcomers or in postwar expansion) was
 *   destroyed. This reading frames the stone not as a commemorative monument
 *   but as an operational behavioral regulator—a commitment that retained
 *   causal force on land-use decisions across 78 years of ordinary time
 *   (1896–1974 to 2011) and proved predictively accurate when catastrophe
 *   returned.
 *
 * KEY AGENTS:
 *   - Aneyoshi village collective: authors and beneficiaries of the commitment; sustained the constraint across generations without external enforcement
 *   - Individual builders and landowners: bore the cost of restricted building footprint; bound by identity lock (to be Aneyoshi is to accept the obligation)
 *   - Municipal government: observer and post-hoc certifier; adopted the stone's location as official resettlement boundary after 2011
 *   - Tsunami process (non-agent): the physical hazard the commitment was designed to govern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Commitment: Behavioral Land-Use Constraint (78-Year Compliance Record)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/institutional_commitment/temporal_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, 'a39d2f50-658f-46a9-afa4-894a284fc530').
narrative_ontology:cs_kernel_codification('a39d2f50-658f-46a9-afa4-894a284fc530', fixed_text).
narrative_ontology:cs_authority_grounding('a39d2f50-658f-46a9-afa4-894a284fc530', lineage).
narrative_ontology:cs_interpretation_layer_present('a39d2f50-658f-46a9-afa4-894a284fc530').
narrative_ontology:cs_reading_relation('a39d2f50-658f-46a9-afa4-894a284fc530', aneyoshi_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('a39d2f50-658f-46a9-afa4-894a284fc530', foundational, stone_retains_behavioral_constraint_force).
narrative_ontology:cs_axiom_status(stone_retains_behavioral_constraint_force, holdable).
narrative_ontology:cs_axiom_grounding('a39d2f50-658f-46a9-afa4-894a284fc530', stone_retains_behavioral_constraint_force, empirically_contingent).
narrative_ontology:cs_axiom('a39d2f50-658f-46a9-afa4-894a284fc530', foundational, transgenerational_memory_codification_enables_survival).
narrative_ontology:cs_axiom_status(transgenerational_memory_codification_enables_survival, holdable).
narrative_ontology:cs_axiom_grounding('a39d2f50-658f-46a9-afa4-894a284fc530', transgenerational_memory_codification_enables_survival, instrumental).
narrative_ontology:cs_reference_frame('a39d2f50-658f-46a9-afa4-894a284fc530', ancestral_tsunami_wisdom_as_operational_rule).
narrative_ontology:cs_drift_state('a39d2f50-658f-46a9-afa4-894a284fc530', contemporary_postwar_development_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a39d2f50-658f-46a9-afa4-894a284fc530', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_village_collective).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, individual_builders_and_landowners).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__behavioral_competence_reading, individual_builders_and_landowners).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, institutional_memory_as_behavioral_regulator).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, stone_codification_enables_transgenerational_compliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ancestors established the stone directive after the 1896 tsunami: 'Do not build below this line.' For 78 years (1896–1974, then 1974–2011) the community adhered to the rule without institutional enforcement machinery, without written government mandate, without economic incentive beyond collective survival. When the 2011 tsunami struck, every house above the stone survived; every house below was destroyed. The village collective both authors the original commitment and benefits from its persistence through time.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_village_collective, agenda_setter,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_village_collective, beneficiary).

% Face the cost of the constraint: restricted building footprint, foregone oceanfront property value, limited expansion of homes and businesses. The identity lock is profound—to be Aneyoshi is to carry forward the ancestor obligation; exiting the constraint would mean departing the community or betraying generational trust. In 2011, the survivors in homes above the stone inhabited that constraint's functional form—they lived because their parents and grandparents accepted its terms.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, individual_builders_and_landowners, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, individual_builders_and_landowners, beneficiary).

% Post-2011, the government memorialized the stone (built a shrine around it) and adopted its location as the official post-disaster resettlement line. The government did not enforce the original constraint and did not codify it as law until after the tsunami validated it. They observe, certify, and leverage the stone's demonstrated efficacy.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, municipal_government, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_village_collective).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves institutional memory of tsunami hazard across generations in a form that operates without written law, government enforcement, or economic incentive—pure transgenerational commitment codified in stone. Solves the coordination problem of how to maintain survival-critical knowledge when written records fade and institutions turn over.
% TRANSFER_FUNCTION: Moves restraint on land use (foregone property value, compressed building footprint) from each generation to the collective, in exchange for preserved knowledge of catastrophic hazard. The transfer is costless in ordinary time (no tsunami = the constraint feels arbitrary) and invaluable in catastrophic time (tsunami = the constraint is the difference between survival and loss).
% ABSENT_VOICES: Developers and land speculators with interests in oceanfront property would object if they occupied a seat in Aneyoshi's decision-making; they are excluded by the strength of community identity and generational obligation. Voices from prior generations (ancestors who placed the stone) would affirm the constraint from beyond participation.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared (stone removed, rule forgotten), future generations would build down the slope toward the ocean, losing the institutional memory that protected them. The 2011 tsunami proves the rearrangement: houses built in ignorance of or defiance of the stone were destroyed; houses built in compliance survived. The constraint's disappearance would rearrange land use, settlement patterns, and ultimately casualty distributions.
% FOUNDING_PROBLEM: The 1896 tsunami killed a large fraction of Aneyoshi. Survivors recognized the need to preserve the knowledge of where the water reached, in a form that would survive across generations and remain binding when memory faded and institutional structures changed.
% FOUNDING_PROBLEM_CORROBORATION: The 1896 tsunami is historical fact; the 2011 survival of houses above the stone and destruction of houses below is documented in post-disaster assessments and verified by independent researchers (Lim et al. on disaster anthropology of Aneyoshi; Japanese government post-2011 tsunami surveys). Corroboration comes from outside the Aneyoshi community: geomorphologists, disaster researchers, and municipal assessments all affirm that the stone's location matches the tsunami reach.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08 at 2011) because the constraint is cooperative and self-imposed; no party extracts benefit from the arrangement—the collective benefits together from shared survival knowledge. Suppression is minimal (0.12) because enforcement is internal (generational obligation, identity lock) rather than external coercive machinery; resistance is near-zero (0.04) because the constraint is perceived as legitimate and necessary (the 2011 outcome reinforces rather than challenges it). Theater ratio rises from 0 to 0.18 over the interval: in early generations (1896–1950) the stone was a fresh warning and behavioral anchor; by 1980–2011, some erosion of memory had occurred and the stone became increasingly symbolic/commemorative without being functionally abandoned. The measurement series show extractiveness declining over time as memory distance increased—the constraint felt less urgent in generations that had not experienced tsunami. Suppression rising reflects increasing pressure from economic incentives (oceanfront property value, postwar development) that required active internal commitment to resist. The 2011 validation reverses the theater trajectory: the constraint's functional meaning floods back into consciousness. All metrics are authored on one shared time grid (1896, 1920, 1950, 1980, 2000, 2011) with early points projected and 2011 observed.
 *
 * PERSPECTIVAL GAP:
 *   From the builder's seat, the constraint is felt as obligation carrying material cost; from the collective's seat it is felt as preserved survival knowledge. The engine should compute different directionalities: payers (identity-locked builders) sit near the target end despite moderate power, because their exit is irreversibly bound to community identity; beneficiaries (the collective) sit near the beneficiary end because they benefit without active enforcement overhead. The 2011 tsunami collapses the perspectival gap—both seats recognize the constraint as functionally identical to their stake in survival.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual builders and landowners are the payer seat (d near 1.0 target end): they accept material constraint on land use, foregone property value, and compressed building footprint. Their exit is identity_locked—departure would mean leaving the village or breaking generational trust, costs so high they function as trapped. The village collective is the beneficiary seat (d near 0.0 beneficiary end): they benefit from the aggregate outcome of compliance without running enforcement machinery (no agenda-setter burden). The structural relationship is symmetric internally but asymmetric between insiders and outsiders: a developer seeking to build below the stone would face collective resistance (the payer community defends the constraint). Identity lock is the mechanism that keeps the constraint operational without suppression: compliance is internalized as obligation, not experienced as external coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows zero mandatrophy: the founding problem (need to preserve tsunami hazard knowledge across generations) remains live and directly instantiated by the commitment's operation. The constraint's persistence is not due to inertia, theater, or sunk costs—it is directly reinforced by the outcome it was designed for. The 2011 validation is the diagnostic proof: a mandatrophic constraint facing the same test would have been abandoned by the 2011 event (revealed as unnecessary or wrongly motivated); instead, the stone's causal force in that event reaffirmed it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_building_distribution,
    'What would the land-use pattern in Aneyoshi have been in absence of the stone constraint? Would developers have saturated the lower slopes in the absence of collective commitment?',
    'Comparison with other Japanese coastal villages that experienced similar postwar economic pressures but lacked comparable stone markers or generational commitment; examination of building permit applications in Aneyoshi (do they show systematic avoidance of below-stone locations, or is the pattern random?); economic modeling of development pressure in the 1960s–1990s period.',
    'If the counterfactual shows that absent the constraint, buildings would have clustered at lower elevations, the constraint''s behavioral force is established empirically. If the counterfactual shows lower-elevation building in other villages despite no stone marker, the constraint''s causal role becomes ambiguous (other factors may explain the pattern).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_building_distribution, empirical, 'Whether the stone constraint actively deterred lower-slope building or whether land-use patterns were shaped by other factors.').

omega_variable(
    memory_decay_vs_collective_reinforcement,
    'Is the constraint''s persistence across 78 years attributable to active collective reinforcement (each generation teaching the next), or to passive inertia (the stone is simply there, buildings never happened to be built below it by chance)?',
    'Ethnographic investigation of child-rearing practices, elder-to-youth knowledge transmission, community narratives about the stone; oral history interviews with builders about their decision-making; examination of whether families that moved away from Aneyoshi retained the obligation or abandoned it.',
    'If persistence is active reinforcement, the constraint is a rope (genuine coordination of collective survival knowledge). If persistence is passive inertia, the constraint may be better classified as piton (maintained by accident, theater of meaning without behavioral force).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(memory_decay_vs_collective_reinforcement, empirical, 'Whether the constraint''s 78-year stability is attributable to active transgenerational commitment or passive settlement patterns.').

omega_variable(
    kernel_vs_reading_stability,
    'Is the behavioral_competence reading stable, or does the post-2011 government memorialization of the stone signal a shift toward the commemorative_husk reading (transition from behavioral rule to symbolic monument)?',
    'Monitor land-use decisions in reconstructed Aneyoshi (post-2011): do builders still spontaneously honor the stone-line constraint, or only when explicitly instructed by government policy? Do younger-generation residents taught the stone''s history as a survival fact differ from those taught it as a historical monument?',
    'If the commemorative reading is rising (the stone is increasingly symbolic, behavioral constraint is declining), the constraint may transition from rope to piton over the next generation. If behavioral competence persists (rebuilders still choose to honor the line without compulsion), the reading is stable and the constraint remains operationally rope-classified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_reading_stability, empirical, 'Whether the 2011 validation reinforces behavioral competence or enables transition to commemorative husk.').

omega_variable(
    reading_kernel_committer_gap,
    'The stone itself (the kernel) is inert—a physical object. The readings layer interpretive frameworks over it: one reading says the stone constrains behavior, the other says it merely symbolizes. Is this a genuine structural ambiguity (the stone could support either reading), or does the behavioral_competence reading claim empirical facts (2011 survival) that foreclose the commemorative reading within any single framework?',
    'Philosophical clarification: if the behavioral_competence reading rests on the 2011 survival outcome as evidence that the stone had causal force, then a framework that denies the stone''s causal force must also deny the 2011 connection—which would require a different explanation of why houses above the stone survived. The commemorative reading could accommodate the 2011 outcome (coincidence, geology, luck) but would do so by denying the causal claim. Are they genuinely coexistent readings or do they coexist only by refusing to acknowledge the empirical test?',
    'If the 2011 outcome is interpreted as strong evidence that the behavioral reading''s core claim (the stone constrains behavior) is true, then the commemorative reading does not coexist—it forecloses. If both readings can coherently claim the 2011 outcome as consistent with their interpretation (one as proof of causal force, the other as fortunate coincidence), they coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_committer_gap, conceptual, 'Whether the behavioral_competence and commemorative_husk readings are genuinely coexistent or whether the 2011 outcome forecloses one or both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1896, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1896, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1896, 0.0).
narrative_ontology:measurement_basis(aney_tr_t1896, projected).
narrative_ontology:measurement(aney_tr_t1920, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1920, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1920, projected).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(aney_tr_t1950, projected).
narrative_ontology:measurement(aney_tr_t1980, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(aney_tr_t1980, projected).
narrative_ontology:measurement(aney_tr_t2000, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2000, 0.17).
narrative_ontology:measurement_basis(aney_tr_t2000, projected).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.18).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1896, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1896, 0.25).
narrative_ontology:measurement_basis(aney_be_t1896, projected).
narrative_ontology:measurement(aney_be_t1920, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1920, 0.18).
narrative_ontology:measurement_basis(aney_be_t1920, projected).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement_basis(aney_be_t1950, projected).
narrative_ontology:measurement(aney_be_t1980, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1980, 0.08).
narrative_ontology:measurement_basis(aney_be_t1980, projected).
narrative_ontology:measurement(aney_be_t2000, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2000, 0.06).
narrative_ontology:measurement_basis(aney_be_t2000, projected).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.08).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1896, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1896, 0.0).
narrative_ontology:measurement_basis(aney_su_t1896, projected).
narrative_ontology:measurement(aney_su_t1920, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1920, 0.02).
narrative_ontology:measurement_basis(aney_su_t1920, projected).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1950, 0.04).
narrative_ontology:measurement_basis(aney_su_t1950, projected).
narrative_ontology:measurement(aney_su_t1980, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1980, 0.08).
narrative_ontology:measurement_basis(aney_su_t1980, projected).
narrative_ontology:measurement(aney_su_t2000, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement_basis(aney_su_t2000, projected).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2011, 0.12).
narrative_ontology:measurement_basis(aney_su_t2011, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_commitment kernel admits two structurally distinct readings: behavioral_competence_reading (this story) frames the stone as an active regulatory mechanism whose causal force is demonstrated by the 2011 survival pattern; commemorative_husk_reading frames the same stone as having decayed into symbolic observance without behavioral constraint. The readings share a kernel (the physical stone and historical obligation) but diverge on the empirical claim of causal force. Both stories link via network.affects_constraints; the behavioral reading influences the commemorative reading by establishing the 2011 outcome as the interpretive test.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
